
# TODO:
# * (issue) : Fix problematic choices for some of the variables in the topaz hack.
# * (enhance): More general handling of ice categories (might vary between models).
#   The category limits are hardcoded in as an attribute at this point, but should
#   perhaps be inputed by user in the future.

import os
import glob
import shutil
import calendar
import netCDF4
import numpy as np
import datetime as dt
from collections import OrderedDict as odict

class BryVar(object):
    """
    Class for representing a CICE boundary variable that can
    later be used when writing data to a CICE boundary file.

    Example usage:
        > var = BryVar('aicen', float, ('days', 'ice_types', "xi_t"), {'units': '1'})
        > print(var)
    """
    def __init__(self, name, dtype, dims, attrs):
        """
        Constructor setting metadata that represents the boundary variable.

        Args:
            name (str) : Name of the variable
            dtype (type) : Datatype for data elements of the variable
            dims (tuple) : Tuple of strings specifying the variables
                           dimensions (and the order of the dimensions)
            attrs (dict) : Dict with <name: val> for descriptive attributes
        """
        self.name = name
        self.dtype = dtype
        self.dims = dims
        self.attrs = attrs

    def __str__(self):
        """String representation of the object (called upon print(self))."""
        string = "\nname = {}\ndtype = {}\ndims = {}".format(self.name, self.dtype, self.dims)
        string += "\nAttributes:"

        for key, val in self.attrs.items():
            string += "\n\t{} = {}".format(key, val)

        return string

class CiceBry(object):
    """
    Class for generating a boundary condition file for CICE for use with Pedro Duarte's boundary
    method. Intended for writing a new boundary file (its main purpose) or for viewing/verifying
    an existing boundary file. This class itself, though, is meant to be an abstract superclass
    and you should use/write a subclass that defines more specifically what boundary data should
    be written to the boundary file. A subclass should implement a write_bry_data() method that
    writes data to the boundary file for all the variables specified in the load_bry_vars() method
    of this class, in addition to other methods if appropriate.
    """

    def __init__(self, bry_file, mode="w", fmt="NETCDF3_CLASSIC", overwrite=True):
        """
        Constructor setting attributes and loading in boundary variables (metadata).
        Opens the supplied boundary file for either reading or writing.

        Args:
            bry_file (str)   : Filename of CICE boundary file (either exisiting or not)
            mode (str)       : Use "r" for reading an existing boundary file or use "w"
                               for setup object for writing a new boundary file.
            fmt (str)        : Netcdf format for output boundary file (if mode="w")
            overwrite (bool) : Whether or not to overwrite existing file (if mode="w")
        """
        # basic attributes for the boundary file
        self.mode = mode
        self.bry_file = bry_file
        self.data_written_to_bry = False
        self.dims = odict([("nkice", None), ("days", None), ("ice_types", None), ("eta_t", None), ("xi_t", None)])
        self.bry_order = ["E", "N", "S", "W"]
        self.cat_lims = [0, 0.6445072, 1.391433, 2.470179, 4.567288, 1e+08]  # upper limits of ice categories
        self.bry_vars = self.load_bry_vars()

        # start/end date of time dimension in bry file, can be set in self.set_dates()
        self.start_date = None
        self.end_date = None
        self.time_units = None

        # open the boundary file according to user preferences
        if mode == "r":
            self.bry = netCDF4.Dataset(bry_file, mode="r")

        elif mode == "w":
            if os.path.exists(bry_file) and not overwrite:
                raise IOError("File {} exists! Change <bry_file>, <mode> or <overwrite>".format(bry_file))

            self.bry = netCDF4.Dataset(bry_file, mode="w", format=fmt)

        else:
            raise ValueError("Invalid value {} for keyword 'mode'".format(mode))

    def set_dates(self, start_date, end_date, units):
        """
        Method that sets the start and end dates for the time range that the
        boundary data should cover. This should correspond with the time dim size.

        Args:
            start_date (datetime) : First day of boundary data
            end_date (datetime)   : Last day of boundary data
        """
        self.start_date = start_date
        self.end_date = end_date
        self.time_units = units

    def set_dims(self, **dim_sizes):
        """
        Method for specifying the sizes of the dimensions to be written to the boundary file. Should
        correspond with the shapes of the data arrays that are potentailly written to file later.

        Args:
            auto_time (bool) : Whether or not to automatically get time dim size from start/end dates
            dim_sizes (dict) : Dict with <dim_name: dim_size>
        """
        for name in dim_sizes.keys():
            if name not in self.dims.keys():
                raise ValueError("Invalid dimension {}!".format(name))

        for name in self.dims:
            if name in dim_sizes.keys():
                if name == "days" and dim_sizes[name] is not None:
                    raise ValueError("Size of dim 'days' must be UNLIMITED, i.e. self.dims['days']=None!")

                elif name != "days" and dim_sizes[name] is None:
                    raise ValueError("Only dim 'days' can be UNLIMITED, i.e. None")

                elif type(dim_sizes[name]) not in [int, np.int, np.int8, np.int16, np.int32, np.int64]:
                    raise TypeError("Invalid type {} for dim {}! Must be int-like.".format(type(dim_sizes[name]), name))

                self.dims[name] = dim_sizes[name]

    def set_var_attr(self, var_name, attr_name, attr_val, update_file=False):
        """
        Method that sets (or updates) an attribute of a BryVar object (in the
        self.bry_vars list) and also updates it in the boundary file if wanted.

        Args:
            var_name (str) : Corresponding to the BryVar.name attribute
            attr_name (str) : Name of attribute to set/update
            attr_val (str) : New value to set/update with
            update_file (bool) : Whether or not to also update the attr in file
        """
        idx = [v.name for v in self.bry_vars].index(var_name)
        self.bry_vars[idx].attrs[attr_name] = attr_val

        if update_file:
            self.write_var_attrs()

    def get_time_array(self, nc_file, time_name, return_type=dt.datetime):
        """
        Method that gets the time array from the specified netcdf file and
        returns the contents in a desired datatype.

        Args:
            nc_file (Dataset)  : Open netCDF4.Dataset fro reading
            time_name (str)    : Name of time variable in netcdf file
            return_type (type) : Type of each element in the returned array
        Returns:
            time (ndarray) : 1D-array with time values in teh specified return_type
        """
        time_var = nc_file.variables[time_name]

        if return_type is float:
            return time_var[:]
        elif return_type is dt.datetime:
            return netCDF4.num2date(time_var[:], time_var.units)
        else:
            raise TypeError("Invalid type {} for keyword return_type!".format(return_type))

    def get_time_endpoints(self, nc_file, time_name):
        """
        Method that gets the indices corresponding to the start and end
        date attributes from the time array in the specified netcdf file.

        Args:
            nc_file (Dataset) : Open netCDF4.Dataset with a time dimension
            time_name (str)   : Name of time variable in climatology file
        Returns:
            start_idx (int) : Index of self.start_date in time array
            end_idx (int)   : Index of self.end_date in time array
        """
        if any(True for d in [self.start_date, self.end_date] if d is None):
            raise ValueError("Both self.start_date and self.end_date must be non-None!")

        time = self.get_time_array(nc_file, time_name)
        start_idx = np.where(time == self.start_date)[0]
        end_idx = np.where(time == self.end_date)[0]

        if len(start_idx) == 0:
            raise ValueError("Start date {} not in {}!".format(self.start_date, nc_file.filepath()))

        if len(end_idx) == 0:
            raise ValueError("End date {} not in {}!".format(self.end_date, nc_file.filepath()))

        return start_idx[0], end_idx[0]

    def compute_bry_time(self, nc_file, time_name):
        """
        Method that computes the time array for the boundary file using supplied
        input time data together with the self.time_units attribute.

        Args:
            nc_file (Dataset) : netcDF4.Dataset with a time variable
            time_name (str)   : Name of the time variable in file
        Returns:
            bry_time (ndarray) : 1D-array with time data for boundary file
        """
        input_time = self.get_time_array(nc_file, time_name, return_type=dt.datetime)
        start_idx, end_idx = self.get_time_endpoints(nc_file, time_name)
        bry_time = netCDF4.date2num(input_time, self.time_units)[start_idx:end_idx+1]
        return bry_time

    def load_bry_vars(self):
        """Method that generates a BryVar object for each variable necessary to specify
        in the boundary file (most variable names are on the form <name>_{W,S,E,N}_bry)."""
        bry_vars = list()
        bry_vars.append(BryVar("Ice_layers", np.int16, ("nkice"),
            odict([("long_name", "vertical ice levels"), ("units", "1")])))
        bry_vars += self.omnibry_var("Sinz_BRY_bry", float, ("days", "ice_types", "nkice", "BRY"),
            odict([("long_name", "vertical salinity profile - BRY boundary")]))
        bry_vars += self.omnibry_var("TLAT_BRY", float, ("BRY",),
            odict([("long_name", "T grid center latitude - BRY boundary"), ("units", "degrees_north")]))
        bry_vars += self.omnibry_var("TLON_BRY", float, ("BRY",),
            odict([("long_name", "T grid center longitude - BRY boundary"), ("units", "degrees_east")]))
        bry_vars.append(BryVar("Time", np.int16, ("days"),
            odict([("long_name", "model time"), ("units", "days since 2008-01-01 00:00:00")])))
        bry_vars += self.omnibry_var("Tinz_BRY_bry", float, ("days", "ice_types", "nkice", "BRY"),
            odict([("long_name", "vertical temperature profile - BRY boundary")]))
        bry_vars += self.omnibry_var("Tsfc_BRY_bry", float, ("days", "ice_types", "BRY"),
            odict([("long_name", "snow/ice surface temperature - BRY boundary")]))
        bry_vars += self.omnibry_var("aicen_BRY_bry", float, ("days", "ice_types", "BRY"),
            odict([("long_name", "ice area (aggregate) - BRY boundary")]))
        bry_vars += self.omnibry_var("alvln_BRY_bry", float, ("days", "ice_types", "BRY"),
            odict([("long_name", "concentration of level ice - BRY boundary"), ("units", "1")]))
        bry_vars += self.omnibry_var("apondn_BRY_bry", float, ("days", "ice_types", "BRY"),
            odict([("long_name", "melt pond fraction - BRY boundary"), ("units", "1")]))
        bry_vars += self.omnibry_var("fbrine_BRY_bry", float, ("days", "ice_types", "BRY"),
            odict([("long_name", "ratio of brine tracer height to ice thickness - BRY boundary"), ("units", "1")]))
        bry_vars += self.omnibry_var("hbrine_BRY_bry", float, ("days", "ice_types", "BRY"),
            odict([("long_name", "brine surface height above sea ice base - BRY boundary"), ("units", "m")]))
        bry_vars += self.omnibry_var("hpondn_BRY_bry", float, ("days", "ice_types", "BRY"),
            odict([("long_name", "mean melt pond depth - BRY boundary"), ("units", "m")]))
        bry_vars += self.omnibry_var("iage_BRY_bry", float, ("days", "BRY"),
            odict([("long_name", "ice age - BRY boundary")]))
        bry_vars += self.omnibry_var("ipondn_BRY_bry", float, ("days", "ice_types", "BRY"),
            odict([("long_name", "mean melt pond ice thickness - BRY boundary"), ("units", "m")]))
        bry_vars += self.omnibry_var("vicen_BRY_bry", float, ("days", "ice_types", "BRY"),
            odict([("long_name", "grid cell mean ice thickness - BRY boundary"), ("units", "m")]))
        bry_vars += self.omnibry_var("vlvln_BRY_bry", float, ("days", "ice_types", "BRY"),
            odict([("long_name", "volume per unit of area of level ice - BRY boundary"), ("units", "m")]))
        bry_vars += self.omnibry_var("vsnon_BRY_bry", float, ("days", "ice_types", "BRY"),
            odict([("long_name", "grid cell mean snow thickness - BRY boundary"), ("units", "m")]))

        # bry_vars.append(BryVar("Ice_layers", np.int16, ("nkice"),
        #     odict([("long_name", "vertical ice levels"), ("units", "1")])))
        # bry_vars += self.omnibry_var("Sinz_BRY_bry", float, ("days", "ice_types", "nkice", "BRY"),
        #     odict([("long_name", "vertical salinity profile - BRY boundary")]))
        # bry_vars += self.omnibry_var("TLAT_BRY", float, ("BRY",),
        #     odict([("long_name", "T grid center latitude - BRY boundary"), ("units", "degrees_north")]))
        # bry_vars += self.omnibry_var("TLON_BRY", float, ("BRY",),
        #     odict([("long_name", "T grid center longitude - BRY boundary"), ("units", "degrees_east")]))
        # bry_vars.append(BryVar("Time", np.int16, ("days"),
        #     odict([("long_name", "model time"), ("units", "days since 2008-01-01 00:00:00")])))
        # bry_vars += self.omnibry_var("Tinz_BRY_bry", float, ("days", "ice_types", "nkice", "BRY"),
        #     odict([("long_name", "vertical temperature profile - BRY boundary")]))
        # bry_vars += self.omnibry_var("Tsfc_BRY_bry", float, ("days", "ice_types", "BRY"),
        #     odict([("long_name", "snow/ice surface temperature - BRY boundary")]))
        # bry_vars += self.omnibry_var("aicen_BRY_bry", float, ("days", "ice_types", "BRY"),
        #     odict([("long_name", "ice area (aggregate) - BRY boundary")]))
        # bry_vars += self.omnibry_var("alvln_BRY_bry", float, ("days", "ice_types", "BRY"))
        # bry_vars += self.omnibry_var("apondn_BRY_bry", float, ("days", "ice_types", "BRY"))
        # bry_vars += self.omnibry_var("fbrine_BRY_bry", float, ("days", "ice_types", "BRY"))
        # bry_vars += self.omnibry_var("hbrine_BRY_bry", float, ("days", "ice_types", "BRY"))
        # bry_vars += self.omnibry_var("hpondn_BRY_bry", float, ("days", "ice_types", "BRY"))
        # bry_vars += self.omnibry_var("iage_BRY_bry", float, ("days", "BRY"))
        # bry_vars += self.omnibry_var("ipondn_BRY_bry", float, ("days", "ice_types", "BRY"))
        # bry_vars += self.omnibry_var("vicen_BRY_bry", float, ("days", "ice_types", "BRY"),
        #     odict([("long_name", "grid cell mean ice thickness - BRY boundary")]))
        # bry_vars += self.omnibry_var("vlvln_BRY_bry", float, ("days", "ice_types", "BRY"))
        # bry_vars += self.omnibry_var("vsnon_BRY_bry", float, ("days", "ice_types", "BRY"),
        #     odict([("long_name", "grid cell mean snow thickness - BRY boundary")]))

        return bry_vars

    def omnibry_var(self, name, dtype, dims, attrs=dict(), bry_replace="BRY"):
        """
        Method that creates a BryVar object for each boundary wall (W, S, E, N)
        based on the supplied input BryVar template input parameters.

        Args:
            name (str)        : Template for name of variable (replace bry_replace)
            dtype (type)      : Datatype of variable
            dims (tuple)      : Tuple of strings for dims of variable (replace bry_replace)
            attrs (dict)      : Dict of <str: str> (variable attributes) (replace bry_replace)
            bry_replace (str) : String to replace in <name>, <dims>, <attrs>
        Returns:
            bry_vars (list) : List of 4 BryVar objects (one for each boundary)
        """
        bry_vars = list()
        bry_to_dim = {"W": "eta_t", "S": "xi_t", "E": "eta_t", "N": "xi_t"}
        bry_to_dir = {"W": "Western", "S": "Southern", "E": "Eastern", "N": "Northern"}

        for bry in self.bry_order:
            actual_attrs = attrs.copy()

            for key, val in attrs.items():
                actual_attrs[key] = val.replace(bry_replace, bry_to_dir[bry])

            actual_name = name.replace(bry_replace, bry)
            actual_dims = tuple([d.replace(bry_replace, bry_to_dim[bry]) for d in dims])
            bry_vars.append(BryVar(actual_name, dtype, actual_dims, actual_attrs))

        return bry_vars

    def write_global_attrs(self, attrs=dict()):
        """
        Method writing global attributes to the boundary file. Some attributes are written
        by default, but can be supplied with more (ro to overwrite defaults) in a dictionary.

        Args:
            attr (dict) : Dict <name: val> (empty dict by default)
        """
        setattr(self.bry, "title", "Boundary condition file for CICE for use in METROMS")

        for attr_name, attr_val in attrs.items():
            setattr(self.bry, attr_name, attr_val)

    def write_dims(self):
        """Method writing dimensions to boundary file using self.dims."""
        for name, size in self.dims.items():
            self.bry.createDimension(name, size)

    def write_vars(self):
        """Method writing variables (metadata only) based on the boundary variables created
        earlier to the boundary file."""
        for var in self.bry_vars:
            self.bry.createVariable(var.name, var.dtype, var.dims)

    def write_var_attrs(self):
        """Method that writes variable attributes (from the BryVar objects in self.bry_vars)
        to variable attributes in the boundary file."""
        for var in self.bry_vars:
            for attr_name, attr_val in var.attrs.items():
                setattr(self.bry.variables[var.name], attr_name, attr_val)

    def write_bry_data(self):
        """Method not implemented for abtstract superclass."""
        raise NotImplementedError("This method should be overwritten by a subclass!")

    def generate_bry_file(self, start_date, end_date, time_units, auto_time=True, **dim_sizes):
        """Method that is a wrapper for all the required steps to generate a boundary file. Calls all
        necessary methods and relies on <self> being an instance o a subclass ith an write_bry_data method."""
        self.set_dates(start_date, end_date, time_units)
        self.set_dims(**dim_sizes)
        self.write_global_attrs()
        self.write_dims()
        self.write_vars()
        self.write_var_attrs()
        self.write_bry_data()  # has to be implemented in a subclass

    def expand_to_yearly_files(self):
        """
        Method that takes the witten boundary file that contains a time dimension spanning
        only the relevant dates (for which data is computed) and expands the file into yearly
        files with time entries for the entire year. If the original written boundary data has
        a time dimension spanning across one or more year transistions, the data will be written
        to one file per year, e.g. if start date is 20191225 and end date is 20190105, you will
        get two files, BRY_2018.nc and BRY_2019.nc. (method must be called after self.write_bry_data())

        TODO: (IMPORTANT) Needs to be updated to account for year-to-year transition. In that case,
        have this method generate one 365/366 file for each year contained in the bry date range.
        Make it general so it works for both forecasting and hindcasts.

        Args:
            filename (str) : If supplied, copy written bry file to this filename and
                             expand into yearly files. Defaults to existing bry file.
        """

        if not self.data_written_to_bry:
            raise RuntimeError("Must have called self.write_bry_data() before epxanding to yearly files!")

        print("Expanding {} into a yearly boundary file...".format(self.bry_file))
        self.close()  # close so can be opened as "r+" below
        ds = netCDF4.Dataset(self.bry_file, mode="r+")

        days_in_year = 366 if calendar.isleap(self.start_date.year) else 365
        start_date_idx = (self.start_date - dt.datetime(self.start_date.year, 1, 1)).days
        end_date_idx = (self.end_date - dt.datetime(self.start_date.year, 1, 1)).days
        fill_pre_range = dict()
        fill_in_range = dict()
        fill_post_range = dict()

        for var_name, var in ds.variables.items():
            if "days" in var.dimensions and var_name != "Time":
                fill_pre_range[var_name] = var[0,:]    # use first time entry as fill before start date
                fill_in_range[var_name] = var[:]  # use all written data for actual date range
                fill_post_range[var_name] = var[-1,:]  # use last time entry to fill after end date

        for n in range(days_in_year):
            current_date = dt.datetime(self.start_date.year, 1, 1) + dt.timedelta(days=n)
            print("Filling date {:>12},   day #{:>4d}".format(dt.datetime.strftime(current_date, "%Y-%m-%d"), n))
            ds.variables["Time"][n] = n

            for var_name in fill_pre_range.keys():
                if n < start_date_idx:
                    ds.variables[var_name][n,:] = fill_pre_range[var_name][:]  # fill with first time entry before date range

                elif start_date_idx <= n <= end_date_idx:
                    n_offset = n - start_date_idx
                    ds.variables[var_name][n,:] = fill_in_range[var_name][n_offset,:]  # fill with time-varying data in date range

                elif n > end_date_idx:
                    ds.variables[var_name][n,:] = fill_post_range[var_name][:]  # fill with last time entry after date range

        ds.close()

    def verify_variables(self):
        """Method that checks if the boundary file contains all necessary variables."""
        necessary_vars = [var.name for var in self.load_bry_vars()]

        # check if there is a missing variable in the file
        for var in self.bry_vars:
            if var.name not in self.bry.variables.keys():
                raise ValueError("Variable {} not in file {}!".format(var.name, self.bry_file))

        # chekc if theres are variables in the file that should not be there
        for var.name in self.bry.variables.keys():
            if var.name not in necessary_vars:
                raise ValueError("Variable {} is not needed in CICE bry file!".format(var.name))

        print("File {} contains all expected boundary variables!".format(self.bry_file))
        return True

    def ncdump_bry(self, output_file):
        """Method that does a system call to 'ncdump' on the boundary file."""
        os.system("ncdump -h {} > {}".format(self.bry_file, output_file))

    def close(self):
        """Method that closes the bry file."""
        self.bry.close()

    def __del__(self):
        """Destructor closing files if still open."""
        if hasattr(self, "bry") and self.bry.isopen():
            self.bry.close()

class CiceBryTopazCiceHax(CiceBry):
    """(This class is likely to be removed soon) Class that extends CiceBry and implements a
    'hack' for generating boundary data for CICE (for use with Pedro Duarte's boundary method)
    from the rather lacking TOPAZ4 ice field output. The method used here uses the TOPAZ fields
    for certain variables (where it makes sense) and uses supplied CICE restart fields (from the
    same model for which the boundary file is intended for) for the remaining fields.

    Example usage:
        > ..."""
    def __init__(self, bry_file, grid_file, clm_file, atm_file,
                 cice_file, mode="w", fmt="NETCDF3_CLASSIC", overwrite=True):
        """Constructor setting attributes and opening all input files for reading.

        Args:
            bry_file (str)   : Filename of CICE boundary file
            grid_file (str)  : Filename of grid file for your model
            clm_file (str)   : Filename of climatology file
            atm_file (str)   : Filename of atmosphere file
            cice_file (str)  : Filename (or wildcard/list) of data from a forecast from the
                               same model that boundary data is generated for
            mode (str)       : Use "r" for reading an existing boundary file or use "w"
                               for setup object for writing a new boundary file.
            fmt (str)        : Netcdf format for output boundary file (if mode="w")
            overwrite (bool) : Whether or not to overwrite existing file (if mode="w")
        """
        super(CiceBryTopazCiceHax, self).__init__(bry_file, mode=mode, fmt=fmt, overwrite=overwrite)

        self.grid_file = grid_file
        self.grid = netCDF4.Dataset(grid_file, mode="r")

        self.clm_file = clm_file
        self.clm = netCDF4.Dataset(clm_file, mode="r")

        self.atm_file = atm_file
        self.atm = netCDF4.Dataset(atm_file, mode="r")

        if type(cice_file) == list:
            self.cice_files = cice_file
        elif any(True for s in ["*", "?"] if s in cice_file):
            self.cice_files = sorted(glob.glob(cice_file))
        else:
            self.cice_files = [cice_file]

        self.cice = [netCDF4.Dataset(fn, mode="r") for fn in self.cice_files]

        # mapping between bry names and CICE restart file names
        self.cice_names = {"alvln": "alvl", "vlvln": "vlvl", "apondn": "apnd",
                           "hpondn": "apnd", "ipondn": "ipnd"}

    def get_cice_times(self):
        """
        Method that extracts the datetimes of the CICE restart files based on
        filenames. Gives a list of equal length to the number of supplied CICE files.

        Returns:
            cice_times (ndarray) : 1D-array of datetimes for the restart times on the
                                   set CICE restart files.
        """
        cice_times = list()

        for fn in self.cice_files:
            fn = fn.split("/")[-1]  # filename from potential full path
            time_string = fn.split(".")[1]
            date = dt.datetime.strptime(time_string[:10], "%Y-%m-%d")
            seconds = int(time_string.split("-")[-1].lstrip("0") or "0")
            t_delta = dt.timedelta(seconds=seconds)
            cice_times.append(date + t_delta)

        return np.array(cice_times)

    def get_slice(self, slice_type, **kwargs):
        """
        Method that gives a tuple of indexes/slices to be used for indexing the clm
        atm and grid files when extracting data.

        Args:
            slice_type (str) : Determines what local function to call
            kwargs (dict)    : Keyword arguments that are passed to the function
                               specified by <slice_type>.
        Returns:
            slices (tuple) : Tuple of indices/slices (be vary of dimension order)
        """
        def clm_slice_3d(t_idx, bry):
            slices = {"W": (t_idx, slice(None), 0), "S": (t_idx, 0, slice(None)),
                      "E": (t_idx, slice(None), -1), "N": (t_idx, -1, slice(None))}
            return tuple(slices[bry])

        def clm_slice_4d(t_idx, bry, s_idx):
            slices = {"W": (t_idx, s_idx, slice(None), 0), "S": (t_idx, s_idx, 0, slice(None)),
                      "E": (t_idx, s_idx, slice(None), -1), "N": (t_idx, s_idx, -1, slice(None))}
            return tuple(slices[bry])

        def grid_slice_2d(bry):
            slices = {"W": (slice(None), 0), "S": (0, slice(None)),
                      "E": (slice(None), -1), "N": (-1, slice(None))}
            return tuple(slices[bry])

        if slice_type == "clm_3d": return clm_slice_3d(**kwargs)
        elif slice_type == "clm_4d": return clm_slice_4d(**kwargs)
        elif slice_type == "grid_2d": return grid_slice_2d(**kwargs)
        else: raise ValueError("Invalid slice_type {}".format(slice_type))

    def write_bry_data(self):
        """
        Method that implements a hack to use the limited ice data from TOPAZ5 to generate
        data for the CICE boundary file. Relevant available variables from TOPAZ are only
        aice (ice conc.), hice (ice thick.) anf hsnow (snow thick.). From these, we generate
        the CICE variables aicen, vicen and vsnon at the boundaries. Ice conc. ice distributed
        amongst the ice categories (according the pre-defined thickness categories (self.cat_lims))
        and so is ice volume and snow volume.

        Additionally, atmosphere (from relevant model at corresponding times) and ocean (from
        clm file) data is used to help specify data for the variablesTsfc, Tinz and Sinz. Tsfc
        is assumed to be equal to the atmosphere surface temp. Tinz is linearly interpolated
        from the ocean surface temp to the air temperature (if there is less than 1cm of snow
        (at the particular grid cell in question)) and to ocean_temp+0.2*|ocean_temp - atm_temp|
        (if there is more than 1cm of snow). Sinz is linearly interpolated from the ocean surface
        salinity to 0.2 of the ocean surface salinity.

        Time-independent variables Ice_layers, TLON and TLAT are also as well as Time itself are
        also set here. All other time-dependent variables that are needed in the boundary file
        (see self.load_bry_vars()) are set to zero everywhere because of a lack of a better choice.
        """
        if self.grid is None:
            raise AttributeError("No grid file has been set! Use self.set_grid().")
        if self.clm is None:
            raise AttributeError("No clm file has been set! Use self.set_clm().")
        if self.atm is None:
            raise AttributeError("No atm file has been set! Use self.set_atm().")

        # get time arrays needed for loop below
        super(CiceBryTopazCiceHax, self).set_var_attr("Time", "units", self.time_units, update_file=True)           # set new time units for Time var
        bry_time = super(CiceBryTopazCiceHax, self).compute_bry_time(self.clm, "clim_time")                         # float array in bry file
        clm_dates = super(CiceBryTopazCiceHax, self).get_time_array(self.clm, "clim_time", return_type=dt.datetime) # date array in clm file
        atm_dates = super(CiceBryTopazCiceHax, self).get_time_array(self.atm, "time")                               # date array in atm file
        cice_dates = self.get_cice_times()                                                                 # date array for cice files
        t0c_idx, t1c_idx = super(CiceBryTopazCiceHax, self).get_time_endpoints(self.clm, "clim_time")               # start and end indices in clm file

        # extract pointers for all relevant variables from climatology and atmosphere files
        aice = self.clm.variables["aice"]            # clm ice concentration
        hice = self.clm.variables["hice"]            # clm ice thickness
        hsno = self.clm.variables["snow_thick"]      # clm snow thickness (seems to be zero everywhere all the time(?))
        tair = self.atm.variables["Tair"]            # atm surface temperature
        toce = self.clm.variables["temp"]            # ocean surface temperature
        salt = self.clm.variables["salt"]            # ocean surface salinity
        land_mask = self.grid.variables["mask_rho"]  # grid file land mask

        # variables that, due to lack of better data, is set to zero everywhere
        zero_vars_2d = ["iage_BRY_bry"]
        zero_vars_3d = ["fbrine_BRY_bry", "hbrine_BRY_bry"]
        cice_vars_3d = ["apondn_BRY_bry", "hpondn_BRY_bry", "ipondn_BRY_bry", "alvln_BRY_bry", "vlvln_BRY_bry"]

        # write time-independent data to boundary file
        self.bry.variables["Ice_layers"][:] = list(range(self.dims["nkice"]))  # vertical dim variable
        for bry in self.bry_order:
            grid_slice = self.get_slice("grid_2d", bry=bry)
            self.bry.variables["TLON_{}".format(bry)][:] = self.grid.variables["lon_rho"][grid_slice]  # longitude
            self.bry.variables["TLAT_{}".format(bry)][:] = self.grid.variables["lat_rho"][grid_slice]  # latitude

        print("Computing boundary data for:")
        # handle one time index at a time for memory efficiency and write boundary data for all times,
        # all ice categories and all four boundaries
        for clm_tidx in range(t0c_idx, t1c_idx + 1):
            bry_tidx = clm_tidx - t0c_idx                                    # time index for bry file starts at 0
            atm_tidx = (np.abs(atm_dates - clm_dates[clm_tidx])).argmin()    # closest time atm index for corresponding days
            cice_tidx = (np.abs(cice_dates - clm_dates[clm_tidx])).argmin()  # closest time cice index for corresponding days
            self.bry.variables["Time"][bry_tidx] = bry_time[bry_tidx]      # fill time array with current time
            print("\tbry time: {},\n\tclm time: {},\n\tatm time: {},\n\tcice time: {}".format(netCDF4.num2date(bry_time[bry_tidx],
                  self.time_units), clm_dates[clm_tidx], atm_dates[atm_tidx], cice_dates[cice_tidx]))

            # loop over all 4 boundaries (W, S, E, N)
            for bry in self.bry_order:
                print("\t\tBoundary {}".format(bry))
                # slices for 3d variables aice, hice and Tsfc
                cslice = self.get_slice("clm_3d", t_idx=clm_tidx, bry=bry)  # slices for clim variables
                aslice = self.get_slice("clm_3d", t_idx=atm_tidx, bry=bry)  # slices for atm variables

                # loop over all ice categories and fill from clm and atm data
                for n in range(len(self.cat_lims[1:])):
                    print("\t\t\tCategory {}".format(n))
                    bslice_2d = (bry_tidx, slice(None))         # slices for 2D bry variables (time, xi/eta)
                    bslice_3d = (bry_tidx, n, slice(None))      # slices for 3D bry variables (time, cat, xi/eta)
                    cice_slice = tuple([n] + list(cslice)[1:])  # slice for cice forecast data

                    # copy from model forecast for variables the clm file has no useful data for
                    for var in cice_vars_3d:
                        bry_name = var.replace("BRY", bry)
                        cice_name = self.cice_names[var.split("_")[0]]
                        self.bry.variables[bry_name][bslice_3d] = self.cice[cice_tidx].variables[cice_name][cice_slice]

                    # masking out all other ice categories except category n
                    cat_mask = np.logical_and(hice[cslice] > self.cat_lims[n], hice[cslice] < self.cat_lims[n+1])

                    # handle ice concentration, ice volume and snow volume (distribute over the categories)
                    self.bry.variables["aicen_{}_bry".format(bry)][bslice_3d] = np.where(cat_mask,
                        aice[cslice], 0.0)*land_mask[cslice[1:]]                                 # set to clm ice conc.
                    self.bry.variables["vicen_{}_bry".format(bry)][bslice_3d] = np.where(cat_mask,
                        hice[cslice]*aice[cslice], 0.0)*land_mask[cslice[1:]]                    # set to clm ice vol. x conc.
                    self.bry.variables["vsnon_{}_bry".format(bry)][bslice_3d] = np.where(cat_mask,
                        hsno[cslice], 0.0)*land_mask[cslice[1:]]                                 # set to clm snow thick
                    self.bry.variables["Tsfc_{}_bry".format(bry)][bslice_3d] = np.where(cat_mask,
                        tair[aslice], 0.0)*land_mask[cslice[1:]]                                 # set to surface temp from atm

                    # copy vertical salinity profile (4d var) from CICE forecast
                    for zlvl in range(self.dims["nkice"]):
                        bslice_z = (bry_tidx, n, zlvl, slice(None))
                        cice_name = "sice00{}".format(zlvl+1)        # one sice variable per zlvl
                        bry_name = "Sinz_{}_bry".format(bry)
                        self.bry.variables[bry_name][bslice_z] = self.cice[cice_tidx].variables[cice_name][cice_slice]*land_mask[cslice[1:]]

                    # slices needed for 4d var Tinz
                    cslice_4d = self.get_slice("clm_4d", t_idx=clm_tidx, bry=bry, s_idx=-1)
                    bslice_4d = (bry_tidx, n, slice(None), slice(None))

                    # handle vertical profile of temperature
                    snow_mask = self.bry.variables["vsnon_{}_bry".format(bry)][bslice_3d] > 0.01         # where more than 1cm snow
                    temp_interp_ice = np.linspace(tair[aslice], toce[cslice_4d], self.dims["nkice"])  # linear interp from ocean to atm
                    temp_interp_sno = np.linspace(tair[aslice] + 0.2*np.abs(toce[cslice_4d] -\
                        tair[aslice]), toce[cslice_4d], self.dims["nkice"])                           # linear interp from ocean to ocean+0.2*|ocean-atm|
                    temp_interp = np.where(snow_mask, temp_interp_sno, temp_interp_ice)
                    self.bry.variables["Tinz_{}_bry".format(bry)][bslice_4d] = np.where(cat_mask,
                        temp_interp, -5.0)*land_mask[cslice[1:]]                                      # fill category n with vertical temp profile

                    # handle all variables that should be set to zero
                    for var in zero_vars_2d:
                        self.bry.variables[var.replace("BRY", bry)][bslice_2d] = 0.0

                    for var in zero_vars_3d:
                        self.bry.variables[var.replace("BRY", bry)][bslice_3d] = 0.0

    def __del__(self):
        """Destructor closing files if still open."""
        if getattr(self, "grid", None) is not None and self.grid.isopen():
            self.grid.close()

        if getattr(self, "clm", None) is not None and self.clm.isopen():
            self.clm.close()

        if getattr(self, "atm", None) is not None and self.atm.isopen():
            self.atm.close()

        if getattr(self, "cice", None) is not None:
            for f in self.cice:
                if f.isopen():
                    f.close()

        super(CiceBryTopazCiceHax, self).__del__()  # call super destructor to close boundary file

class CiceBryTopazHax(CiceBry):
    """
    Class that extends CiceBry and implements a 'hack' for generating boundary data for CICE (for
    use with Pedro Duarte's boundary method) from the rather lacking TOPAZ4 ice field output. The
    method used here uses the TOPAZ fields to best ability to guess reasonable values for variables
    required by the CICE boundary method.

    Example usage:
        > grid_file = "grid_file.nc"
        > clm_file = "clm_file.nc"
        > bry_file = "bry_file.nc"
        > dim_sizes = {"ice_types": 5, "nkice": 7, "xi_t": 739, "eta_t": 949}
        > start_date = dt.datetime(2019, 9, 1)
        > end_date = dt.datetime(2019, 10, 2)
        > time_units = "days since 2019-01-01 00:00:00"
        >
        > bry = cice_bry.CiceBryTopazHax(bry_file, grid_file, clm_file)
        > bry.generate_bry_file(start_date, end_date, time_units, **dim_sizes)  # superclass call
        > bry.expand_to_yearly_files()  # superclass call
    """
    def __init__(self, bry_file, grid_file, clm_file,
                 mode="w", fmt="NETCDF3_CLASSIC", overwrite=True):
        """
        Constructor setting attributes and opening all input files for reading.

        Args:
            bry_file (str)   : Filename of CICE boundary file
            grid_file (str)  : Filename of grid file for your model
            clm_file (str)   : Filename of climatology file
            atm_file (str)   : Filename of atmosphere file
            cice_file (str)  : Filename (or wildcard/list) of data from a forecast from the
                               same model that boundary data is generated for
            mode (str)       : Use "r" for reading an existing boundary file or use "w"
                               for setup object for writing a new boundary file.
            fmt (str)        : Netcdf format for output boundary file (if mode="w")
            overwrite (bool) : Whether or not to overwrite existing file (if mode="w")
        """
        super(CiceBryTopazHax, self).__init__(bry_file, mode=mode, fmt=fmt, overwrite=overwrite)

        self.grid = netCDF4.Dataset(grid_file, mode="r")
        self.grid_file = grid_file

        self.clm_file = clm_file
        self.clm = netCDF4.Dataset(clm_file, mode="r")

    def get_slice(self, slice_type, **kwargs):
        """
        Method that gives a tuple of indexes/slices to be used for indexing the clm
        atm and grid files when extracting data.

        Args:
            slice_type (str) : Determines what local function to call
            kwargs (dict)    : Keyword arguments that are passed to the function
                               specified by <slice_type>.
        Returns:
            slices (tuple) : Tuple of indices/slices (be vary of dimension order)
        """
        def clm_slice_3d(t_idx, bry):
            slices = {"W": (t_idx, slice(None), 0), "S": (t_idx, 0, slice(None)),
                      "E": (t_idx, slice(None), -1), "N": (t_idx, -1, slice(None))}
            return tuple(slices[bry])

        def clm_slice_4d(t_idx, bry, s_idx):
            slices = {"W": (t_idx, s_idx, slice(None), 0), "S": (t_idx, s_idx, 0, slice(None)),
                      "E": (t_idx, s_idx, slice(None), -1), "N": (t_idx, s_idx, -1, slice(None))}
            return tuple(slices[bry])

        def grid_slice_2d(bry):
            slices = {"W": (slice(None), 0), "S": (0, slice(None)),
                      "E": (slice(None), -1), "N": (-1, slice(None))}
            return tuple(slices[bry])

        if slice_type == "clm_3d": return clm_slice_3d(**kwargs)
        elif slice_type == "clm_4d": return clm_slice_4d(**kwargs)
        elif slice_type == "grid_2d": return grid_slice_2d(**kwargs)
        else: raise ValueError("Invalid slice_type {}".format(slice_type))

    def write_bry_data(self):
        """
        Method that implements a hack to use the limited ice data from TOPAZ5 to generate
        data for the CICE boundary file. Relevant available variables from TOPAZ are only
        aice (ice conc.), hice (ice thick.) anf hsnow (snow thick.). From these, we generate
        the CICE variables aicen, vicen and vsnon at the boundaries by distributing TOPAZ4 data
        amongst the ice categories (according the pre-defined thickness categories (self.cat_lims))
        and so is ice volume and snow volume. Tsfcn is set to -5, alvln and vlvln to 1, Tinz is
        vertically interpolated from -5 at surface to -2 at bottom and Sinz is set to 5 everywhere

        Time-independent variables Ice_layers, TLON and TLAT as well as Time itself are also set
        here. All other time-dependent variables that are needed in the boundary file (see
        self.load_bry_vars()) are set to zero everywhere because of a lack of a better choice.
        """
        if self.grid is None:
            raise AttributeError("No grid file has been set! Use self.set_grid().")
        if self.clm is None:
            raise AttributeError("No clm file has been set! Use self.set_clm().")

        # get time arrays needed for loop below
        super(CiceBryTopazHax, self).set_var_attr("Time", "units", self.time_units, update_file=True)           # set new time units for Time var
        bry_time = super(CiceBryTopazHax, self).compute_bry_time(self.clm, "clim_time")                         # float array in bry file
        clm_dates = super(CiceBryTopazHax, self).get_time_array(self.clm, "clim_time", return_type=dt.datetime) # date array in clm file
        t0c_idx, t1c_idx = super(CiceBryTopazHax, self).get_time_endpoints(self.clm, "clim_time")               # start and end indices in clm file

        # extract pointers for all relevant variables from climatology and atmosphere files
        aice = self.clm.variables["aice"]            # clm ice concentration
        hice = self.clm.variables["hice"]            # clm ice thickness
        hsno = self.clm.variables["snow_thick"]      # clm snow thickness (seems to be zero everywhere all the time(?))
        toce = self.clm.variables["temp"]            # ocean surface temperature
        salt = self.clm.variables["salt"]            # ocean surface salinity
        land_mask = self.grid.variables["mask_rho"]  # grid file land mask

        # variables that, due to lack of better data, is set to zero everywhere
        zero_vars_2d = ["iage_BRY_bry"]
        zero_vars_3d = ["fbrine_BRY_bry", "hbrine_BRY_bry", "apondn_BRY_bry", "hpondn_BRY_bry", "ipondn_BRY_bry"]

        # write time-independent data to boundary file
        self.bry.variables["Ice_layers"][:] = list(range(self.dims["nkice"]))  # vertical dim variable

        for bry in self.bry_order:
            grid_slice = self.get_slice("grid_2d", bry=bry)
            self.bry.variables["TLON_{}".format(bry)][:] = self.grid.variables["lon_rho"][grid_slice]  # longitude
            self.bry.variables["TLAT_{}".format(bry)][:] = self.grid.variables["lat_rho"][grid_slice]  # latitude

        print("Computing and writing boundary data for:")
        # handle one time index at a time for memory efficiency and write boundary data for all times,
        # all ice categories and all four boundaries
        for clm_tidx in range(t0c_idx, t1c_idx + 1):
            bry_tidx = clm_tidx - t0c_idx                                    # time index for bry file starts at 0
            ice_mask = np.where(aice[clm_tidx,:,:]*land_mask[:,:] > 0.0, 1, 0)  # to filter out areas with no ice below
            self.bry.variables["Time"][bry_tidx] = bry_time[bry_tidx]      # fill time array with current time
            print("\tbry time: {},\n\tclm time: {}".format(netCDF4.num2date(bry_time[bry_tidx],
                  self.time_units), clm_dates[clm_tidx]))

            # loop over all 4 boundaries (W, S, E, N)
            for bry in self.bry_order:
                print("\t\tBoundary {}".format(bry))
                # slices for 3d variables aice, hice and Tsfc
                cslice = self.get_slice("clm_3d", t_idx=clm_tidx, bry=bry)  # slices for clim variables
                cslice_4d = self.get_slice("clm_4d", t_idx=clm_tidx, s_idx=-1, bry=bry)  # slices for clim variables

                # loop over all ice categories and fill from clm and atm data
                for n in range(len(self.cat_lims[1:])):
                    print("\t\t\tCategory {}".format(n))
                    bslice_2d = (bry_tidx, slice(None))         # slices for 2D bry variables (time, xi/eta)
                    bslice_3d = (bry_tidx, n, slice(None))      # slices for 3D bry variables (time, cat, xi/eta)

                    # masking out all other ice categories except category n
                    cat_mask = np.logical_and(hice[cslice] > self.cat_lims[n], hice[cslice] <= self.cat_lims[n+1])

                    # adding temperature mask to accomodate for difference between TOPAZ and barents land mask creating
                    # ice where there should not be (caused by fimex). Cna maybe remove this when using nearestneighbor
                    # interpolation in fimex but not tested yet
                    temp_mask = toce[cslice_4d] < 0.0
                    cat_mask = np.logical_and(cat_mask, temp_mask)

                    # remove very sparse and thin ice (to avoid model crashes)
                    aice_mask = aice[cslice] > 0.01
                    hice_mask = hice[cslice] > 0.01
                    hsno_mask = hsno[cslice] > 0.005
                    cat_mask = np.logical_and(cat_mask, aice_mask*hice_mask*hsno_mask)

                    # handle ice concentration, ice volume and snow volume (distribute over the categories)
                    self.bry.variables["aicen_{}_bry".format(bry)][bslice_3d] = np.where(cat_mask,
                        aice[cslice], 0.0)*land_mask[cslice[1:]]                                 # set to clm ice conc.
                    self.bry.variables["vicen_{}_bry".format(bry)][bslice_3d] = np.where(cat_mask,
                        hice[cslice]*aice[cslice], 0.0)*land_mask[cslice[1:]]                    # set to clm ice vol. x conc.
                    self.bry.variables["vsnon_{}_bry".format(bry)][bslice_3d] = np.where(cat_mask,
                        hsno[cslice]*aice[cslice], 0.0)*land_mask[cslice[1:]]                # should get from topaz, but tmp hax
                    self.bry.variables["Tsfc_{}_bry".format(bry)][bslice_3d] = np.where(cat_mask,
                        -5.0, 0.0)*land_mask[cslice[1:]]
                    self.bry.variables["alvln_{}_bry".format(bry)][bslice_3d] = np.where(cat_mask,
                        1.0, 0.0)*land_mask[cslice[1:]]
                    self.bry.variables["vlvln_{}_bry".format(bry)][bslice_3d] = np.where(cat_mask,
                        1.0, 0.0)*land_mask[cslice[1:]]

                    # slices needed for 4d var Tinz and Sinz
                    cslice_4d = self.get_slice("clm_4d", t_idx=clm_tidx, bry=bry, s_idx=-1)
                    bslice_4d = (bry_tidx, n, slice(None), slice(None))

                    # handle vertical profile of temperature and salinity
                    temp_interp_ice = np.linspace(-5.0, -2.0, self.dims["nkice"])

                    tmp_temp = self.bry.variables["Tinz_{}_bry".format(bry)][bry_tidx,n,:,:]
                    tmp_temp[:,:] = temp_interp_ice[:,None]
                    tmp_salt = self.bry.variables["Sinz_{}_bry".format(bry)][bry_tidx,n,:,:]
                    tmp_salt[:,:] = 5.0
                    self.bry.variables["Tinz_{}_bry".format(bry)][bry_tidx,n,:,:] = tmp_temp[:,:]*cat_mask[None,:]*land_mask[cslice[1:]]
                    self.bry.variables["Sinz_{}_bry".format(bry)][bslice_4d] = tmp_salt[:,:]*cat_mask[None,:]*land_mask[cslice[1:]]

                    # handle all variables that should be set to zero
                    for var in zero_vars_2d:
                        self.bry.variables[var.replace("BRY", bry)][bslice_2d] = 0.0

                    for var in zero_vars_3d:
                        self.bry.variables[var.replace("BRY", bry)][bslice_3d] = 0.0

        self.data_written_to_bry = True

    def __del__(self):
        """Destructor closing files if still open."""
        if getattr(self, "grid", None) is not None and self.grid.isopen():
            self.grid.close()

        if getattr(self, "clm", None) is not None and self.clm.isopen():
            self.clm.close()

        if getattr(self, "atm", None) is not None and self.atm.isopen():
            self.atm.close()

        super(CiceBryTopazHax, self).__del__()  # call super destructor to close boundary file

class CiceBryCiceHax(CiceBry):
    """
    Class that extends CiceBry and implements a 'hack' for generating boundary data for CICE (for
    use with Pedro Duarte's boundary method) from the rather lacking TOPAZ5 ice field output. The
    method used here uses the TOPAZ fields for certain variables (where it makes sense) and uses
    supplied CICE restart fields (from the same model for which the boundary file is intended for)
    for the remaining fields.

    Example usage:
        > ...
    """
    def __init__(self, bry_file, grid_file, clm_file, atm_file,
                 cice_file, mode="w", fmt="NETCDF3_CLASSIC", overwrite=True):
        """Constructor setting attributes and opening all input files for reading.

        Args:
            bry_file (str)   : Filename of CICE boundary file
            grid_file (str)  : Filename of grid file for your model
            clm_file (str)   : Filename of climatology file
            atm_file (str)   : Filename of atmosphere file
            cice_file (str)  : Filename (or wildcard/list) of data from a forecast from the
                               same model that boundary data is generated for
            mode (str)       : Use "r" for reading an existing boundary file or use "w"
                               for setup object for writing a new boundary file.
            fmt (str)        : Netcdf format for output boundary file (if mode="w")
            overwrite (bool) : Whether or not to overwrite existing file (if mode="w")
        """
        super(CiceBryCiceHax, self).__init__(bry_file, mode=mode, fmt=fmt, overwrite=overwrite)

        self.grid_file = grid_file
        self.grid = netCDF4.Dataset(grid_file, mode="r")

        self.clm_file = clm_file
        self.clm = netCDF4.Dataset(clm_file, mode="r")

        self.atm_file = atm_file
        self.atm = netCDF4.Dataset(atm_file, mode="r")

        if type(cice_file) == list:
            self.cice_files = cice_file
        elif any(True for s in ["*", "?"] if s in cice_file):
            self.cice_files = sorted(glob.glob(cice_file))
        else:
            self.cice_files = [cice_file]

        self.cice = [netCDF4.Dataset(fn, mode="r") for fn in self.cice_files]

        # mapping between bry names and CICE restart file names
        self.cice_names = {"aicen": "aicen", "vicen": "vicen", "vsnon": "vsnon", "Tsfc": "Tsfcn",
                           "alvln": "alvl", "vlvln": "vlvl", "apondn": "apnd", "hpondn": "apnd", "ipondn": "ipnd"}

    def get_cice_times(self):
        """
        Method that extracts the datetimes of the CICE restart files based on
        filenames. Gives a list of equal length to the number of supplied CICE files.

        Returns:
            cice_times (ndarray) : 1D-array of datetimes for the restart times on the
                                   set CICE restart files.
        """
        cice_times = list()

        for fn in self.cice_files:
            fn = fn.split("/")[-1]  # filename from potential full path
            time_string = fn.split(".")[1]
            date = dt.datetime.strptime(time_string[:10], "%Y-%m-%d")
            seconds = int(time_string.split("-")[-1].lstrip("0") or "0")
            t_delta = dt.timedelta(seconds=seconds)
            cice_times.append(date + t_delta)

        return np.array(cice_times)

    def get_slice(self, slice_type, **kwargs):
        """
        Method that gives a tuple of indexes/slices to be used for indexing the clm
        atm and grid files when extracting data.

        Args:
            slice_type (str) : Determines what local function to call
            kwargs (dict)    : Keyword arguments that are passed to the function
                               specified by <slice_type>.
        Returns:
            slices (tuple) : Tuple of indices/slices (be vary of dimension order)
        """
        def clm_slice_3d(t_idx, bry):
            slices = {"W": (t_idx, slice(None), 0), "S": (t_idx, 0, slice(None)),
                      "E": (t_idx, slice(None), -1), "N": (t_idx, -1, slice(None))}
            return tuple(slices[bry])

        def clm_slice_4d(t_idx, bry, s_idx):
            slices = {"W": (t_idx, s_idx, slice(None), 0), "S": (t_idx, s_idx, 0, slice(None)),
                      "E": (t_idx, s_idx, slice(None), -1), "N": (t_idx, s_idx, -1, slice(None))}
            return tuple(slices[bry])

        def grid_slice_2d(bry):
            slices = {"W": (slice(None), 0), "S": (0, slice(None)),
                      "E": (slice(None), -1), "N": (-1, slice(None))}
            return tuple(slices[bry])

        if slice_type == "clm_3d": return clm_slice_3d(**kwargs)
        elif slice_type == "clm_4d": return clm_slice_4d(**kwargs)
        elif slice_type == "grid_2d": return grid_slice_2d(**kwargs)
        else: raise ValueError("Invalid slice_type {}".format(slice_type))

    def write_bry_data(self):
        """
        Method that implements a hack to use the limited ice data from TOPAZ5 to generate
        data for the CICE boundary file. Relevant available variables from TOPAZ are only
        aice (ice conc.), hice (ice thick.) anf hsnow (snow thick.). From these, we generate
        the CICE variables aicen, vicen and vsnon at the boundaries. Ice conc. ice distributed
        amongst the ice categories (according the pre-defined thickness categories (self.cat_lims))
        and so is ice volume and snow volume.

        Additionally, atmosphere (from relevant model at corresponding times) and ocean (from
        clm file) data is used to help specify data for the variablesTsfc, Tinz and Sinz. Tsfc
        is assumed to be equal to the atmosphere surface temp. Tinz is linearly interpolated
        from the ocean surface temp to the air temperature (if there is less than 1cm of snow
        (at the particular grid cell in question)) and to ocean_temp+0.2*|ocean_temp - atm_temp|
        (if there is more than 1cm of snow). Sinz is linearly interpolated from the ocean surface
        salinity to 0.2 of the ocean surface salinity.

        Time-independent variables Ice_layers, TLON and TLAT are also as well as Time itself are
        also set here. All other time-dependent variables that are needed in the boundary file
        (see self.load_bry_vars()) are set to zero everywhere because of a lack of a better choice.
        """
        if self.grid is None:
            raise AttributeError("No grid file has been set! Use self.set_grid().")
        if self.clm is None:
            raise AttributeError("No clm file has been set! Use self.set_clm().")
        if self.atm is None:
            raise AttributeError("No atm file has been set! Use self.set_atm().")

        # get time arrays needed for loop below
        super(CiceBryCiceHax, self).set_var_attr("Time", "units", self.time_units, update_file=True)           # set new time units for Time var
        bry_time = super(CiceBryCiceHax, self).compute_bry_time(self.clm, "clim_time")                         # float array in bry file
        clm_dates = super(CiceBryCiceHax, self).get_time_array(self.clm, "clim_time", return_type=dt.datetime) # date array in clm file
        atm_dates = super(CiceBryCiceHax, self).get_time_array(self.atm, "time")                               # date array in atm file
        cice_dates = self.get_cice_times()                                                                 # date array for cice files
        t0c_idx, t1c_idx = super(CiceBryCiceHax, self).get_time_endpoints(self.clm, "clim_time")               # start and end indices in clm file

        # extract pointers for all relevant variables from climatology and atmosphere files
        aice = self.clm.variables["aice"]            # clm ice concentration
        hice = self.clm.variables["hice"]            # clm ice thickness
        hsno = self.clm.variables["snow_thick"]      # clm snow thickness (seems to be zero everywhere all the time(?))
        tair = self.atm.variables["Tair"]            # atm surface temperature
        toce = self.clm.variables["temp"]            # ocean surface temperature
        salt = self.clm.variables["salt"]            # ocean surface salinity
        land_mask = self.grid.variables["mask_rho"]  # grid file land mask

        # variables that, due to lack of better data, is set to zero everywhere
        zero_vars_2d = ["iage_BRY_bry"]
        zero_vars_3d = ["fbrine_BRY_bry", "hbrine_BRY_bry"]
        cice_vars_3d = ["aicen_BRY_bry", "vicen_BRY_bry", "vsnon_BRY_bry", "Tsfc_BRY_bry", "apondn_BRY_bry",
                        "hpondn_BRY_bry", "ipondn_BRY_bry", "alvln_BRY_bry", "vlvln_BRY_bry"]

        # write time-independent data to boundary file
        self.bry.variables["Ice_layers"][:] = list(range(self.dims["nkice"]))  # vertical dim variable
        for bry in self.bry_order:
            grid_slice = self.get_slice("grid_2d", bry=bry)
            self.bry.variables["TLON_{}".format(bry)][:] = self.grid.variables["lon_rho"][grid_slice]  # longitude
            self.bry.variables["TLAT_{}".format(bry)][:] = self.grid.variables["lat_rho"][grid_slice]  # latitude

        print("Computing boundary data for:")
        # handle one time index at a time for memory efficiency and write boundary data for all times,
        # all ice categories and all four boundaries
        for clm_tidx in range(t0c_idx, t1c_idx + 1):
            bry_tidx = clm_tidx - t0c_idx                                    # time index for bry file starts at 0
            atm_tidx = (np.abs(atm_dates - clm_dates[clm_tidx])).argmin()    # closest time atm index for corresponding days
            cice_tidx = (np.abs(cice_dates - clm_dates[clm_tidx])).argmin()  # closest time cice index for corresponding days
            self.bry.variables["Time"][bry_tidx] = bry_time[bry_tidx]      # fill time array with current time
            ice_mask = np.where(self.cice[cice_tidx].variables["aicen"][:]*land_mask[:,:] > 0.0, 1, 0)  # to filter out areas with no ice below
            print("\tbry time: {},\n\tclm time: {},\n\tatm time: {},\n\tcice time: {}".format(netCDF4.num2date(bry_time[bry_tidx],
                  self.time_units), clm_dates[clm_tidx], atm_dates[atm_tidx], cice_dates[cice_tidx]))

            # loop over all 4 boundaries (W, S, E, N)
            for bry in self.bry_order:
                print("\t\tBoundary {}".format(bry))
                # slices for 3d variables aice, hice and Tsfc
                cslice = self.get_slice("clm_3d", t_idx=clm_tidx, bry=bry)  # slices for clim variables
                aslice = self.get_slice("clm_3d", t_idx=atm_tidx, bry=bry)  # slices for atm variables

                # loop over all ice categories and fill from clm and atm data
                for n in range(len(self.cat_lims[1:])):
                    print("\t\t\tCategory {}".format(n))
                    bslice_2d = (bry_tidx, slice(None))         # slices for 2D bry variables (time, xi/eta)
                    bslice_3d = (bry_tidx, n, slice(None))      # slices for 3D bry variables (time, cat, xi/eta)
                    cice_slice = tuple([n] + list(cslice)[1:])  # slice for cice forecast data

                    # slices needed for 4d var Tinz and Sinz
                    cslice_4d = self.get_slice("clm_4d", t_idx=clm_tidx, bry=bry, s_idx=-1)
                    bslice_4d = (bry_tidx, n, slice(None), slice(None))

                    # copy from model forecast for variables the clm file has no useful data for
                    for var in cice_vars_3d:
                        bry_name = var.replace("BRY", bry)
                        cice_name = self.cice_names[var.split("_")[0]]
                        self.bry.variables[bry_name][bslice_3d] = self.cice[cice_tidx].variables[cice_name][cice_slice]

                    # copy vertical salinity profile (4d var) from CICE forecast
                    for zlvl in range(self.dims["nkice"]):
                        bslice_z = (bry_tidx, n, zlvl, slice(None))
                        sinz_cice_name = "sice00{}".format(zlvl+1)        # one sice variable per zlvl on input file
                        sinz_bry_name = "Sinz_{}_bry".format(bry)
                        qice_cice_name = "qice00{}".format(zlvl+1)        # one sice variable per zlvl on input file
                        tinz_bry_name = "Tinz_{}_bry".format(bry)
                        self.bry.variables[sinz_bry_name][bslice_z] = self.cice[cice_tidx].variables[sinz_cice_name][cice_slice]*land_mask[cslice[1:]]
                        self.bry.variables[tinz_bry_name][bslice_z] = temperature_mush(self.cice[cice_tidx].variables[qice_cice_name][cice_slice],
                                                                                       self.cice[cice_tidx].variables[sinz_cice_name][cice_slice])*land_mask[cslice[1:]]

                    # # handle vertical profile of temperature
                    # snow_mask = self.bry.variables["vsnon_{}_bry".format(bry)][bslice_3d] > 0.01      # where more than 1cm snow
                    # temp_interp_ice = np.linspace(tair[aslice], toce[cslice_4d], self.dims["nkice"])  # linear interp from ocean to atm
                    # temp_interp_sno = np.linspace(tair[aslice] + 0.2*np.abs(toce[cslice_4d] -\
                    #     tair[aslice]), toce[cslice_4d], self.dims["nkice"])                           # linear interp from ocean to ocean+0.2*|ocean-atm|
                    # temp_interp = np.where(snow_mask, temp_interp_sno, temp_interp_ice)
                    # self.bry.variables["Tinz_{}_bry".format(bry)][bry_tidx,n,0,:] = temp_interp[0,:]*land_mask[cslice[1:]]  # fill category n with vertical temp profile
                    # self.bry.variables["Tinz_{}_bry".format(bry)][bry_tidx,n,1,:] = temp_interp[1,:]*land_mask[cslice[1:]]  # fill category n with vertical temp profile
                    # self.bry.variables["Tinz_{}_bry".format(bry)][bry_tidx,n,2,:] = temp_interp[2,:]*land_mask[cslice[1:]]  # fill category n with vertical temp profile
                    # self.bry.variables["Tinz_{}_bry".format(bry)][bry_tidx,n,3,:] = temp_interp[3,:]*land_mask[cslice[1:]]  # fill category n with vertical temp profile
                    # self.bry.variables["Tinz_{}_bry".format(bry)][bry_tidx,n,4,:] = temp_interp[4,:]*land_mask[cslice[1:]]  # fill category n with vertical temp profile
                    # self.bry.variables["Tinz_{}_bry".format(bry)][bry_tidx,n,5,:] = temp_interp[5,:]*land_mask[cslice[1:]]  # fill category n with vertical temp profile
                    # self.bry.variables["Tinz_{}_bry".format(bry)][bry_tidx,n,6,:] = temp_interp[6,:]*land_mask[cslice[1:]]  # fill category n with vertical temp profile
                    # self.bry.variables["Tinz_{}_bry".format(bry)][bslice_4d] = temperature_mush(self.cice[cice_tidx].variables[])

                    # handle all variables that should be set to zero
                    for var in zero_vars_2d:
                        self.bry.variables[var.replace("BRY", bry)][bslice_2d] = 0.0

                    for var in zero_vars_3d:
                        self.bry.variables[var.replace("BRY", bry)][bslice_3d] = 0.0

    def __del__(self):
        """Destructor closing files if still open."""
        if getattr(self, "grid", None) is not None and self.grid.isopen():
            self.grid.close()

        if getattr(self, "clm", None) is not None and self.clm.isopen():
            self.clm.close()

        if getattr(self, "atm", None) is not None and self.atm.isopen():
            self.atm.close()

        if getattr(self, "cice", None) is not None:
            for f in self.cice:
                if f.isopen():
                    f.close()

        super(CiceBryCiceHax, self).__del__()  # call super destructor to close boundary file


def temperature_mush(Q, S):
    """
    Ported from temperature_mush() subroutine in ice_therm_mushy.f90 in CICE 5.1.2 source code.

    Args:
        enthalpy (array-like)  : Enthalpy of ice (scalar or array of any dimension)
        saltiness (array-like) : Salinity of ice (scalar or array of any dimension)
    """
    # physical constants
    rhoi      = 917.0            # density of ice (kg/m^3)
    rhow      = 1026.0           # density of seawater (kg/m^3)
    cp_ice    = 2106.0           # specific heat of fresh ice (J/kg/K)
    cp_ocn    = 4218.0           # specific heat of ocn    (J/kg/K)
    Lsub      = 2.835e6          # latent heat, sublimation freshwater (J/kg)
    Lvap      = 2.501e6          # latent heat, vaporization freshwater (J/kg)
    Lfresh    = Lsub-Lvap        # latent heat of melting of fresh ice (J/kg)

    # numbers
    puny = 1.0e-11

    # liquidus break
    Tb_liq = -7.6362968855167352                        # temperature of liquidus break
    Sb_liq =  123.66702800276086                        # salinity of liquidus break

    # liquidus relation - higher temperature region
    az1_liq = -18.48
    bz1_liq = 0.0

    # liquidus relation - lower temperature region
    az2_liq = -10.3085
    bz2_liq = 62.4

    # basic liquidus relation constants
    az1p_liq = az1_liq/1000.0
    bz1p_liq = bz1_liq/1000.0
    az2p_liq = az2_liq/1000.0
    bz2p_liq = bz2_liq/1000.0

    # just fully melted enthapy constants
    F1_liq = (-1000.0*cp_ocn*rhow)/az1_liq
    F2_liq = (-1000.0*cp_ocn*rhow)/az2_liq
    G1_liq = -1000.0
    G2_liq = -1000.0
    H1_liq = (-bz1_liq*cp_ocn*rhow)/az1_liq
    H2_liq = (-bz2_liq*cp_ocn*rhow)/az2_liq

    # warmer than fully melted constants
    I_liq = 1.0/(cp_ocn*rhow)

    # break enthalpy constants
    D_liq = ((1.0 + az1p_liq*Tb_liq + bz1p_liq)/(az1_liq*Tb_liq + bz1_liq))*((cp_ocn*rhow - cp_ice*rhoi)*Tb_liq + Lfresh*rhoi)
    E_liq = cp_ice*rhoi*Tb_liq - Lfresh*rhoi

    # quadratic constants - higher temperature region
    AS1_liq = az1p_liq*(rhow*cp_ocn - rhoi*cp_ice)
    AC1_liq = rhoi*cp_ice*az1_liq
    BS1_liq = (1.0 + bz1p_liq)*(rhow*cp_ocn - rhoi*cp_ice) + rhoi*Lfresh*az1p_liq
    BQ1_liq = -az1_liq
    BC1_liq = rhoi*cp_ice*bz1_liq - rhoi*Lfresh*az1_liq
    CS1_liq = rhoi*Lfresh*(1.0 + bz1p_liq)
    CQ1_liq = -bz1_liq
    CC1_liq = -rhoi*Lfresh*bz1_liq

    # quadratic constants - lower temperature region
    AS2_liq = az2p_liq*(rhow*cp_ocn - rhoi*cp_ice)
    AC2_liq = rhoi*cp_ice*az2_liq
    BS2_liq = (1.0 + bz2p_liq)*(rhow*cp_ocn - rhoi*cp_ice) + rhoi*Lfresh*az2p_liq
    BQ2_liq = -az2_liq
    BC2_liq = rhoi*cp_ice*bz2_liq - rhoi*Lfresh*az2_liq
    CS2_liq = rhoi*Lfresh*(1.0 + bz2p_liq)
    CQ2_liq = -bz2_liq
    CC2_liq = -rhoi*Lfresh*bz2_liq

    # just melted enthalpy
    S_low = np.where(S < Sb_liq, 1.0, 0.0)
    q0 = ((F1_liq*S)/(G1_liq + S) + H1_liq)*S_low + ((F2_liq*S)/(G2_liq + S) + H2_liq)*(1.0 - S_low)
    q_melt = np.where(Q > q0, 1.0, 0.0)

    # break enthalpy
    qb = D_liq*S + E_liq
    t_high = np.where(Q > qb, 1.0, 0.0)
    t_low = 1.0 - t_high

    # quadratic values
    A = (AS1_liq*S + AC1_liq)*t_high + (AS2_liq*S + AC2_liq)*t_low
    B = (BS1_liq*S + BQ1_liq*Q + BC1_liq)*t_high + (BS2_liq*S + BQ2_liq*Q + BC2_liq)*t_low
    C = (CS1_liq*S + CQ1_liq*Q + CC1_liq)* t_high + (CS2_liq*S + CQ2_liq*Q + CC2_liq)*t_low

    T = (-B + np.sqrt(np.maximum(B**2 - 4.0*A*C, puny)))/(2.0*A)

    # change T if all melted
    T = q_melt*Q*I_liq + (1.0 - q_melt)*T

    return T
