
# TODO:
# * (issue) : When using self.write_bry_data(method="hax"), one currently cannot specify
#   a date later than what is in the atmosphere file. Add support for allowing to specify
#   at least one day after the final date in the atm file (the clm file usually has a lot
#   longer time series so this would be useful).
# * (enhance): More general handling of ice categories (might vary between models).
#   The category limits are hardcoded in as an attribute at this point, but should
#   perhaps be inputed by user in the future.
# * (enahnce) Some of the usage of variable names, especially when extracting time array
#   from clm/atm file should be made more general and not hard code in e.g. "clim_time".

import os
import netCDF4
import numpy as np
import datetime as dt

class BryVar(object):
    """
    Class for representing a CICE boundary variable that can
    later be used when writing data to a CICE boundary file.

    Example usage:
        > var = BryVar('aicen', float, ('days', 'ice_types', "xi_t"), {'units': '1'})
        > print(var)
    """
    def __init__(self, name, dtype, dims, attrs):
        """Constructor setting the supplied parameters as attributes."""
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
    Class for generating a boundary condition file for CICE from a climatology file
    to be used as open BC's with Pedro Duarte's boundary method. An instance can be
    initiated for writing a new boundary file (its main purpose) or for viewing/verifying
    an existing boundary file.

    Example usage:
        > ...
    """

    def __init__(self, bry_file, mode="w", fmt="NETCDF4", overwrite=True):
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
        self.dims = {"t": "days", "cat": "ice_types",
                     "z": "nkice", "x": "xi_t", "y": "eta_t"}
        self.dim_sizes = None                                                # set by calling self.set_dims()
        self.bry_order = ["W", "S", "E", "N"]
        self.cat_lims = [0, 0.6445072, 1.391433, 2.470179, 4.567288, 1e+08]  # upper limits of ice categories
        self.bry_vars = self.load_bry_vars()

        # climatology file attributes that can e.g. be set in self.set_clm()
        self.clm_file = None
        self.clm = None
        self.start_date = None
        self.end_date = None

        # atmosphere file attributes that can e.g. be set in self.set_clm()
        self.atm_file = None
        self.atm = None

        # grid file attributes that can e.g. be set in self.set_grid()
        self.grid_file = None
        self.grid = None

        # open the boundary file according to user preferances
        if mode == "r":
            self.bry = netCDF4.Dataset(bry_file, mode="r")

        elif mode == "w":
            if os.path.exists(bry_file) and not overwrite:
                raise IOError("File {} exists! Change <bry_file>, <mode> or <overwrite>".format(bry_file))

            self.bry = netCDF4.Dataset(bry_file, mode="w", fmt=fmt)

        else:
            raise ValueError("Invalid value {} for keyword 'mode'".format(mode))

    def set_clm(self, clm_file, start_date, end_date):
        """Method that sets the climatotlogy file, start- and end date (for
        time dim in clm file) attributes. Also opens the clm file for reading.

        Args:
            clm_file (str) : Filename of climatology file
            start_date (datetime) : Datetime of first desired time in boundary file
            end_date (datetime) : Datetime of last desired time in boundary file
        """
        self.clm_file = clm_file
        self.start_date = start_date
        self.end_date = end_date
        self.clm = netCDF4.Dataset(clm_file, mode="r")

    def set_atm(self, atm_file):
        """
        Method that sets the atmosphere filename attribute and opens the dataset
        and stores that as an attribute as well.

        Args:
            atm_file (str) : Filename of climatology file
        """
        self.atm_file = atm_file
        self.atm = netCDF4.Dataset(atm_file, mode="r")

    def set_grid(self, grid_file):
        """
        Method that sets the grid filename attribute and opens the dataset
        and stores that as an attribute as well.

        Args:
            atm_file (str) : Filename of climatology file
        """
        self.grid_file = grid_file
        self.grid = netCDF4.Dataset(grid_file, mode="r")

    def set_dims(self, use_clm_time=False, **dim_sizes):
        """
        Method for specifying the sizes of the dimensions to be written to the boundary file. Should
        correspond with the shapes of the data arrays that are potentailly written to file later.

        Args:
            use_clm_time (bool) : Whether or not to automatically get time size from clm file
            dim_sizes (dict)    : Dict with <dim_name: dim_size>
        """
        for name, size in dim_sizes.items():
            if name not in self.dims.values():
                raise ValueError("Invalid dimension {}. Must be {}!".format(name, self.dims.values()))

            elif type(size) not in [int, np.int, np.int8, np.int16, np.int32, np.int64]:
                raise TypeError("Invalid type {} for dim {}! Must be int-like.".format(type(size), name))

        self.dim_sizes = dim_sizes.copy()

        # extract number of time entries from clm file
        if use_clm_time:
            start_idx, end_idx = self.get_time_endpoints(self.clm, "clim_time")
            self.dim_sizes[self.dims["t"]] = end_idx - start_idx + 1  # +1: include both start and end

        # check that all dimensions are specified
        if not all(True if dn in self.dim_sizes.keys() else False for dn in self.dims.values()):
            raise ValueError("Required dimensions are {}!".format(self.dims.values()))

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
        time = self.get_time_array(nc_file, time_name)
        start_idx = np.where(time == self.start_date)[0]
        end_idx = np.where(time == self.end_date)[0]

        if len(start_idx) == 0:
            raise ValueError("Start date {} not in {}!".format(self.start_date, nc_file.filepath()))

        if len(end_idx) == 0:
            raise ValueError("End date {} not in {}!".format(self.end_date, nc_file.filepath()))

        return start_idx[0], end_idx[0]

    def load_bry_vars(self):
        """Method that generates a BryVar object for each variable necessary to specify
        in the boundary file (most variable names are on the form <name>_{W,S,E,N}_bry)."""
        bry_vars = list()
        bry_vars.append(BryVar("Time", np.int16, ("days"),
            {"long_name": "model time", "units": "days since 2008-01-01 00:00:00"}))
        bry_vars.append(BryVar("Ice_layers", np.int16, ("nkice"),
            {"long_name": "vertical ice levels", "units": "1"}))
        bry_vars += self.omnibry_var("TLON_BRY", float, ("BRY",),
            {"long_name": "T grid center longitude - BRY boundary", "units": "degrees_east"})
        bry_vars += self.omnibry_var("TLAT_BRY", float, ("BRY",),
            {"long_name": "T grid center latitude - BRY boundary", "units": "degrees_north"})
        bry_vars += self.omnibry_var("Tsfc_BRY_bry", float, ("days", "ice_types", "BRY"),
            {"long_name": "snow/ice surface temperature - BRY boundary"})
        bry_vars += self.omnibry_var("aicen_BRY_bry", float, ("days", "ice_types", "BRY"),
            {"long_name": "ice area (aggregate) - BRY boundary"})
        bry_vars += self.omnibry_var("vicen_BRY_bry", float, ("days", "ice_types", "BRY"),
            {"long_name": "grid cell mean ice thickness - BRY boundary", "units": "m"})
        bry_vars += self.omnibry_var("vsnon_BRY_bry", float, ("days", "ice_types", "BRY"),
            {"long_name": "grid cell mean snow thickness - BRY boundary", "units": "m"})
        bry_vars += self.omnibry_var("apond_BRY_bry", float, ("days", "ice_types", "BRY"),
            {"long_name": "melt pond fraction - BRY boundary", "units": "1"})
        bry_vars += self.omnibry_var("hpond_BRY_bry", float, ("days", "ice_types", "BRY"),
            {"long_name": "mean melt pond depth - BRY boundary", "units": "m"})
        bry_vars += self.omnibry_var("ipond_BRY_bry", float, ("days", "ice_types", "BRY"),
            {"long_name": "mean melt pond ice thickness - BRY boundary", "units": "m"})
        bry_vars += self.omnibry_var("fbrine_BRY_bry", float, ("days", "ice_types", "BRY"),
            {"long_name": "ratio of brine tracer height to ice thickness - BRY boundary", "units": "1"})
        bry_vars += self.omnibry_var("hbrine_BRY_bry", float, ("days", "ice_types", "BRY"),
            {"long_name": "brine surface height above sea ice base - BRY boundary", "units": "m"})
        bry_vars += self.omnibry_var("iage_BRY_bry", float, ("days", "ice_types", "BRY"),
            {"long_name": "ice age - BRY boundary"})
        bry_vars += self.omnibry_var("alvln_BRY_bry", float, ("days", "ice_types", "BRY"),
            {"long_name": "concentration of level ice - BRY boundary", "units": "1"})
        bry_vars += self.omnibry_var("vlvln_BRY_bry", float, ("days", "ice_types", "BRY"),
            {"long_name": "volume per unit of area of level ice - BRY boundary", "units": "m"})
        bry_vars += self.omnibry_var("Tinz_BRY_bry", float, ("days", "ice_types", "nkice", "BRY"),
            {"long_name": "vertical temperature profile - BRY boundary"})
        bry_vars += self.omnibry_var("Sinz_BRY_bry", float, ("days", "ice_types", "nkice", "BRY"),
            {"long_name": "vertical salinity profile - BRY boundary"})
        return bry_vars

    def omnibry_var(self, name, dtype, dims, attrs, bry_replace="BRY"):
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
        bry_to_coor = {"W": "eta_t", "S": "xi_t", "E": "eta_t", "N": "xi_t"}

        for bry in self.bry_order:
            actual_attrs = attrs.copy()

            for key, val in attrs.items():
                actual_attrs[key] = val.replace(bry_replace, bry)

            actual_name = name.replace(bry_replace, bry)
            actual_dims = tuple([d.replace(bry_replace, bry_to_coor[bry]) for d in dims])
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
        """Method writing dimensions to boundary file using earlier set self.dim_sizes."""
        if self.dim_sizes is None:
            raise AttributeError("dim_sizes attribute is not set! Use self.set_dims()")

        for name, size in self.dim_sizes.items():
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

    def write_basic_data(self, time_units):
        """
        Method that writes data for the time variable itself and all other time-independent
        variables to the boundary file.

        Args:
            time_units (str) : Units for time values used to convert from/to datetime and
                               when writing the 'units' attribute to the file. Give in
                               e.g. 'days since 2019-01-01 00:00:00'
        """
        # set time variable based on user-specified units
        clm_time = self.get_time_array(self.clm, "clim_time")
        t0_idx, t1_idx = self.get_time_endpoints(self.clm, "clim_time")
        bry_time = netCDF4.date2num(clm_time, time_units)[t0_idx:t1_idx+1]
        self.set_var_attr("Time", "units", time_units, update_file=True)
        self.bry.variables["Time"][:] = bry_time

        # set vertical dimension variable
        self.bry.variables["Ice_layers"][:] = list(range(self.dim_sizes[self.dims["z"]]))

        # write longitude and latitude based on the grid file
        for bry in self.bry_order:
            grid_slice = self._get_bry_slice("grid_2d", bry=bry)
            self.bry.variables["TLON_{}".format(bry)][:] = self.grid.variables["lon_rho"][grid_slice]
            self.bry.variables["TLAT_{}".format(bry)][:] = self.grid.variables["lat_rho"][grid_slice]

    def write_bry_data(self, method="proper"):
        """
        Method that writes data to file by calling more specific methods. Here more
        similar methods can be added if other approaches are desired.

        Args:
            method (str) : What method to use when generating the boundary data
        """
        if method == "proper":
            self.topaz2bry_proper()

        elif method == "hax":
            self.topaz2bry_hax()

    def topaz2bry_proper(self):
        """Method docstring..."""
        # to be implemented when new TOPAZ comes out summer 2019
        raise NotImplementedError

    def topaz2bry_hax(self):
        """
        Method that implements a hack to use the limited ice data from TOPAZ5 to generate
        data for the CICE boundary file. Relevant available variables from TOPAZ are only
        aice (ice conc.) and hice (ice thick.). From these, we generate the CICE variables
        aicen, vicen and vsnon at the boundaries. Ice conc. ice distributed amongst the ice
        categories (according the pre-defined thickness categories (self.cat_lims)) and so
        is ice volume. Snow volume is assumed to be 20% that of the ice volume.

        Additionally, atmosphere (from relevant model at corrsponding times) and ocean (from
        clm file) data is used to help specify data for the variablesTsfc, Tinz and Sinz. Tsfc
        is assumed to be equal to the atmosphere surface temp. Tinz is linearly interpolated
        from the ocean surface temp to the air temperature (if there is less than 1cm of snow
        (at the particular grid cell in question)) and to 0.2*|ocean_temp - atm_temp| (if there
        is more than 1cm of snow). Sinz is linearly interpolated from the ocean surface salinity
        to 0.2 of the ocean surface salinity.

        All other time-dependent variables that are needed in the boundary file (see
        self.load_bry_vars()) are set to zero everywhere because of a lack of a better choice.
        """
        if self.clm is None:
            raise AttributeError("No clm file has been set! Use self.set_clm().")
        if self.atm is None:
            raise AttributeError("No atm file has been set! Use self.set_atm().")
        if self.grid is None:
            raise AttributeError("No grid file has been set! Use self.set_grid().")

        # get time arrays needed for loop below
        clm_time = self.clm.variables["clim_time"]
        clm_dates = netCDF4.num2date(clm_time[:], clm_time.units)
        t0c_idx, t1c_idx = self.get_time_endpoints(self.clm, "clim_time")
        atm_time = self.get_time_array(self.atm, "time")

        # extract pointers for all releevant variables from climatology and atmopshere files
        aice = self.clm.variables["aice"]            # clm ice concentration
        hice = self.clm.variables["hice"]            # clm ice thickness
        hsno = self.clm.variables["snow_thick"]      # clm snow thickness (seems to be zero everywhere all the time(?))
        tair = self.atm.variables["Tair"]            # atm surface temperature
        toce = self.clm.variables["temp"]            # ocean surface temperature
        salt = self.clm.variables["salt"]            # ocean surface salinity
        land_mask = self.grid.variables["mask_rho"]  # grid file land mask

        # variables that, due to lack of better data, is set to zero everywhere
        zero_vars_3d = ["iage_BRY_bry", "apond_BRY_bry", "hpond_BRY_bry", "ipond_BRY_bry",
                        "fbrine_BRY_bry", "hbrine_BRY_bry", "alvln_BRY_bry", "vlvln_BRY_bry"]

        # handle one time index at a time for memory efficiency
        for clm_idx in range(t0c_idx, t1c_idx + 1):
            print("Computing boundary data for {}...".format(clm_dates[clm_idx]))
            bry_idx = clm_idx - t0c_idx  # time index for bry file starts at 0
            atm_idx = (np.abs(atm_time - clm_dates[clm_idx])).argmin()

            # loop over all ice categories and fill from clm and atm data
            for n in range(len(self.cat_lims[1:])):
                #print("\tCategory {}".format(n))

                # loop over all 4 boundaries (W, S, E, N)
                for bry in self.bry_order:
                    #print("\t\tBoundary {}".format(bry))
                    # slices for 3d variables aice, hice and Tsfc
                    cslice = self._get_bry_slice("clm_3d", t_idx=clm_idx, bry=bry)  # slices for clim variables
                    aslice = self._get_bry_slice("clm_3d", t_idx=atm_idx, bry=bry)  # slices for atm variables
                    bslice = (bry_idx, n, slice(None))                              # slices for bry variables

                    # masking out all other ice categories except category n
                    cat_mask = np.logical_and(hice[cslice] > self.cat_lims[n], hice[cslice] < self.cat_lims[n+1])

                    # handle ice concentration, ice volume and snow volume (distribute over the categories)
                    self.bry.variables["aicen_{}_bry".format(bry)][bslice] = np.where(cat_mask,
                        aice[cslice], 0.0)*land_mask[cslice[1:]]                                                # set to clm ice conc.
                    self.bry.variables["vicen_{}_bry".format(bry)][bslice] = np.where(cat_mask,
                        hice[cslice]*aice[cslice], 0.0)*land_mask[cslice[1:]]                                   # set to clm ice vol. x conc.
                    self.bry.variables["vsnon_{}_bry".format(bry)][bslice] = np.where(cat_mask,
                        0.2*self.bry.variables["vicen_{}_bry".format(bry)][bslice], 0.0)*land_mask[cslice[1:]]  # set to 0.2 of ice vol.
                    self.bry.variables["Tsfc_{}_bry".format(bry)][bslice] = np.where(cat_mask,
                        tair[aslice], 0.0)*land_mask[cslice[1:]]                                                # set to surface temp from atm

                    # slices needed for 4d vars Tinz and Sinz
                    cslice_4d = self._get_bry_slice("clm_4d", t_idx=clm_idx, bry=bry, s_idx=-1)
                    bslice_4d = (bry_idx, n, slice(None), slice(None))

                    # handle vertical profiles of salinity
                    salt_interp = np.linspace(salt[cslice_4d], 0.2*salt[cslice_4d], self.dim_sizes[self.dims["z"]])  # linear interp from ocean to 0.2*ocean
                    self.bry.variables["Sinz_{}_bry".format(bry)][bslice_4d] = np.where(cat_mask,
                        salt_interp, 5.0)*land_mask[cslice[1:]]                                                      # fill category n with vertical salt profile

                    # handle vertical profiles of salinity
                    snow_mask = self.bry.variables["vsnon_{}_bry".format(bry)][bslice] > 0.01                        # where more than 1cm snow
                    temp_interp_ice = np.linspace(toce[cslice_4d], tair[aslice], self.dim_sizes[self.dims["z"]])     # linear interp from ocean to atm
                    temp_interp_sno = np.linspace(toce[cslice_4d], toce[cslice_4d] - 0.2*np.abs(toce[cslice_4d] -\
                        tair[aslice]), self.dim_sizes[self.dims["z"]])                                               # linear interp from ocean to ocean+0.2*|ocean-atm|
                    temp_interp = np.where(snow_mask, temp_interp_sno, temp_interp_ice)
                    self.bry.variables["Tinz_{}_bry".format(bry)][bslice_4d] = np.where(cat_mask,
                        temp_interp, -5.0)*land_mask[cslice[1:]]                                                     # fill category n with vertical temp profile

                    # handle all variables that are set to zero
                    for var in zero_vars_3d:
                        self.bry.variables[var.replace("BRY", bry)][bslice] = 0.0

    def _get_bry_slice(self, slice_type, **kwargs):
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

    def ncdump_bry(self):
        """Method that does a system call to 'ncdump' on the boundary file."""
        os.system("ncdump -h {}".format(self.bry_file))

    def __str__(self):
        """Method docstring..."""
        raise NotImplementedError

    def __del__(self):
        """Destructor closing files if still open."""
        if hasattr(self, "clm") and self.clm is not None and self.clm.isopen():
            self.clm.close()

        if hasattr(self, "bry") and self.bry.isopen():
            self.bry.close()
