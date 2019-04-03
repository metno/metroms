
import os
import netCDF4
import numpy as np

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
        self.mode = mode
        self.bry_file = bry_file
        self.dims = {"t": "days", "cat": "ice_types",
                     "z": "nkice", "x": "xi_t", "y": "eta_t"}
        self.bry_vars = self.load_bry_vars()
        self.clm_file = None
        self.clm = None
        self.start_date = None
        self.end_date = None

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
        time dim in clm file) attributes. Also opens the clm file for reading."""
        self.clm_file = clm_file
        self.start_date = start_date
        self.end_date = end_date
        self.clm = netCDF4.Dataset(clm_file, mode="r")

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
            start_idx, end_idx = self.get_time_endpoints()
            self.dim_sizes[self.dims["t"]] = end_idx - start_idx + 1  # +1: include both start and end

        # check that all dimensions are specified
        if not all(True if dn in self.dim_sizes.keys() else False for dn in self.dims.values()):
            raise ValueError("Required dimensions are {}!".format(self.dims.values()))

    def get_time_endpoints(self, time_name="clim_time"):
        """
        Method that gets the indices corresponding to the start and end
        date attributes from the time array in the input climatology

        Args:
            time_name (str) : Name of time variable in climatology file
        Returns:
            start_idx (int) : Index in climatology time of self.start_date
            end_idx (int)   : Index in climatology time of self.end_date
        """
        time_var = self.clm.variables[time_name]
        time = netCDF4.num2date(time_var[:], time_var.units)
        start_idx = np.where(time == self.start_date)[0]
        end_idx = np.where(time == self.end_date)[0]

        if len(start_idx) == 0:
            raise ValueError("Start date {} not in {}!".format(self.start_date, self.clm_file))

        if len(end_idx) == 0:
            raise ValueError("Start date {} not in {}!".format(self.start_date, self.clm_file))

        return start_idx, end_idx

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

        for bry in ["W", "S", "E", "N"]:
            actual_attrs = attrs.copy()

            for key, val in attrs.items():
                actual_attrs[key] = val.replace(bry_replace, bry)

            actual_name = name.replace(bry_replace, bry)
            actual_dims = tuple([d.replace(bry_replace, bry_to_coor[bry]) for d in dims])
            bry_vars.append(BryVar(actual_name, dtype, actual_dims, actual_attrs))

        return bry_vars

    def write_global_attrs(self):
        """Method writing global attributes to the boundary file."""
        setattr(self.bry, "title", "Boundary condition file for CICE for use in METROMS")

    def write_dims(self):
        """Method writing dimensions to boundary file using earlier set self.dim_sizes."""
        if not hasattr(self, "dim_sizes"):
            raise AttributeError("dim_sizes attribute must exist! Use self.set_dims()")

        for name, size in self.dim_sizes.items():
            self.bry.createDimension(name, size)

    def write_vars(self):
        """Method writing variables (metadata only) based on the
        boundary variables created earlier to the boundary file."""
        for var in self.bry_vars:
            self.bry.createVariable(var.name, var.dtype, var.dims)

    def write_var_attrs(self):
        """Method docstring..."""
        for var in self.bry_vars:
            for attr_name, attr_val in var.attrs.items():
                setattr(self.bry.variables[var.name], attr_name, attr_val)

    def write_bry_data(self, method="proper"):
        """Method docstring..."""
        if method == "proper":
            self.topaz2bry_proper()

        elif method == "hack":
            self.topaz2bry_hack()

    def topaz2bry_proper(self):
        """Method docstring..."""
        # to be implemented when new TOPAZ comes out summer 2019
        raise NotImplementedError

    def topaz2bry_hack(self):
        """Method docstring..."""
        raise NotImplementedError

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

    def __del__(self):
        """Destructor closing files if still open."""
        if hasattr(self, "clm") and self.clm is not None and self.clm.isopen():
            self.clm.close()

        if hasattr(self, "bry") and self.bry.isopen():
            self.bry.close()
