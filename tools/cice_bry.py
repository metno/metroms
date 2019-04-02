
import os
import netCDF4
import numpy as np

def omnibry_var(name, dtype, dims, attrs, bry_replace="BRY"):
    """
    Function that creates a BryVar object for each boundary wall (W, S, E, N)
    based on the supplied input BryVar template input parameters.

    Args:
        name (str) : Template for name of variable (replace bry_replace)
        dtype (type) : Datatype of variable
        dims (tuple) : Tuple of strings for dims of variable (replace bry_replace)
        attrs (dict) : Dict of <str: str> (variable attributes) (replace bry_replace)
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

class BryVar(object):
    """Class for representing a CICE boundary variable that can
    later be used when writing data to a CICE boundary file."""
    def __init__(self, name, dtype, dims, attrs):
        """Constructor setting the supplied paramters as attributes."""
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
    """Class for generating a boundary condition file for CICE from a
    climatology file to be used as open BC's with Pedro Duarte's boundary method."""

    def __init__(self, clm_file, bry_file, start_date, end_date, fmt="NETCDF4", overwrite=False):
        """Constructor docstring..."""
        if os.path.exists(bry_file) and not overwrite:
            raise IOError("File {} already exists!".format(bry_file))

        self.clm_file = clm_file
        self.bry_file = bry_file
        self.start_date = start_date
        self.end_date = end_date
        self.dimensions = ("days", "nkice", "ice_types", "xi_t", "eta_t")  # pre-determined
        self.clm = netCDF4.Dataset(clm_file, mode="r")
        self.bry = netCDF4.Dataset(bry_file, mode="w", fmt=fmt)
        self.bry_vars = self.load_bry_vars()

    def load_bry_vars(self):
        """Method that generates a BryVar object for each variable necessary to specify
        in the boundary file (most variable names are on the form <name>_W/S/E/N_bry)."""
        bry_vars = list()
        bry_vars.append(BryVar("Time", np.int16, ("days"),
                               {"long_name": "model time", "units": "days since 2008-01-01 00:00:00"}))
        bry_vars.append(BryVar("Ice_layers", np.int16, ("nkice"),
                               {"long_name": "vertical ice levels", "units": "1"}))
        bry_vars += omnibry_var("TLON_BRY", float, ("BRY",),
                                {"long_name": "T grid center longitude - BRY boundary", "units": "degrees_east"})
        bry_vars += omnibry_var("TLAT_BRY", float, ("BRY",),
                                {"long_name": "T grid center latitude - BRY boundary", "units": "degrees_north"})
        bry_vars += omnibry_var("Tsfc_BRY_bry", float, ("days", "ice_types", "BRY"),
                                {"long_name": "snow/ice surface temperature - BRY boundary"})
        bry_vars += omnibry_var("aicen_BRY_bry", float, ("days", "ice_types", "BRY"),
                                {"long_name": "ice area (aggregate) - BRY boundary"})
        bry_vars += omnibry_var("vicen_BRY_bry", float, ("days", "ice_types", "BRY"),
                                {"long_name": "grid cell mean ice thickness - BRY boundary", "units": "m"})
        bry_vars += omnibry_var("vsnon_BRY_bry", float, ("days", "ice_types", "BRY"),
                                {"long_name": "grid cell mean snow thickness - BRY boundary", "units": "m"})
        bry_vars += omnibry_var("apond_BRY_bry", float, ("days", "ice_types", "BRY"),
                                {"long_name": "melt pond fraction - BRY boundary", "units": "1"})
        bry_vars += omnibry_var("hpond_BRY_bry", float, ("days", "ice_types", "BRY"),
                                {"long_name": "mean melt pond depth - BRY boundary", "units": "m"})
        bry_vars += omnibry_var("ipond_BRY_bry", float, ("days", "ice_types", "BRY"),
                                {"long_name": "mean melt pond ice thickness - BRY boundary", "units": "m"})
        bry_vars += omnibry_var("fbrine_BRY_bry", float, ("days", "ice_types", "BRY"),
                                {"long_name": "ratio of brine tracer height to ice thickness - BRY boundary", "units": "1"})
        bry_vars += omnibry_var("hbrine_BRY_bry", float, ("days", "ice_types", "BRY"),
                                {"long_name": "brine surface height above sea ice base - BRY boundary", "units": "m"})
        bry_vars += omnibry_var("iage_BRY_bry", float, ("days", "ice_types", "BRY"),
                                {"long_name": "ice age - BRY boundary"})
        bry_vars += omnibry_var("alvln_BRY_bry", float, ("days", "ice_types", "BRY"),
                                {"long_name": "concentration of level ice - BRY boundary", "units": "1"})
        bry_vars += omnibry_var("vlvln_BRY_bry", float, ("days", "ice_types", "BRY"),
                                {"long_name": "volume per unit of area of level ice - BRY boundary", "units": "m"})
        bry_vars += omnibry_var("Tinz_BRY_bry", float, ("days", "ice_types", "nkice", "BRY"),
                                {"long_name": "vertical temperature profile - BRY boundary"})
        bry_vars += omnibry_var("Sinz_BRY_bry", float, ("days", "ice_types", "nkice", "BRY"),
                                {"long_name": "vertical salinity profile - BRY boundary"})
        return bry_vars

    def get_time_endpoints(self, time_name="ocean_time"):
        """
        Method that gets the indices corresponding to the start and end
        date attributes from the time array in the input climatology

        Returns:
            start_idx (int) : Index in climatology time of self.start_date
            end_idx (int) : Index in climatology time of self.end_date
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

    def write_global_attrs(self):
        """Method writing global attributes to the boundary file."""
        setattr(self.bry, "title", "Boundary condition file for CICE for use in METROMS")

    def write_dims(self, dim_sizes):
        """Method writing dimensions to the boundary file."""
        for dim_name in self.dimensions:
            if dim_name in dim_sizes.keys():
                self.bry.createDimension(dim_name, dim_sizes[dim_name])

            else:
                raise ValueError("Invalid dimension {}. Must be {}!".format(dim_name, self.dimensions))

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
        if hasattr(self, "clm") and self.clm.isopen():
            self.clm.close()

        if hasattr(self, "bry") and self.bry.isopen():
            self.bry.close()
