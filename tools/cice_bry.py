
import os
import netCDF4
import numpy as np

class bry_var(object):
    """Class docstring..."""
    def __init__(self, name, dtype, dims, attrs):
        """Constructor docstring..."""
        self.name = name
        self.dtype = dtype
        self.dims = dims
        self.attrs = attrs

class cice_bry_file(object):
    """Class for generating a boundary condition file for CICE from a climatology
    file (from TOPAZ) to be used as open BC's with Pedro Duarte's boundary method."""

    def __init__(self, clm_file, bry_file, start_date, end_date, fmt="NETCDF4", overwrite=False):
        """Constructor docstring..."""
        if os.path.exists(bry_file) and not overwrite:
            raise IOError("File {} already exists!".format(bry_file))

        self.clm_file = clm_file
        self.start_date = start_date
        self.end_date = end_date
        self.dimensions = ("days", "nkice", "ice_types", "xi_t", "eta_t")  # pre-determined
        self.clm = netCDF4.Dataset(clm_file, mode="r")
        self.bry = netCDF4.Dataset(bry_file, mode="w", fmt=fmt)
        self.bry_vars = self.load_bry_vars()

    def get_time_endpoints(self, time_name="ocean_time"):
        """Method docstring..."""
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
        """Method docstring..."""
        raise NotImplementedError

    def write_dims(self, dim_sizes):
        """Method docstring..."""
        for dim_name in self.dimensions:
            if dim_name in dim_sizes.keys():
                self.bry.createDimension(dim_name, dim_sizes[dim_name])

            else:
                raise ValueError("Invalid dimension {}. Must be {}!".format(dim_name, self.dimensions))

    def write_vars(self):
        """Method docstring..."""
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

    def get_necessary_variables(self):
        """Method that gives a list of all variable sthat need to be
        defined in the boundary file before it is used by CICE."""
        bry_names = ["Time", "Ice_layers"]
        bry_templates = ["TLAT", "TLON", "Sinz", "Tinz", "Tsfc", "aicen",
                         "alvln", "vlvln", "vicen", "vsnon", "apondn",
                         "hpondn", "ipondn", "fbrine", "hbrine", "iage"]

        for var_name in bry_templates:
            for direction in ["W", "S", "E", "N"]:
                if var_name in ["TLON", "TLAT"]:
                    bry_names.append(var_name + "_{}".format(direction))

                else:
                    bry_names.append(var_name + "_{}_bry".format(direction))

        return bry_names

    def verify_variables(self):
        """Method that checks if the boundary file contains all necessary variables."""
        bry_names = get_necessary_variables()

        # check if there is a missing variable in the file
        for var_name in bry_names:
            if var_name not in self.bry.variables.keys():
                raise ValueError("Variable {} not in file {}!".format(var_name, self.bry_file))

        # chekc if theres are variables in the file that should not be there
        for var_name in self.bry.variables.keys():
            if var_name not in bry_names:
                raise ValueError("Variable {} is not needed in CICE bry file!".format(var_name))

        print("File {} contains all expected boundary variables!".format(self.bry_file))
        return True

    def __del__(self):
        """Destructor closing files if still open."""
        if hasattr(self, "clm") and self.clm.isopen():
            self.clm.close()

        if hasattr(self, "bry") and self.bry.isopen():
            self.bry.close()

if __name__ == "__main__":
    clm_file = "/lustre/storeB/users/josteinb/cice_bry_testing/barents_clm.nc"
    bry_file = "/lustre/storeB/users/josteinb/cice_bry_testing/cice_bry_test.nc"
    dim_sizes = {"days": 1, "nkice": 7, "ice_types": 5, "xi_t": 739, "eta_t": 949}
    #bry = cice_bry_file(clm_file, bry_file, 1, 1, overwrite=True)
    #bry.write_dims(dim_sizes)
    #bry.write_vars()
