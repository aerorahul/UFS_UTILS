# TODO


# 2. Remap the respective variables.

# 3. Write out remapped variables to the specified netCDF output
# file.

import numpy
from netCDF4 import Dataset
from wxflow import YAMLFile
from typing import Dict, Generic
from wxflow.attrdict import AttrDict
from ncio import NCIO
from grids import Grids
from regrid import Regrid

# ----

__all__ = ["ESMFRemap"]

# ----


class ESMFRemap:
    """ """

    def __init__(self: Generic, yaml_file: str):
        """
        Description
        -----------

        Creates a new ESMFRemap class.

        """

        # Define the base-class attributes.
        self.yaml_dict = YAMLFile(path=yaml_file)
        self.ncout_obj = NCIO(ncfile=self.yaml_dict.ncoutput, write=True)

        # TODO: This could be replaced using schema.py.
        self.dstgrid_default_dict = {
            "scrip": False
        }

    def init_ncoutput(self: Generic, dstgrid_dict: AttrDict) -> None:
        """
        Description
        -----------

        This method initializes the output netCDF-formatted file path.

        Parameters
        ----------

        dstgrid_dict: AttrDict

            A Python AttrDict containing the destination grid
            attributes (i.e., the attributes to which the specified
            variables will be remapped).

        """

        # Configure the output grid array dimension attributes.
        ncio_ncinput_obj = NCIO(ncfile=self.yaml_dict.ncinput, read=True)
        ncio_grid_obj = NCIO(ncfile=self.yaml_dict.grid.ncfile, read=True)
        dims_dict = dict(ncio_ncinput_obj.get_ncdims(),
                         **ncio_grid_obj.get_ncdims())
        ncio_ncinput_obj.close()
        ncio_grid_obj.close()
        for (dim_key, dim_value) in dims_dict.items():
            self.ncout_obj.write_ncdim(ncdimname=dim_key, ncdimsize=dim_value)
        self.ncout_obj.write_ncdim(
            ncdimname="lats", ncdimsize=len(dstgrid_dict.lats[:, 0]))
        self.ncout_obj.write_ncdim(
            ncdimname="lons", ncdimsize=len(dstgrid_dict.lons[0, :]))

        # Define the netCDF-formatted arrays for the respective
        # variables.
        for variable in self.yaml_dict.variables:
            ncvar_dict = AttrDict()
            ncio_obj = NCIO(ncfile=self.yaml_dict.variables[variable].ncfile,
                            read=True)
            ncinfo_dict = ncio_obj.read_ncinfo(
                ncvarname=self.yaml_dict.variables[variable].ncvarname)
            dimid_list = []
            for (idx, dimid) in enumerate(ncinfo_dict.dims):
                if idx < len(ncinfo_dict.dims) - 2:
                    dimid_list.append(dimid)
            ncvar_dict.dims = tuple(dimid_list + ["lats", "lons"])
            ncvar_dict.name = variable
            ncvar_dict.attrs = ncinfo_dict.attrs
            self.ncout_obj.create_ncvar(ncvar_dict=ncvar_dict)

        # Define the COORDS convention geographical attribute arrays.
        ncvar_dict = AttrDict()
        ncvar_dict.dims = ("lats", "lons")
        ncvar_dict.name = "lats"
        self.ncout_obj.create_ncvar(ncvar_dict=ncvar_dict)
        ncvar_dict.name = "lons"
        self.ncout_obj.create_ncvar(ncvar_dict=ncvar_dict)
        self.ncout_obj.close()

    def read_dstgrid(self: Generic, yaml_dict: AttrDict) -> AttrDict:
        """
        Description
        -----------

        This method parses the Python dictionary containing the
        contents of the YAML-formatteed configuration file and returns
        the destination grid attributes.

        Parameters
        ----------

        yaml_dict: AttrDict

            A Python AttrDict containing the contents of the
            YAML-formatted configuration file.

        Returns
        -------

        dstgrid_dict: AttrDict

        """

        # Compute/define the destination grid attributes.
        dstgrid_dict = Grids(gridinfo_dict=self.yaml_dict.grid).grid

        return dstgrid_dict

    def regrid(self: Generic) -> None:
        """ """

        for variable in self.yaml_dict.variables:
            regrid_obj = Regrid(varinfo=self.yaml_dict.variables[variable])

    def run(self: Generic) -> None:
        """
        Description
        -----------

        This module performs the following tasks.

        (1) Parses the YAML-formatted configuration file.

        (2) Collects the destination grid attibutes.

        (3) Builds/initializes the output netCDF-formatted file path.

        """

        dstgrid_dict = self.read_dstgrid(yaml_dict=self.yaml_dict)
        self.init_ncoutput(dstgrid_dict=dstgrid_dict)
        self.regrid()
