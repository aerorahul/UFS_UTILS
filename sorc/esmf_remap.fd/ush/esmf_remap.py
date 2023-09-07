# =========================================================================
# File: esmf_remap.fd/ush/esmf_remap.py
# Author: Henry R. Winterbottom
# Date: 31 August 2023
# Version: 0.0.1
# License: LGPL v2.1
# =========================================================================

"""
Module
------

    esmf_remap.py

Description
-----------

    This module contains the base-class object for the Earth System
    Modeling Framework (ESMF) remapping/regridding application.

Classes
-------

    ESMFRemap(yaml_file)

        This is the base-class object for the ESMF remapping
        application.

Requirements
------------

- wxflow; https://github.com/NOAA-EMC/wxflow

Author(s)
---------

    Henry R. Winterbottom; 31 August 2023

History
-------

    2023-08-31: Henry Winterbottom -- Initial implementation.

"""

# ----

# pylint: disable=consider-using-dict-items
# pylint: disable=fixme

# ----

import os
import shutil
from tempfile import NamedTemporaryFile
from typing import Generic

from wxflow import YAMLFile
from wxflow.attrdict import AttrDict
from wxflow.executable import Executable
from wxflow.jinja import Jinja

from grids import Grids
from ncio import NCIO

# ----

__all__ = ["ESMFRemap"]

# ----


class ESMFRemap:
    """
    Description
    -----------

    This is the base-class object for the ESMF remapping application.

    Parameters
    ----------

    yaml_file: str

        A Python string specifying the path to the YAML-formatted
        configuration file.

    """

    def __init__(self: Generic, yaml_file: str):
        """
        Description
        -----------

        Creates a new ESMFRemap class.

        """

        # Define the base-class attributes.
        self.yaml_dict = YAMLFile(path=yaml_file)
        self.ncout_obj = NCIO(ncfile=self.yaml_dict.ncoutput, write=True)
        with NamedTemporaryFile(delete=False) as tmp_file:
            self.regrid_nml_tmp = tmp_file.name
        with NamedTemporaryFile(delete=True) as tmp_file:
            self.ncout_file_tmp = tmp_file.name
        self.executable = Executable(name=self.yaml_dict.regrid_exec)

        # TODO: This could be replaced using schema.py.
        self.dstgrid_default_dict = {"scrip": False}

    def build_nml(self: Generic, variable: str) -> None:
        """
        Description
        -----------

        This method populates a Jinja-templated file for the
        regridding application FORTRAN namelist.

        Parameters
        ----------

        variable: str

            A Python string specifying the respective variable.

        """
        nml_tmpl_dict = {
            "esmf_coeffs_file": "esmf_coeffs_file",
            "invar_file": "ncfile",
            "invar_name": "ncvarname",
            "outvar_name": "ncvarname",
        }
        data = AttrDict()
        data.outvar_file = self.ncout_file_tmp
        data.ncoutput = self.yaml_dict.ncoutput
        for nml_tmpl_key in nml_tmpl_dict:
            value = nml_tmpl_dict[nml_tmpl_key]
            data[nml_tmpl_key] = self.yaml_dict.variables[variable][value]
        Jinja(self.yaml_dict.nml_tmpl, data=data).save(
            output_file=self.regrid_nml_tmp)

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
            ncdimname="lats", ncdimsize=len(dstgrid_dict.lats[:, 0])
        )
        self.ncout_obj.write_ncdim(
            ncdimname="lons", ncdimsize=len(dstgrid_dict.lons[0, :])
        )

        # Define the netCDF-formatted arrays for the respective
        # variables.
        for variable in self.yaml_dict.variables:
            ncvar_dict = AttrDict()
            ncio_obj = NCIO(
                ncfile=self.yaml_dict.variables[variable].ncfile, read=True)
            ncinfo_dict = ncio_obj.read_ncinfo(
                ncvarname=self.yaml_dict.variables[variable].ncvarname
            )
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

    def read_dstgrid(self: Generic) -> AttrDict:
        """
        Description
        -----------

        This method parses the Python dictionary containing the
        contents of the YAML-formatteed configuration file and returns
        the destination grid attributes.

        Returns
        -------

        dstgrid_dict: AttrDict

        """

        # Compute/define the destination grid attributes.
        dstgrid_dict = Grids(gridinfo_dict=self.yaml_dict.grid).grid

        return dstgrid_dict

    def regrid(self: Generic) -> None:
        """
        Description
        -----------

        This method regrids the specified variables in accordance with
        the configuration.

        """

        # Regrid the specified variables accordingly.
        for variable in self.yaml_dict.variables:
            shutil.copy(self.yaml_dict.ncoutput, self.ncout_file_tmp)
            self.build_nml(variable=variable)
            self.executable.add_default_arg(f"{self.regrid_nml_tmp}")
            self.executable()
        try:
            os.unlink(self.regrid_nml_tmp)
            os.unlink(self.ncout_file_tmp)
        except FileNotFoundError:
            pass

    def run(self: Generic) -> None:
        """
        Description
        -----------

        This module performs the following tasks.

        (1) Parses the YAML-formatted configuration file.

        (2) Collects the destination grid attibutes.

        (3) Builds/initializes the output netCDF-formatted file path.

        (4) Regrids/remaps the respective variables and updates the
            specified netCDF-formatted output file path.

        """

        # Define the destination grid attributes and regrid/remap the
        # specified variables accordingly.
        dstgrid_dict = self.read_dstgrid()
        self.init_ncoutput(dstgrid_dict=dstgrid_dict)
        self.regrid()
