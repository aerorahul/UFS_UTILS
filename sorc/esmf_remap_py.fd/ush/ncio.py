# =========================================================================
# File: esmf_remap_py.fd/ush/ncio.py
# Author: Henry R. Winterbottom
# Date: 24 August 2023
# Version: 0.0.1
# License: LGPL v2.1
# =========================================================================

"""
Module
------

    ncio.py

Description
-----------

    This module contains the base-class object for all
    netCDF-formatted file and file object applications.

Classes
-------

    NCIO(ncfile, read=False, read_write=False, write=False)

        This is the base-class object for all netCDF-formatted file
        and file object applications.

Requirements
------------

- netCDF4; https://github.com/Unidata/netcdf4-python

- wxflow; https://github.com/NOAA-EMC/wxflow

Author(s)
---------

    Henry R. Winterbottom; 24 August 2023

History
-------

    2023-08-24: Henry Winterbottom -- Initial implementation.

"""

# ----

# pylint: disable=expression-not-assigned
# pylint: disable=fixme # FOR NOW
# pylint: disable=no-name-in-module

# ----

from typing import Generic

import numpy
from netCDF4 import Dataset
from wxflow.attrdict import AttrDict

# ----

__all__ = ["NCIO"]

# ----


class NCIO:
    """
    Description
    -----------

    This is the base-class object for all netCDF-formatted file and
    file object applications.

    Parameters
    ----------

    ncfile: str

        A Python string specifying the netCDF-formatted file path.

    Keywords
    --------

    read: bool, optional

        A Python boolean valued variable specifying whether the netCDF
        file object is opened for `read` only applications.

    read_write: bool, optional

        A Python boolean valued variable specifying whether the netCDF
        file object is opened for `read` and `write` applications.

    write: bool, optional

        A Python boolean valued variable specifying whether the netCDF
        file object is opened for `write` only applications.

    Raises
    ------

    ## TODO

    """

    def __init__(
        self: Generic,
        ncfile: str,
        read: bool = False,
        read_write: bool = False,
        write: bool = False,
    ):
        """
        Description
        -----------

        Creates a new NCIO object.

        """

        # Define the base-class attributes.
        if read:
            self.ncio_file = Dataset(ncfile, "r")
        elif read_write:
            self.ncio_file = Dataset(ncfile, "rw")
        elif write:
            self.ncio_file = Dataset(ncfile, "w")
        else:
            # TODO: Exception here if neither `read`, `read_write`, or
            # `write` is specified.
            raise AttributeError

        # TODO: Add a logger message here regarding the file action
        # and path.

    def close(self: Generic) -> None:
        """
        Description
        -----------

        This method closes an open netCDF file object.

        """

        # Close the open netCDF file object.
        self.ncio_file.close()

    def create_ncvar(self: Generic, ncvar_dict: AttrDict) -> None:
        """
        Description
        -----------

        This method creates a netCDF variable within the netCDF file
        object; this optionally includes the specified/respective
        variable attributes.

        Parameters
        ----------

        ncvar_dict: AttrDict

            A Python AttrDict containing the netCDF variable
            attributes.

        Raises
        ------

        ## TODO

        """

        # TODO: Raise exception if netCDF file object is not opened in
        # `write` mode.

        # Create the specified netCDF vriable within the netCDF file
        # object.
        ncvar = self.ncio_file.createVariable(
            ncvar_dict.name, numpy.float, ncvar_dict.dims
        )
        if "_FillValue" in ncvar_dict.attrs:
            fill_value = ncvar_dict.attrs.pop("_FillValue")
            ncvar_dict.attrs["fill_value"] = fill_value
        [
            ncvar.setncattr(f"{item}", f"{ncvar_dict.attrs[item]}")
            for item in ncvar_dict.attrs
        ]

    def get_ncdims(self: Generic) -> AttrDict:
        """
        Description
        -----------

        This method collects the netCDF file object dimension
        attributes.

        Returns
        -------

        ncdims_dict: AttrDict

            A Python AttrDict containing the netCDF file object
            dimension attributes.

        """

        # Collect the netCDF file object dimension attributes.
        ncdims_dict = AttrDict()
        for ncdim_key, _ in self.ncio_file.dimensions.items():
            ncdims_dict[ncdim_key] = self.ncio_file.dimensions[ncdim_key].size

        return ncdims_dict

    def read_ncinfo(self: Generic, ncvarname: str) -> AttrDict:
        """
        Description
        -----------

        This method collects the netCDF information and attributes for
        a netCDF variable within the netCDF file object.

        Parameters
        ----------

        ncvarname: str

            A Python string specifying a variable within the netCDF
            file object.

        Returns
        -------

        ncinfo: AttrDict

            A Python AttrDict containing the respective netCDF
            variable information and attributes.

        """

        # Collect the netCDF variable information and attributes.
        ncinfo = AttrDict()
        variable = self.ncio_file.variables[f"{ncvarname}"]
        ncinfo.dims = {dim.name: dim.size for dim in variable.get_dims()}
        ncinfo.attrs = {attr: variable.getncattr(
            attr) for attr in variable.ncattrs()}

        return ncinfo

    def read_ncvar(self: Generic, ncvarname: str) -> numpy.ma.core.MaskedArray:
        """
        Desciption
        ----------

        This method will read and return the contents of a specified
        variable within the netCDF file object.

        Parameters
        ----------

        ncvarname: str

            A Python string specifying a variable within the netCDF
            file object.

        Returns
        -------

        ncvar_array: numpy.ma.core.MaskedArray

            A Python numpy.ma.core.MaskedArray containing the netCDF
            variable array.

        """

        # Read the netCDF variable array contents.
        ncvar_array = self.ncio_file.variables[f"{ncvarname}"][...]

        return ncvar_array

    def write_ncdim(self: Generic, ncdimname: str, ncdimsize: int) -> None:
        """
        Description
        -----------

        This method will write the netCDF dimension attributes to the
        netCDF file object.

        Parameters
        ----------

        ncdimname: str

            A Python string specifying the netCDF dimension name.

        ncdimsize: int

            A Python integer specifying the netCDF dimension size.

        Raises
        ------

        ## TODO

        """

        # TODO: Raise exception if netCDF file object is not opened in
        # `write` mode.

        # Define the netCDF file object dimension.
        self.ncio_file.createDimension(ncdimname, ncdimsize)
