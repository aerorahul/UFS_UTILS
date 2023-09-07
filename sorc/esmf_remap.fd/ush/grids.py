# =========================================================================
# File: esmf_remap_py.fd/ush/grids.py
# Author: Henry R. Winterbottom
# Date: 24 August 2023
# Version: 0.0.1
# License: LGPL v2.1
# =========================================================================

"""
Module
------

    grids.py

Description
-----------

    This module contains the base-class for all destination grid
    configuration/format types.

Classes
-------

    Grids(gridinfo_dict)

        This is the base-class object for all destination grid
        configuration and format types.

Requirements
------------

- wxflow; https://github.com/NOAA-EMC/wxflow

Author(s)
---------

    Henry R. Winterbottom; 24 August 2023

History
-------

    2023-08-24: Henry Winterbottom -- Initial implementation.

"""

# ----

# pylint: disable=too-few-public-methods

# ----

from typing import Generic

import numpy
from wxflow.attrdict import AttrDict

from ncio import NCIO

# ----

__all__ = ["Grids"]

# ----


class Grids:
    """
    Description
    -----------

    This is the base-class object for all destination grid
    configuration and format types.

    Parameters
    ----------

    gridinfo_dict: AttrDict

        A Python AttrDict containing the grid-type attributes; this is
        collected/defined from/by the `grid` attributes within the
        experiment configuration.

    Notes
    -----

    - Upon success of the constructor, the entirety of the grid
      attributes if contained within the AttrDict `grid`.

    """

    def __init__(self: Generic, gridinfo_dict: AttrDict):
        """
        Description
        -----------

        Creates a new Grids object.

        """

        # Define the base-class attributes.
        self.ncio_obj = NCIO(ncfile=gridinfo_dict.ncfile, read=True)
        if gridinfo_dict.scrip:
            self.grid = self.scrip()
        self.ncio_obj.close()

    def scrip(self: Generic) -> AttrDict:
        """
        Description
        -----------

        This function reads a SCRIP formatted file and returns a
        Python dictionary containing the latitude and longitude
        coordinate arrays.

        Parameters
        ----------

        ncfile: str

            A Python string specifying the netCDF-formatted file path
            for the SCRIP formatted destination grid attributes.

        Returns
        -------

        grid_dict: AttrDict

            A Python AttrDict containing the destination grid
            attributes.

        """

        # Define the grid attributes assuming the SCRIP convention.
        grid_dict = AttrDict()
        dims = self.ncio_obj.read_ncvar(ncvarname="grid_dims")
        arr_shape = (dims[1], dims[0])
        grid_dict.lats = numpy.reshape(
            self.ncio_obj.read_ncvar(ncvarname="grid_center_lat"), arr_shape
        )
        grid_dict.lons = numpy.reshape(
            self.ncio_obj.read_ncvar(ncvarname="grid_center_lon"), arr_shape
        )

        return grid_dict
