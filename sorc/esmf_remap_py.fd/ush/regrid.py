
from typing import Generic
from wxflow.attrdict import AttrDict

import numpy
import esmpy


from ncio import NCIO

# ----

__all__ = ["Regrid"]

# ----


class Regrid:
    """

    """

    def __init__(self: Generic, varinfo: AttrDict):
        """
        Description
        -----------

        Creates a new Regrid object.

        """

        # Define the base-class attributes.
        self.varinfo = varinfo

        # TODO: Add logger message regarding netCDF-formatted file
        # path being parsed.

        ncio_obj = NCIO(ncfile=self.varinfo.esmf_coeff_file,
                        read=True)
        esmf_dict = self.read_esmf(ncio_obj=ncio_obj)
        self.remap_matrix = self.build_remap_matrix(esmf_dict=esmf_dict)

    @staticmethod
    def build_remap_matrix(esmf_dict: AttrDict) -> numpy.array:
        """ """

        remap_matrix = numpy.zeros((esmf_dict.n_b, esmf_dict.n_a))
        for dst_idx in range(esmf_dict.n_b):
            start = esmf_dict.row[dst_idx]
            end = esmf_dict.row[dst_idx + 1]
            remap_matrix[dst_idx, esmf_dict.col[start:end]
                         ] = esmf_dict.s[start:end]

        print(remap_matrix)

#        for dst_idx in range(esmf_dict.n_b):
#            start = esmf_dict.row[dst_idx]
#            end = esmf_dict.row[dst_idx + 1]
#            for src_idx in range(esmf_dict.n_a):
#                remap_matrix[dst_idx, esmf_dict.col[src_idx]
#                             ] = esmf_dict.s[src_idx]

        return remap_matrix

    def interp(self: Generic, invar: numpy.array) -> numpy.array:
        """ """

    @ staticmethod
    def read_esmf(ncio_obj: NCIO) -> AttrDict:  # TODO
        """ """

        esmf_dict = AttrDict()
        ncdims_dict = ncio_obj.get_ncdims()
        esmf_dict.n_a = ncdims_dict.n_a
        esmf_dict.n_b = ncdims_dict.n_b
        esmf_dict.row = ncio_obj.read_ncvar(ncvarname="row")
        esmf_dict.col = ncio_obj.read_ncvar(ncvarname="col")
        esmf_dict.s = ncio_obj.read_ncvar(ncvarname="S")
        ncio_obj.close()

        # TODO: Ignore these for now; only required for `conserve`.
        # esmf_dict.frac_a = ncio_obj.read_ncvar(ncvarname="frac_a")
        # esmf_dict.frac_b = ncio_obj.read_ncvar(ncvarname="frac_b")

        return esmf_dict
