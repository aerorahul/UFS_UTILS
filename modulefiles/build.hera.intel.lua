help([[ 
Load environment to compile UFS_UTILS on NOAA CSPs using Intel
]])

-- prepend_path("MODULEPATH", "/scratch1/NCEPDEV/da/Henry.Winterbottom/UFS/spack-stack/envs/ufs_utils_stack/install/modulefiles/Core")

cmake_ver=os.getenv("cmake_ver") or "3.23.1"
load(pathJoin("cmake", cmake_ver))

hpc_intel_ver=os.getenv("hpc_intel_ver") or "2022.1.2"
load(pathJoin("intel", hpc_intel_ver))

impi_ver=os.getenv("impi_ver") or "2022.1.2"
load(pathJoin("impi", impi_ver))

bacio_ver=os.getenv("bacio_ver") or "2.4.1"
load(pathJoin("bacio", bacio_ver))

g2_ver=os.getenv("g2_ver") or "3.4.5"
load(pathJoin("g2", g2_ver))

ip_ver=os.getenv("ip_ver") or "4.0.0"
load(pathJoin("ip", ip_ver))

nemsio_ver=os.getenv("nemsio_ver") or "2.5.4"
load(pathJoin("nemsio", nemsio_ver))

sp_ver=os.getenv("sp_ver") or "2.3.3"
load(pathJoin("sp", sp_ver))

w3emc_ver=os.getenv("w3emc_ver") or "2.9.2"
load(pathJoin("w3emc", w3emc_ver))

sfcio_ver=os.getenv("sfcio_ver") or "1.4.1"
load(pathJoin("sfcio", sfcio_ver))

sigio_ver=os.getenv("sigio_ver") or "2.3.2"
load(pathJoin("sigio", sigio_ver))

zlib_ver=os.getenv("zlib_ver") or "1.2.11"
load(pathJoin("zlib", zlib_ver))

png_ver=os.getenv("png_ver") or "1.6.37"
load(pathJoin("libpng", png_ver))

hdf5_ver=os.getenv("hdf5_ver") or "1.10.6"
load(pathJoin("hdf5", hdf5_ver))

netcdf_ver=os.getenv("netcdf_ver") or "4.7.4"
load(pathJoin("netcdf-c", netcdf_ver))
-- load(pathJoin("netcdf-fortran", "4.5.3")) 

-- nccmp_ver=os.getenv("nccmp_ver") or "1.8.9.0"
-- load(pathJoin("nccmp", nccmp_ver))

esmf_ver=os.getenv("esmf_ver") or "8.4.2"
load(pathJoin("esmf", esmf_ver))

-- nco_ver=os.getenv("nco_ver") or "4.9.1"
-- load(pathJoin("nco", nco_ver))

ncio_ver=os.getenv("ncio_ver") or "1.1.2"
load(pathJoin("ncio", ncio_ver))

whatis("Description: UFS_UTILS build environment")
