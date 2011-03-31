2011-03-31

Please note the date at the top of the file; these instructions are
likely to become stale!

* ncdf4

Streamer currently Depends: on ncdf4, which has system dependencies on
NetCDF compiled with HDF5 support, which in turn has a dependency on a
current zlib. The steps I took to install ncdf4 on SuSE11.3 are:

** Download dependency and ncdf4 source code

Choose a convenient directory. I chose /home/mtmorgan/src.
Download 

zlib: http://zlib.net/zlib-1.2.5.tar.gz
hdf5: http://www.hdfgroup.org/ftp/HDF5/current/src/
NetCDF: http://www.unidata.ucar.edu/downloads/netcdf/ftp/netcdf-4.1.2.tar.gz
ncdf4: e.g., http://cran.fhcrc.org/web/packages/ncdf4/index.html

** Build and install

I build each package in the source directory, then installed them in
the directory tree /home/mtmorgan/bin. I chose to install them into
this tree to avoid conflicts with system variants, and because the
software makes assumptions about folder structure that are most easily
satisfied by doing as generic an installation as possible. Adjusting
these...

export SRCDIR=/home/mtmorgan/src
export PREFIX=/home/mtmorgan/bin

should make the code below cut-and-pasteable

*** zlib

cd ${SRCDIR}/zlib-1.2.5
CFLAGS=-fPIC ./configure --prefix=${PREFIX}/zlib-1.2.5
make install

*** hdf5

cd ${SRCDIR}/hdf4
CFLAGS=-fPIC ./configure --with-zlib=${PREFIX}/zlib-1.2.5 \
    --prefix=${PREFIX}/hdf5-1.8.6
make install

*** netcdf

cd ${SRCDIR}/netcdf-4.1.2
./configure --with-hdf5=${PREFIX}/hdf5-1.8.6 \
    --enable-netcdf4 --prefix=${PREFIX}/netcdf-4.1.2
make install

*** ncdf4

cd ${SRCDIR}/ncdf4
LD_LIBRARY_PATH=${PREFIX}/netcdf-4.1.2/lib \
R CMD INSTALL \
    --configure-args="--with-nc-config=${PREFIX}/netcdf-4.1.2/bin/nc-config" \
    ncdf4_1.0.tar.gz

