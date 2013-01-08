Erlhdf5
============
[![Build Status](https://secure.travis-ci.org/bkearns/erlhdf5.png)](http://travis-ci.org/bkearns/erlhdf5)

This is the binding from erlang to HDF5 and is fork of RomanShestakov/erlhdf5's original.

Library's
-----
This downloads and compiles the latest version of code from http://www.hdfgroup.org/ftp/HDF5/current/src/ 
This creates a shared object in deps/hdf5/lib/libhdf5.so that needs to be placed into the library path.

Notes:
-----
Compiling of the base library fails on Arm.  This should work against the Debian ARM package installed with:

sudo apt-get install libhdf5-serial-dev hdf5-tools

