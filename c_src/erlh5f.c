/* Copyright (C) 2012 Roman Shestakov */

/* This file is part of erlhdf5 */

/* elrhdf5 is free software: you can redistribute it and/or modify */
/* it under the terms of the GNU Lesser General Public License as */
/* published by the Free Software Foundation, either version 3 of */
/* the License, or (at your option) any later version. */

/* elrhdf5 is distributed in the hope that it will be useful, */
/* but WITHOUT ANY WARRANTY; without even the implied warranty of */
/* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the */
/* GNU Lesser General Public License for more details. */

/* You should have received a copy of the GNU Lesser General Public */
/* License along with Erlsom.  If not, see */
/* <http://www.gnu.org/licenses/>. */

/* Author contact: romanshestakov@yahoo.co.uk */

#include <stdio.h>
#include <stdlib.h>
#include "hdf5.h"
#include "erl_nif.h"
#include "dbg.h"
#include "erlhdf5.h"

// H5F: File interface

// prototype
static int convert_access_flag(char* file_flags, unsigned *flags);

// convert
static int convert_access_flag(char* file_flags, unsigned *flags)
{
  if(strncmp(file_flags, "H5F_ACC_TRUNC", MAXBUFLEN) == 0)
    *flags = H5F_ACC_TRUNC;
  else if(strncmp(file_flags, "H5F_ACC_EXCL", MAXBUFLEN) == 0)
    *flags = H5F_ACC_EXCL;
  else if(strncmp(file_flags, "H5F_ACC_RDWR", MAXBUFLEN) == 0)
    *flags = H5F_ACC_RDWR;
  else if(strncmp(file_flags, "H5F_ACC_RDONLY", MAXBUFLEN) == 0)
    *flags = H5F_ACC_RDONLY;
  else
    sentinel("Unknown file access flag %s", file_flags);
  return 0;

 error:
  return -1;
};


// create file
ERL_NIF_TERM h5fcreate(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  hid_t file_id;
  ERL_NIF_TERM ret;
  char file_name[MAXBUFLEN];
  char file_access_flags[MAXBUFLEN];
  unsigned flags;

  // parse arguments
  check(argc == 2, "Incorrent number of arguments");
  check(enif_get_string(env, argv[0], file_name, sizeof(file_name), ERL_NIF_LATIN1), "Can't get file name from argv");
  check(enif_get_atom(env, argv[1], file_access_flags, sizeof(file_access_flags), ERL_NIF_LATIN1), \
	"Can't get file_access_flag from argv");

  // convert access flag to format which hdf5 api understand
  check(!convert_access_flag(file_access_flags, &flags), "Failed to convert access flag");

  // create a new file using default properties
  file_id = H5Fcreate(file_name, flags, H5P_DEFAULT, H5P_DEFAULT);
  check(file_id > 0, "Failed to create %s.", file_name);

  ret = enif_make_int(env, file_id);
  return enif_make_tuple2(env, ATOM_OK, ret);

 error:
  if(file_id) H5Fclose (file_id);
  return error_tuple(env, "Can not create file");
};

// open file
ERL_NIF_TERM h5fopen(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  hid_t file_id;
  ERL_NIF_TERM ret;
  char file_name[MAXBUFLEN];
  char file_access_flags[MAXBUFLEN];
  unsigned flags;

  // parse arguments
  check(argc == 2, "Incorrent number of arguments");
  check(enif_get_string(env, argv[0], file_name, sizeof(file_name), ERL_NIF_LATIN1), \
	"Can't get file name from argv");
  check(enif_get_atom(env, argv[1], file_access_flags, sizeof(file_access_flags), ERL_NIF_LATIN1), \
	"Can't get file_access_flag from argv");

  // convert access flag to format which hdf5 api understand
  check(!convert_access_flag(file_access_flags, &flags), "Failed to convert access flag");

  // create a new file using default properties
  file_id = H5Fopen(file_name, flags, H5P_DEFAULT);
  check(file_id > 0, "Failed to open %s.", file_name);

  ret = enif_make_int(env, file_id);
  return enif_make_tuple2(env, ATOM_OK, ret);

 error:
  if(file_id) H5Fclose (file_id);
  return error_tuple(env, "Can not open file");
};


// open file
ERL_NIF_TERM h5fclose(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  hid_t file_id;

  // parse arguments
  check(argc == 1, "Incorrent number of arguments");
  check(enif_get_int(env, argv[0], &file_id), "Can't get resource from argv");

  // close file
  check(!H5Fclose(file_id), "Failed to close file.");
  return ATOM_OK;

 error:
  return error_tuple(env, "Can not close file");
};
