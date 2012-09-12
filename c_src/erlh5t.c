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

// H5T: Datatype Interface

// protype
static int convert_type(char*,  hid_t*);

// convert
static int convert_type(char* dtype,  hid_t* dtype_id)
{
  if(strncmp(dtype, "H5T_NATIVE_INT", MAXBUFLEN) == 0)
    *dtype_id = H5T_NATIVE_INT;
  /* else if(strncmp(file_flags, "H5P_LINK_ACCESS", MAXBUFLEN) == 0) */
  /*   *flags = H5P_LINK_ACCESS; */
  else
    sentinel("Unknown type %s", dtype);

  return 0;

 error:
  return -1;
};


// Creates a new property list as an instance of a property list class.
ERL_NIF_TERM h5tcopy(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  hid_t dtype_id; // dataset creation property list
  hid_t type_id;
  Handle* res;
  ERL_NIF_TERM ret;
  //unsigned cls_id;
  char type[MAXBUFLEN];

  // parse arguments
  check(argc == 1, "Incorrent number of arguments");
  check(enif_get_atom(env, argv[0], type, sizeof(type), ERL_NIF_LATIN1) != 0, \
	"Can't get type from argv");

  // convert type to format which hdf5 api understand
  check(convert_type(type, &dtype_id) == 0, "Failed to convert type");

  type_id = H5Tcopy(dtype_id);
  check(type_id > 0, "Failed to create type.");

  // create a resource to pass reference to id back to erlang
  res = enif_alloc_resource(RES_TYPE, sizeof(Handle));
  check(res, "Failed to allocate resource for type %s", "Handle");

  // add ref to resource
  res->id = type_id;
  ret = enif_make_resource(env, res);

  // cleanup
  enif_release_resource(res);
  return enif_make_tuple2(env, ATOM_OK, ret);

 error:
  if(type_id) H5Tclose(type_id);
  return error_tuple(env, "Can not copy type");
};


// close
ERL_NIF_TERM h5tclose(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  Handle* res;
  herr_t err;

  // parse arguments
  check(argc == 1, "Incorrent number of arguments");
  check(enif_get_resource(env, argv[0], RES_TYPE, (void**) &res) != 0,	\
	"Can't get resource from argv");

  // close properties list
  err = H5Tclose(res->id);
  check(err == 0, "Failed to close type.");
  return ATOM_OK;

 error:
  return error_tuple(env, "Can not close type");
};

// Returns the datatype class identifier.
ERL_NIF_TERM h5tget_class(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  Handle* res;
  ERL_NIF_TERM ret;
  H5T_class_t class_id;

  // parse arguments
  check(argc == 1, "Incorrent number of arguments");
  check(enif_get_resource(env, argv[0], RES_TYPE, (void**) &res) != 0,	\
	"Can't get resource from argv");

  class_id = H5Tget_class(res->id);
  //fprintf(stderr, "class type: %d\r\n", class_id);
  check(class_id != H5T_NO_CLASS, "Failed to get type class.");

  ret = enif_make_int(env, class_id);
  return enif_make_tuple2(env, ATOM_OK, ret);

 error:
  return error_tuple(env, "Can not get type class");
};
