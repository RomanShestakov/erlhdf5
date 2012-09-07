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

// H5P: Property List Interface
static int convert_property_flag(char* file_flags, unsigned *flags);

// convert
static int convert_property_flag(char* file_flags, unsigned *flags)
{
  if(strncmp(file_flags, "H5P_OBJECT_CREATE", MAXBUFLEN) == 0)
    *flags = H5P_OBJECT_CREATE;
  else if(strncmp(file_flags, "H5P_FILE_CREATE", MAXBUFLEN) == 0)
    *flags = H5P_FILE_CREATE;
  else if(strncmp(file_flags, "H5P_FILE_ACCESS", MAXBUFLEN) == 0)
    *flags = H5P_FILE_ACCESS;
  else if(strncmp(file_flags, "H5P_DATASET_CREATE", MAXBUFLEN) == 0)
    *flags = H5P_DATASET_CREATE;
  else if(strncmp(file_flags, "H5P_DATASET_ACCESS", MAXBUFLEN) == 0)
    *flags = H5P_DATASET_ACCESS;
  else if(strncmp(file_flags, "H5P_DATASET_XFER", MAXBUFLEN) == 0)
    *flags = H5P_DATASET_XFER;
  else if(strncmp(file_flags, "H5P_FILE_MOUNT", MAXBUFLEN) == 0)
    *flags = H5P_FILE_MOUNT;
  else if(strncmp(file_flags, "H5P_GROUP_CREATE", MAXBUFLEN) == 0)
    *flags = H5P_GROUP_CREATE;
  else if(strncmp(file_flags, "H5P_GROUP_ACCESS", MAXBUFLEN) == 0)
    *flags = H5P_GROUP_ACCESS;
  else if(strncmp(file_flags, "H5P_DATATYPE_CREATE", MAXBUFLEN) == 0)
    *flags = H5P_DATATYPE_CREATE;
  else if(strncmp(file_flags, "H5P_DATATYPE_ACCESS", MAXBUFLEN) == 0)
    *flags = H5P_DATATYPE_ACCESS;
  else if(strncmp(file_flags, "H5P_STRING_CREATE", MAXBUFLEN) == 0)
    *flags = H5P_STRING_CREATE;
  else if(strncmp(file_flags, "H5P_ATTRIBUTE_CREATE", MAXBUFLEN) == 0)
    *flags = H5P_ATTRIBUTE_CREATE;
  else if(strncmp(file_flags, "H5P_OBJECT_COPY", MAXBUFLEN) == 0)
    *flags = H5P_OBJECT_COPY;
  else if(strncmp(file_flags, "H5P_LINK_CREATE", MAXBUFLEN) == 0)
    *flags = H5P_LINK_CREATE;
  else if(strncmp(file_flags, "H5P_LINK_ACCESS", MAXBUFLEN) == 0)
    *flags = H5P_LINK_ACCESS;
  else
    sentinel("Unknown properties flag %s", file_flags);

  return 0;

 error:
  return -1;
};


// Creates a new property list as an instance of a property list class.
ERL_NIF_TERM h5pcreate(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  hid_t dcpl_id; // dataset creation property list
  Handle* res;
  ERL_NIF_TERM ret;
  unsigned cls_id;
  char cls[MAXBUFLEN];

  // parse arguments
  check(argc == 1, "Incorrent number of arguments");
  check(enif_get_atom(env, argv[0], cls, sizeof(cls), ERL_NIF_LATIN1) != 0, \
	"Can't get class id from argv");

  // convert access flag to format which hdf5 api understand
  check(convert_property_flag(cls, &cls_id) == 0, "Failed to convert access flag");

  // create a new file using default properties
  dcpl_id = H5Pcreate(cls_id);
  check(dcpl_id > 0, "Failed to create property list.");

  // create a resource to pass reference to id back to erlang
  res = enif_alloc_resource(RES_TYPE, sizeof(Handle));
  check(res, "Failed to allocate resource for type %s", "Handle");

  // add ref to resource
  res->id = dcpl_id;
  ret = enif_make_resource(env, res);

  // cleanup
  enif_release_resource(res);

  return enif_make_tuple2(env, ATOM_OK, ret);

 error:
  if(dcpl_id) H5Pclose(dcpl_id);

  return error_tuple(env, "Can not create properties list");
};

// close
ERL_NIF_TERM h5pclose(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  Handle* res;
  herr_t err;

  // parse arguments
  check(argc == 1, "Incorrent number of arguments");
  check(enif_get_resource(env, argv[0], RES_TYPE, (void**) &res) != 0,	\
	"Can't get resource from argv");

  // close properties list
  err = H5Pclose(res->id);
  check(err == 0, "Failed to close properties list.");

  return ATOM_OK;

 error:
  return error_tuple(env, "Can not close properties list");
};
