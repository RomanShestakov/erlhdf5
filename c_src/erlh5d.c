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

//  H5D: Datasets Interface


// protype
static herr_t convert_space_status(H5D_space_status_t,  char*);

// convert
static herr_t convert_space_status(H5D_space_status_t space_status,  char* space_status_str)
{
  if(space_status == H5D_SPACE_STATUS_ALLOCATED)
    strcpy(space_status_str, "H5D_SPACE_STATUS_ALLOCATED");
  else if(space_status == H5D_SPACE_STATUS_NOT_ALLOCATED)
    strcpy(space_status_str, "H5D_SPACE_STATUS_NOT_ALLOCATED");
  else if(space_status == H5D_SPACE_STATUS_PART_ALLOCATED)
    strcpy(space_status_str, "H5D_SPACE_STATUS_PART_ALLOCATED");
  else
    sentinel("Unknown status %d", space_status);

  return 0;

 error:
  return -1;
};


// creates a new simple dataspace and opens it for access
ERL_NIF_TERM h5dcreate(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  ERL_NIF_TERM ret;
  Handle* res;
  Handle* file_res;
  Handle* dataspace_res;
  Handle* dtype_res;
  Handle* dcpl_res;
  char ds_name[MAXBUFLEN];
  //char dtype[MAXBUFLEN];
  hid_t file_id;
  hid_t type_id;
  hid_t dataspace_id;
  hid_t ds_id;
  hid_t dcpl_id;

  // parse arguments
  check(argc == 5, "Incorrent number of arguments");
  check(enif_get_resource(env, argv[0], RES_TYPE, (void**) &file_res) != 0,	\
	"Can't get file resource from argv");
  check(enif_get_string(env, argv[1], ds_name, sizeof(ds_name), ERL_NIF_LATIN1) != 0, \
	"Can't get dataset name from argv");
  check(enif_get_resource(env, argv[2], RES_TYPE, (void**) &dtype_res) != 0,	\
	"Can't get datatype resource from argv");
  check(enif_get_resource(env, argv[3], RES_TYPE, (void**) &dataspace_res) != 0,	\
	"Can't get dataspace resource from argv");
  check(enif_get_resource(env, argv[4], RES_TYPE, (void**) &dcpl_res) != 0,	\
	"Can't get properties resource from argv");

  file_id = file_res->id;
  dataspace_id = dataspace_res->id;
  type_id = dtype_res->id;
  dcpl_id = dcpl_res->id;

  // create a new file using default properties
  ds_id = H5Dcreate(file_id, ds_name, type_id, dataspace_id, H5P_DEFAULT, dcpl_id, H5P_DEFAULT);
  check(ds_id > 0, "Failed to create dataset.");

  // create a resource to pass reference to id back to erlang
  res = enif_alloc_resource(RES_TYPE, sizeof(Handle));
  check(res, "Failed to allocate resource for dataset %s", "Handle");

  // add ref to resource
  res->id = ds_id;
  ret = enif_make_resource(env, res);

  // cleanup
  enif_release_resource(res);

  return enif_make_tuple2(env, ATOM_OK, ret);

 error:
  if(ds_id) H5Dclose(ds_id);

  return error_tuple(env, "Can not create dataset");
};

// close
ERL_NIF_TERM h5dclose(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  Handle* res;
  herr_t err;

  // parse arguments
  check(argc == 1, "Incorrent number of arguments");
  check(enif_get_resource(env, argv[0], RES_TYPE, (void**) &res) != 0,	\
	"Can't get resource from argv");

  // close properties list
  err = H5Dclose(res->id);
  check(err == 0, "Failed to close dataset.");

  return ATOM_OK;

 error:
  return error_tuple(env, "Can not close dataset");
};


// Determines whether space has been allocated for a dataset.
ERL_NIF_TERM h5d_get_space_status(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  ERL_NIF_TERM ret;
  Handle* dataset_res;
  hid_t ds_id;
  H5D_space_status_t      space_status;
  char space_status_str[MAXBUFLEN];
  herr_t err;

  // parse arguments
  check(argc == 1, "Incorrent number of arguments");
  check(enif_get_resource(env, argv[0], RES_TYPE, (void**) &dataset_res) != 0,	\
	"Can't get dataset resource from argv");

  ds_id = dataset_res->id;

  // create a new file using default properties
  err = H5Dget_space_status(ds_id, &space_status);
  check(err == 0, "Failed to get space status.");

  err = convert_space_status(space_status, space_status_str);
  check(err == 0, "Failed to convert space status %d", space_status);

  //printf( "status string %s \n",  space_status_str);

  ret = enif_make_atom(env, space_status_str);

  return enif_make_tuple2(env, ATOM_OK, ret);

 error:
  if(ds_id) H5Dclose(ds_id);

  return error_tuple(env, "Can not create dataset");
};

