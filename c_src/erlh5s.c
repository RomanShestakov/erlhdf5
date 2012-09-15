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

// H5S: Dataspace Interface
static int convert_array_to_nif_array(ErlNifEnv* env, hsize_t size, hsize_t *arr_from, ERL_NIF_TERM* arr_to);

// creates a new simple dataspace and opens it for access
ERL_NIF_TERM h5screate_simple(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  hid_t dataspace_id;
  ERL_NIF_TERM ret;
  const ERL_NIF_TERM *terms;
  int rank; // number of dimensions of dataspace
  hsize_t* dimsf; // array specifiying the size of each dimension
  int i;
  int arity;

  // parse arguments
  check(argc == 2, "Incorrent number of arguments");
  check(enif_get_int(env, argv[0], &rank ) != 0, "Can't get rank from argv");
  check(enif_get_tuple(env, argv[1], &arity, &terms) != 0, "Can't get terms from argv");
  // make sure that rank is matching arity
  check(rank <= 2, "does not support > 2 dimensions");

  // allocate array of size rank
  dimsf = (hsize_t*) malloc(arity * sizeof(hsize_t));
  int n;
  for(i = 0; i < arity; i++) {
    check(enif_get_int(env, terms[i], &n), "error getting diskspace dimensions");
    dimsf[i] = (hsize_t)n;
  }

  // create a new file using default properties
  dataspace_id = H5Screate_simple(rank, dimsf, NULL);
  check(dataspace_id > 0, "Failed to create dataspace.");

  // cleanup
  free(dimsf);
  ret = enif_make_int(env, dataspace_id);
  return enif_make_tuple2(env, ATOM_OK, ret);

 error:
  if(dataspace_id) H5Sclose(dataspace_id);
  if(dimsf) free(dimsf);
  return error_tuple(env, "Can not create dataspace");
};


// close
ERL_NIF_TERM h5sclose(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  herr_t err;
  hid_t dataspace_id;

  // parse arguments
  check(argc == 1, "Incorrent number of arguments");
  check(enif_get_int(env, argv[0], &dataspace_id) != 0,	"Can't get resource from argv");

  // close properties list
  err = H5Sclose(dataspace_id);
  check(err == 0, "Failed to close dataspace.");
  return ATOM_OK;

 error:
  return error_tuple(env, "Can not close dataspace");
};


// Determines the dimensionality of a dataspace.
ERL_NIF_TERM h5sget_simple_extent_ndims(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  hid_t dataspace_id;
  int ndims;

  // parse arguments
  check(argc == 1, "Incorrent number of arguments");
  check(enif_get_int(env, argv[0], &dataspace_id) != 0,	"Can't get resource from argv");

  ndims = H5Sget_simple_extent_ndims(dataspace_id);
  check(ndims > 0, "Failed to determine dataspace dimensions.");
  return enif_make_tuple2(env, ATOM_OK, enif_make_int(env, ndims));

 error:
  return error_tuple(env, "Failed to determine dataspace dimensions");
};


// Retrieves dataspace dimension size and maximum size.
ERL_NIF_TERM h5sget_simple_extent_dims(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  hid_t dataspace_id;
  hsize_t *dims = NULL;
  hsize_t *maxdims = NULL;
  int status;
  ERL_NIF_TERM dims_list;
  ERL_NIF_TERM maxdims_list;
  ERL_NIF_TERM* dims_arr;
  ERL_NIF_TERM* maxdims_arr;
  int err;
  int rank;

  // parse arguments
  check(argc == 2, "Incorrent number of arguments");
  check(enif_get_int(env, argv[0], &dataspace_id) != 0,	"Can't get resource from argv");
  check(enif_get_int(env, argv[1], &rank) != 0,	"Can't get rank from argv");

  // allocate space for dims array to store a number of dimensions
  dims = malloc(rank * sizeof(hsize_t));
  maxdims = malloc(rank * sizeof(hsize_t));

  // get a number of dims from dataspace
  status = H5Sget_simple_extent_dims(dataspace_id, dims, maxdims);
  check(status > 0, "Failed to get dims.");

  // allocate mem for arrays of ERL_NIF_TERM so we could convert
  dims_arr = (ERL_NIF_TERM*)enif_alloc(sizeof(ERL_NIF_TERM) * rank);
  maxdims_arr = (ERL_NIF_TERM*)enif_alloc(sizeof(ERL_NIF_TERM) * rank);

  // convert arrays into array of ERL_NIF_TERM
  err = convert_array_to_nif_array(env, rank, dims, dims_arr);
  err = convert_array_to_nif_array(env, rank, maxdims, maxdims_arr);

   // convert arrays to list
  dims_list = enif_make_list_from_array(env, dims_arr, rank);
  maxdims_list = enif_make_list_from_array(env, maxdims_arr, rank);

   // cleanup
  free(dims);
  free(maxdims);
  return enif_make_tuple3(env, ATOM_OK, dims_list, maxdims_list);

 error:
  if(dims) free(dims);
  if(maxdims) free(maxdims);
  return error_tuple(env, "Can not get dims");
};


static int convert_array_to_nif_array(ErlNifEnv* env, hsize_t size, hsize_t *arr_from, ERL_NIF_TERM* arr_to)
{
  int i;
  for(i = 0; i < size; i++) {
    arr_to[i] = enif_make_int(env, arr_from[i]);
  }
  return 0;
};
