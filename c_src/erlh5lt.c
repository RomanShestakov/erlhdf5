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
#include "hdf5_hl.h"
#include "erl_nif.h"
#include "dbg.h"
#include "erlhdf5.h"


static int unpack_int_list(ErlNifEnv* env, unsigned int list_length, const ERL_NIF_TERM list, int* data);

// creates a new simple dataspace and opens it for access
ERL_NIF_TERM h5lt_make_dataset(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  hid_t file_id;
  char ds_name[MAXBUFLEN];
  herr_t err;
  ERL_NIF_TERM ret;
  const ERL_NIF_TERM *dims;
  int* data;
  int rank, arity;
  hsize_t* dims_arr; // array specifiying the size of each dimension
  ERL_NIF_TERM list;
  unsigned int list_length;
  int i = 0;

  // parse arguments
  check(argc == 5, "Incorrent number of arguments");
  check(enif_get_int(env, argv[0], &file_id ) != 0, "Can't get file id from argv");
  check(enif_get_string(env, argv[1], ds_name, sizeof(ds_name), ERL_NIF_LATIN1) != 0, \
	"Can't get dataset name from argv");
  check(enif_get_int(env, argv[2], &rank ) != 0, "Can't get rank from argv");
  check(enif_get_tuple(env, argv[3], &arity, &dims) != 0, "Can't get dimensions from argv");
  check(enif_get_list_length(env, argv[4], &list_length), "empty data");
  list = argv[4];

  // allocate array of size rank
  dims_arr = (hsize_t*) malloc(arity * sizeof(hsize_t));
  int n;
  for(i = 0; i < arity; i++) {
    check(enif_get_int(env, dims[i], &n), "error getting diskspace dimensions");
    dims_arr[i] = (hsize_t)n;
  }

  // allocate space for array to hold elements of list
  data = malloc(list_length * sizeof(int));
  check(data, "can't allocate mem");

  // convert a list of ints into array
  err = unpack_int_list(env, list_length, list, data);
  check(err == 0, "can't unpack list");

  // make a dataset
  err = H5LTmake_dataset(file_id, ds_name, arity, dims_arr, H5T_NATIVE_INT, data);
  check(err == 0, "Failed to make dataset.");

  // cleanup
  free(dims_arr);
  free(data);
  ret = enif_make_int(env, err);
  return enif_make_tuple2(env, ATOM_OK, ret);

 error:
   if(dims_arr) free(dims_arr);
   if(data) free(data);
  return error_tuple(env, "Can not make dataset");
};


// unpack a list of ints into an array of ints
static int unpack_int_list(ErlNifEnv* env, unsigned int list_length, ERL_NIF_TERM list, int* data)
{
  int i = 0;
  ERL_NIF_TERM head, tail;

  while(enif_get_list_cell(env, list, &head, &tail)) {
    for(i = 0; i < list_length; i++) {
      check(enif_get_int(env, head, &data[i]), "error upacking an element");
    }
    list = tail;
  }
  return 0;

 error:
  return -1;
};

// creates a new simple dataspace and opens it for access
ERL_NIF_TERM h5lt_read_dataset_int(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  hid_t file_id;
  char ds_name[MAXBUFLEN];
  herr_t err;
  int *out_buffer;
  /* ERL_NIF_TERM ret; */

  // parse arguments
  check(argc == 2, "Incorrent number of arguments");
  check(enif_get_int(env, argv[0], &file_id ) != 0, "Can't get file id from argv");
  check(enif_get_string(env, argv[1], ds_name, sizeof(ds_name), ERL_NIF_LATIN1) != 0, \
	"Can't get dataset name from argv");

  // read a dataset
  err = H5LTread_dataset_int(file_id, ds_name, out_buffer);
  check(err > 0, "Failed to read dataset.");

  /* // cleanup */
  /* free(dimsf); */
  /* ret = enif_make_int(env, dataspace_id); */
  //  return enif_make_tuple2(env, ATOM_OK, ret);
  return ATOM_OK;

 error:
  return error_tuple(env, "Can not read dataset");
};

