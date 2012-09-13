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
static void freeArray(int **a, int length);
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
  Handle* dataspace_res;
  Handle* dcpl_res;
  char ds_name[MAXBUFLEN];
  hid_t file_id;
  hid_t type_id;
  hid_t dataspace_id;
  hid_t ds_id;
  hid_t dcpl_id;

  // parse arguments
  check(argc == 5, "Incorrent number of arguments");
  check(enif_get_int(env, argv[0], &file_id) != 0,	\
	"Can't get file resource from argv");
  check(enif_get_string(env, argv[1], ds_name, sizeof(ds_name), ERL_NIF_LATIN1) != 0, \
	"Can't get dataset name from argv");
  check(enif_get_int(env, argv[2], &type_id) != 0,	\
	"Can't get datatype resource from argv");
  check(enif_get_resource(env, argv[3], RES_TYPE, (void**) &dataspace_res) != 0,	\
	"Can't get dataspace resource from argv");
  check(enif_get_resource(env, argv[4], RES_TYPE, (void**) &dcpl_res) != 0,	\
	"Can't get properties resource from argv");

  dataspace_id = dataspace_res->id;
  dcpl_id = dcpl_res->id;

  // create a new file using default properties
  ds_id = H5Dcreate(file_id, ds_name, type_id, dataspace_id,
		    H5P_DEFAULT, dcpl_id, H5P_DEFAULT);
  check(ds_id > 0, "Failed to create dataset.");

  ret = enif_make_int(env, ds_id);
  return enif_make_tuple2(env, ATOM_OK, ret);

 error:
  if(ds_id) H5Dclose(ds_id);
  return error_tuple(env, "Can not create dataset");
};

// Opens an existing dataset.
ERL_NIF_TERM h5dopen(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  ERL_NIF_TERM ret;
  char ds_name[MAXBUFLEN];
  hid_t file_id;
  hid_t ds_id;

  // parse arguments
  check(argc == 2, "Incorrent number of arguments");
  check(enif_get_int(env, argv[0], &file_id) != 0,	\
	"Can't get file resource from argv");
  check(enif_get_string(env, argv[1], ds_name, sizeof(ds_name), ERL_NIF_LATIN1) != 0, \
	"Can't get dataset name from argv");

  // create a new file using default properties
  ds_id = H5Dopen(file_id, ds_name, H5P_DEFAULT);
  check(ds_id > 0, "Failed to create dataset.");

  ret = enif_make_int(env, ds_id);
  return enif_make_tuple2(env, ATOM_OK, ret);

 error:
  if(ds_id) H5Dclose(ds_id);
  return error_tuple(env, "Can not create dataset");
};


// close
ERL_NIF_TERM h5dclose(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  herr_t err;
  hid_t ds_id;

  // parse arguments
  check(argc == 1, "Incorrent number of arguments");
  check(enif_get_int(env, argv[0], &ds_id) != 0,	\
	"Can't get resource from argv");

  // close properties list
  err = H5Dclose(ds_id);
  check(err == 0, "Failed to close dataset.");
  return ATOM_OK;

 error:
  return error_tuple(env, "Can not close dataset");
};


// Determines whether space has been allocated for a dataset.
ERL_NIF_TERM h5d_get_space_status(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  ERL_NIF_TERM ret;
  hid_t ds_id;
  H5D_space_status_t      space_status;
  char space_status_str[MAXBUFLEN];
  herr_t err;

  // parse arguments
  check(argc == 1, "Incorrent number of arguments");
  check(enif_get_int(env, argv[0], &ds_id) != 0,	\
	"Can't get dataset resource from argv");

  // create a new file using default properties
  err = H5Dget_space_status(ds_id, &space_status);
  check(err == 0, "Failed to get space status.");

  // convert code to string representation
  err = convert_space_status(space_status, space_status_str);
  check(err == 0, "Failed to convert space status %d", space_status);

  // make atom out of string representation
  ret = enif_make_atom(env, space_status_str);
  return enif_make_tuple2(env, ATOM_OK, ret);

 error:
  if(ds_id) H5Dclose(ds_id);
  return error_tuple(env, "Can not get dataspace status");
};


// Returns the amount of storage allocated for a dataset.
ERL_NIF_TERM h5d_get_storage_size(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  ERL_NIF_TERM ret;
  hid_t ds_id;
  hsize_t size;

  // parse arguments
  check(argc == 1, "Incorrent number of arguments");
  check(enif_get_int(env, argv[0], &ds_id) != 0, "Can't get dataset resource from argv");

  size = H5Dget_storage_size(ds_id);
  ret = enif_make_int64(env, size);
  return enif_make_tuple2(env, ATOM_OK, ret);

 error:
  return error_tuple(env, "Can not determine storage size");
};


// Returns an identifier for a copy of the datatype for a dataset.
ERL_NIF_TERM h5dget_type(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  ERL_NIF_TERM ret;
  hid_t ds_id;
  hid_t type_id;

  // parse arguments
  check(argc == 1, "Incorrent number of arguments");
  check(enif_get_int(env, argv[0], &ds_id) != 0, "Can't get dataset resource from argv");

  type_id = H5Dget_type(ds_id);

  ret = enif_make_int(env, type_id);
  return enif_make_tuple2(env, ATOM_OK, ret);

 error:
  return error_tuple(env, "Can not determine storage size");
};


// creates a new simple dataspace and opens it for access
ERL_NIF_TERM h5dwrite(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  /* Handle* dataset_res; */
  hid_t dataset_id;
  herr_t err;
  ERL_NIF_TERM list;
  unsigned int list_length;
  int arity;//, rank;
  int i = 0, j = 0;
  ERL_NIF_TERM head, tail;
  const ERL_NIF_TERM *terms;

  // parse arguments
  check(argc == 2, "Incorrent number of arguments");
  check(enif_get_int(env, argv[0], &dataset_id) != 0,	\
	"Can't get dataset resource from argv");

  list = argv[1];

  // get the dimentions of list: lenght of list and arity of tuples (elements), all elements must be the same size
  check(enif_get_list_length(env, list, &list_length), "empty data");
  // allocate space for array to hold elements of list
  int **data = malloc(list_length * sizeof(int *));
  check(data, "can't allocate mem");
  // go through each list element and unpack it
  while(enif_get_list_cell(env, list, &head, &tail)) {
    check(enif_get_tuple(env, head, &arity, &terms) != 0, "Can't get elements from the list");
    data[i] = malloc(arity * sizeof(int));
    check(data[i], "can't allocate mem");
    for(j = 0; j < arity; j++) {
      check(enif_get_int(env, terms[j], &data[i][j]), "error upacking an element");
    }
    i++;
    list = tail;
  }

  //dataset_id = dataset_res->id;
  check(dataset_id, "Failed to get dataset handler");

  // write the data to the dataset
  err = H5Dwrite(dataset_id, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, data);
  check(err == 0, "Failed to write into dataset.");

  // free mem
  freeArray(data, list_length);
  return ATOM_OK;

 error:
  if(data) freeArray(data, list_length);
  return error_tuple(env, "Can not write into dataset");
};

// free mem
static void freeArray(int **a, int length) {
  int i;
  for(i = 0; i < length; i++) {
    free(a[i]);
  }
  free(a);
};


// Returns an identifier for a copy of the dataspace for a dataset.
ERL_NIF_TERM h5dget_space(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  ERL_NIF_TERM ret;
  hid_t space_id, dataset_id;

  // parse arguments
  check(argc == 1, "Incorrent number of arguments");
  check(enif_get_int(env, argv[0], &dataset_id) != 0,	\
	"Can't get dataset resource from argv");

  space_id = H5Dget_space(dataset_id);
  check(space_id >= 0, "Failed to get space.");

  ret = enif_make_int(env, space_id);
  return enif_make_tuple2(env, ATOM_OK, ret);

 error:
  return error_tuple(env, "Can not get space id");
};


/* //#define H5FILE_NAME        "SDS.h5" */
/* #define DATASETNAME "C Matrix" */
/* /\* #define NX     3                      /\\* dataset dimensions *\\/ *\/ */
/* /\* #define NY     5 *\/ */
/* #define RANK   2 */


/* // creates a new simple dataspayce and opens it for access */
/* ERL_NIF_TERM h5dwrite_example(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) */
/* { */

/*   hid_t       file, dataset;         /\* file and dataset identifiers *\/ */
/*   hid_t       /\*datatype,*\/ dataspace;   /\* identifiers *\/ */
/*      /\* hsize_t     dims[2];               /\\* dataset dimensions *\\/ *\/ */
/*      herr_t      status; */
/*      int         data[NX][NY];          /\* data to write *\/ */
/*      int         i, j; */

/*   /\* ERL_NIF_TERM ret; *\/ */
/*   /\* Handle* res; *\/ */
/*   Handle* file_res; */
/*   Handle* dataspace_res; */
/*   /\* Handle* dtype_res; *\/ */
/*   /\* Handle* dcpl_res; *\/ */
/*   /\* char ds_name[MAXBUFLEN]; *\/ */
/*   //hid_t file; */
/*   // hid_t type_id; */
/*   //  hid_t dataspace; */
/*   //hid_t ds_id; */
/*   //hid_t dcpl_id; */



/*      /\* */
/*       * Data  and output buffer initialization. */
/*       *\/ */
/*      for (j = 0; j < NX; j++) { */
/*         for (i = 0; i < NY; i++) */
/*             data[j][i] = i + 1 + j*NY; */
/*      } */
/*      /\* */
/*       *  1  2  3  4  5 */
/*       *  6  7  8  9 10 */
/*       * 11 12 13 14 15 */
/*       *\/ */

/*   check(argc == 2, "Incorrent number of arguments"); */
/*   check(enif_get_resource(env, argv[0], RES_TYPE, (void**) &file_res) != 0,	\ */
/* 	"Can't get file resource from argv"); */

/*   check(enif_get_resource(env, argv[1], RES_TYPE, (void**) &dataspace_res) != 0,	\ */
/* 	"Can't get dataspace resource from argv"); */


/*   file = file_res->id; */
/*   dataspace = dataspace_res->id; */
/*   /\* type_id = dtype_res->id; *\/ */
/*   /\* dcpl_id = dcpl_res->id; *\/ */


/*      /\* */
/*       * Create a new file using H5F_ACC_TRUNC access, */
/*       * default file creation properties, and default file */
/*       * access properties. */
/*       *\/ */
/*      //file = H5Fcreate(H5FILE_NAME, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT); */

/*      /\* */
/*       * Describe the size of the array and create the data space for fixed */
/*       * size dataset. */
/*       *\/ */
/*      /\* dims[0] = NX; *\/ */
/*      /\* dims[1] = NY; *\/ */
/*      /\* dataspace = H5Screate_simple(RANK, dims, NULL); *\/ */

/*      /\* */
/*       * Create a new dataset within the file using defined dataspace and */
/*       * datatype and default dataset creation properties. */
/*       *\/ */
/*      dataset = H5Dcreate(file, DATASETNAME, H5T_NATIVE_INT, dataspace, */
/*                         H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT); */

/*      /\* */
/*       * Write the data to the dataset using default transfer properties. */
/*       *\/ */
/*      status = H5Dwrite(dataset, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, */
/*                       H5P_DEFAULT, data); */

/*      /\* */
/*       * Close/release resources. */
/*       *\/ */
/*      H5Sclose(dataspace); */
/*      H5Dclose(dataset); */
/*      H5Fclose(file); */

/*      //return 0; */
/*      return ATOM_OK; */

/*  error: */
/*   //if(ds_id) H5Dclose(ds_id); */

/*   return error_tuple(env, "Can not write into dataset"); */

/* }; */
