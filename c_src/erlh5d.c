/* /\* Copyright (C) 2012 Roman Shestakov *\/ */

/* /\* This file is part of erlhdf5 *\/ */

/* /\* elrhdf5 is free software: you can redistribute it and/or modify *\/ */
/* /\* it under the terms of the GNU Lesser General Public License as *\/ */
/* /\* published by the Free Software Foundation, either version 3 of *\/ */
/* /\* the License, or (at your option) any later version. *\/ */

/* /\* elrhdf5 is distributed in the hope that it will be useful, *\/ */
/* /\* but WITHOUT ANY WARRANTY; without even the implied warranty of *\/ */
/* /\* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the *\/ */
/* /\* GNU Lesser General Public License for more details. *\/ */

/* /\* You should have received a copy of the GNU Lesser General Public *\/ */
/* /\* License along with Erlsom.  If not, see *\/ */
/* /\* <http://www.gnu.org/licenses/>. *\/ */

/* /\* Author contact: romanshestakov@yahoo.co.uk *\/ */

/* #include <stdio.h> */
/* #include <stdlib.h> */
/* #include "hdf5.h" */
/* #include "erl_nif.h" */
/* #include "dbg.h" */
/* #include "erlhdf5.h" */

/* //  H5D: Datasets Interface */

/* // creates a new simple dataspace and opens it for access */
/* ERL_NIF_TERM h5dcreate(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) */
/* { */
/*   hid_t file_id; */
/*   Handle* file_res; */
/*   //Handle* res; */
/*   char ds_name[MAXBUFLEN]; */

/*   hid_t type_id; */
/*   /\* ERL_NIF_TERM ret; *\/ */
/*   /\* const ERL_NIF_TERM *terms; *\/ */
/*   /\* int rank; // number of dimensions of dataspace *\/ */
/*   /\* hsize_t* dimsf; // array specifiying the size of each dimension *\/ */
/*   /\* int i; *\/ */

/*   // parse arguments */
/*   check(argc == 5, "Incorrent number of arguments"); */
/*   check(enif_get_resource(env, argv[0], RES_TYPE, (void**) &file_res) != 0,	\ */
/* 	"Can't get file resource from argv"); */
/*   //check(enif_get_tuple(env, argv[1], &rank, &terms) != 0, "Can't get terms from argv"); */
/*   check(enif_get_string(env, argv[1], ds_name, sizeof(ds_name), ERL_NIF_LATIN1) != 0, \ */
/* 	"Can't get dataset name from argv"); */



/*   file_id = file_res->id; */

/*   /\* // close file *\/ */
/*   /\* err = H5Fclose(res->id); *\/ */
/*   /\* check(err == 0, "Failed to close file."); *\/ */


/*   /\* // allocate array of size rank *\/ */
/*   /\* dimsf = (hsize_t*) malloc(rank * sizeof(int)); *\/ */
/*   /\* for(i = 0; i < rank; i++) { *\/ */
/*   /\*   dimsf[i] = (int)terms[i]; *\/ */
/*   /\* } *\/ */

/*   /\* // convert access flag to format which hdf5 api understand *\/ */
/*   /\* check(_convert_flag(file_access_flags, &flags) == 0, "Failed to convert access flag"); *\/ */

/*   // create a new file using default properties */
/*   dataspace_id = H5Screate_simple(rank, dimsf, NULL); */
/*   check(dataspace_id > 0, "Failed to create dataspace."); */

/*   // create a resource to pass reference to id back to erlang */
/*   res = enif_alloc_resource(RES_TYPE, sizeof(Handle)); */
/*   check(res, "Failed to allocate resource for type %s", "Handle"); */

/*   // add ref to resource */
/*   res->id = dataspace_id; */
/*   ret = enif_make_resource(env, res); */

/*   // cleanup */
/*   enif_release_resource(res); */
/*   free(dimsf); */

/*   return enif_make_tuple2(env, ATOM_OK, ret); */

/*  error: */
/*   if(dataspace_id) H5Sclose(dataspace_id); */
/*   if(dimsf) free(dimsf); */

/*   return error_tuple(env, "Can not create dataspace"); */
/* }; */
