#include "hdf5.h"
#include <stdio.h>
#include <stdlib.h>
#include "erl_nif.h"
#include "dbg.h"
#include "erlhdf5.h"

// creates a new simple dataspace and opens it for access
ERL_NIF_TERM h5screate(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  hid_t dataspace_id;
  Handle* res;
  ERL_NIF_TERM ret;
  const ERL_NIF_TERM *terms;
  int rank; // number of dimensions of dataspace
  hsize_t* dimsf; // array specifiying the size of each dimension
  int i;

  // parse arguments
  check(argc == 2, "Incorrent number of arguments");
  check(enif_get_int(env, argv[0], &rank ) != 0, "Can't get rank from argv");
  check(enif_get_tuple(env, argv[1], &rank, &terms) != 0, "Can't get terms from argv");

  // allocate array of size rank
  dimsf = (hsize_t*) malloc(rank * sizeof(int));
  for(i = 0; i < rank; i++) {
    dimsf[i] = (int)terms[i];
  }

  /* // convert access flag to format which hdf5 api understand */
  /* check(_convert_flag(file_access_flags, &flags) == 0, "Failed to convert access flag"); */

  // create a new file using default properties
  dataspace_id = H5Screate_simple(rank, dimsf, NULL);
  check(dataspace_id > 0, "Failed to create dataspace.");

  // create a resource to pass reference to id back to erlang
  res = enif_alloc_resource(RES_TYPE, sizeof(Handle));
  check(res, "Failed to allocate resource for type %s", "Handle");

  // add ref to resource
  res->id = dataspace_id;
  ret = enif_make_resource(env, res);
  enif_release_resource(res);
  return enif_make_tuple2(env, ATOM_OK, ret);

 error:
  if(dataspace_id) H5Sclose(dataspace_id);
  return error_tuple(env, "Can not create dataspace");
};
