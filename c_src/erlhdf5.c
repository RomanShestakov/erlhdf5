#include "hdf5.h"
#include <stdio.h>
#include <stdlib.h>
#include "erl_nif.h"
#include "dbg.h"

ErlNifResourceType* RES_TYPE;

#define ATOM(Id, Value) { Id = enif_make_atom(env, Value); }

// Atoms (initialized in on_load)
static ERL_NIF_TERM ATOM_TRUE;
static ERL_NIF_TERM ATOM_FALSE;
static ERL_NIF_TERM ATOM_OK;
static ERL_NIF_TERM ATOM_ERROR;
/* // http://calymirror.appspot.com/github.com/boundary/eleveldb/blob/master/c_src/eleveldb.cc */

typedef struct
{
  hid_t* file_id;
} FileHandle;

void free_res(ErlNifEnv* env, void* obj)
{
    /* Tracker* tracker = (Tracker*) enif_priv_data(env); */
    /* tracker->count -= 1; */
}

static int load(ErlNifEnv* env, void** priv, ERL_NIF_TERM load_info)
{
    const char* mod = "erlhdf5";
    const char* name = "FileHandle";
    int flags = ERL_NIF_RT_CREATE | ERL_NIF_RT_TAKEOVER;

    RES_TYPE = enif_open_resource_type(env, mod, name, free_res, flags, NULL);
    if(RES_TYPE == NULL) return -1;

    // Initialize common atoms
    ATOM(ATOM_OK, "ok");
    ATOM(ATOM_ERROR, "error");
    ATOM(ATOM_TRUE, "true");
    ATOM(ATOM_FALSE, "false");

    return 0;
};


static ERL_NIF_TERM h5fcreate(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  hid_t file_id;
  FileHandle* res;
  ERL_NIF_TERM ret;
  char file_name[4096];

  check(argc == 1, "Need an arguments");
  check(enif_get_string(env, argv[0], file_name, sizeof(file_name), ERL_NIF_LATIN1) != 0, "Can't get file name from argv");

  // create a new file using default properties
  file_id = H5Fcreate(file_name, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);
  check(file_id, "Failed to open %s.", file_name);

  //setup handle
  res = enif_alloc_resource(RES_TYPE, sizeof(FileHandle));
  check(res, "Failed to allocate resource for type %s", "FileHandle");
  res->file_id = &file_id;
  ret = enif_make_resource(env, res);
  enif_release_resource(res);
  return enif_make_tuple2(env, ATOM_OK, ret);

 error:
  /* if(res) return enif_make_badarg(env); */
  /* if(file_id) return enif_make_badarg(env); */
  /* if(argc) return enif_make_badarg(env); */
  /* return -1; */
  return enif_make_badarg(env);
};

static ErlNifFunc nif_funcs[] =
{
  {"h5fcreate", 1, h5fcreate}
};

ERL_NIF_INIT(erlhdf5, nif_funcs, &load, NULL, NULL, NULL);
