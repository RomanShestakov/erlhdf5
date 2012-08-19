#include "hdf5.h"
#include <stdio.h>
#include <stdlib.h>
#include "erl_nif.h"
#include "dbg.h"
#include "erlhdf5.h"

ErlNifResourceType* RES_TYPE;

#define MAXBUFLEN       1024

/* // http://calymirror.appspot.com/github.com/boundary/eleveldb/blob/master/c_src/eleveldb.cc */

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
    check(RES_TYPE, "Failed to open resource type %s", name);

    // Initialize common atoms
    ATOM(ATOM_OK, "ok");
    ATOM(ATOM_ERROR, "error");
    ATOM(ATOM_TRUE, "true");
    ATOM(ATOM_FALSE, "false");

    return 0;

 error:
    return -1;
};

// func to convert error message
ERL_NIF_TERM error_tuple(ErlNifEnv* env, char* reason)
{
    ERL_NIF_TERM why = enif_make_string(env, reason, ERL_NIF_LATIN1);
    return enif_make_tuple2(env, ATOM_ERROR, why);
}

static ERL_NIF_TERM h5fcreate(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  hid_t file_id;
  FileHandle* res;
  ERL_NIF_TERM ret;
  char file_name[MAXBUFLEN];
  char allow_truncate[MAXBUFLEN];
  unsigned file_access_mode;

  // parse arguments
  check(argc == 2, "Incorrent number of arguments");
  check(enif_get_string(env, argv[0], file_name, sizeof(file_name), ERL_NIF_LATIN1) != 0, \
	"Can't get file name from argv");
  check(enif_get_atom(env, argv[1], allow_truncate, sizeof(allow_truncate), ERL_NIF_LATIN1) != 0, \
	"Can't get file_access_flag from argv");

  // convert access flag to format which hdf5 api understand
  check(_convert_file_access_flag(allow_truncate, &file_access_mode) == 0, "Failed to convert access flag");

  // create a new file using default properties
  file_id = H5Fcreate(file_name, file_access_mode, H5P_DEFAULT, H5P_DEFAULT);
  check(file_id > 0, "Failed to create %s.", file_name);

  // create a resource to pass reference to file_id back to erlang
  res = enif_alloc_resource(RES_TYPE, sizeof(FileHandle));
  check(res, "Failed to allocate resource for type %s", "FileHandle");

  // add ref to resource
  res->file_id = &file_id;
  ret = enif_make_resource(env, res);
  enif_release_resource(res);
  return enif_make_tuple2(env, ATOM_OK, ret);

 error:
  if(file_id) H5Fclose (file_id);
  return error_tuple(env, "Can not create file");
};

static int _convert_file_access_flag(char* allow_truncate , unsigned *file_access_mode)
{
  if(strncmp(allow_truncate, "true", MAXBUFLEN) == 0)
      *file_access_mode = H5F_ACC_TRUNC;
  else if(strncmp(allow_truncate, "false", MAXBUFLEN) == 0)
    *file_access_mode = H5F_ACC_EXCL;
  else
    sentinel("Unknown file access flag %s", allow_truncate);

  return 0;

 error:
  return -1;
};


static ErlNifFunc nif_funcs[] =
{
  {"h5fcreate", 2, h5fcreate}
};

ERL_NIF_INIT(erlhdf5, nif_funcs, &load, NULL, NULL, NULL);
