#include "hdf5.h"
#include <stdio.h>
#include <stdlib.h>
#include "erl_nif.h"
#include "dbg.h"

ErlNifResourceType* RES_TYPE;

#define ATOM(Id, Value) { Id = enif_make_atom(env, Value); }

#define MAXBUFLEN       1024

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


static unsigned _convert_file_access_flag(char*);


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
  char file_access_flag[MAXBUFLEN];
  unsigned access_flag;

  // parse arguments
  check(argc == 2, "Incorrent number of arguments");
  check(enif_get_string(env, argv[0], file_name, sizeof(file_name), ERL_NIF_LATIN1) != 0, "Can't get file name from argv");
  check(enif_get_atom(env, argv[1], file_access_flag, sizeof(file_access_flag), ERL_NIF_LATIN1) != 0, \
	"Can't get file_access_flag from argv");

  // convert access flag to format which hdf5 api understand
  access_flag = _convert_file_access_flag(file_access_flag);
  check(access_flag, "Failed to convert access flag");

  // create a new file using default properties
  file_id = H5Fcreate(file_name, access_flag, H5P_DEFAULT, H5P_DEFAULT);
  check(file_id > 0, "Failed to create %s.", file_name);

  //setup handle
  res = enif_alloc_resource(RES_TYPE, sizeof(FileHandle));
  check(res, "Failed to allocate resource for type %s", "FileHandle");
  res->file_id = &file_id;
  ret = enif_make_resource(env, res);
  enif_release_resource(res);
  return enif_make_tuple2(env, ATOM_OK, ret);

 error:
  if(file_id) H5Fclose (file_id);
  return error_tuple(env, "Can not create file");
};


static unsigned _convert_file_access_flag(char* flag)
{
  if(strncmp(flag, "true", MAXBUFLEN) == 0)
    return H5F_ACC_TRUNC;
  else if(strncmp(flag, "false", MAXBUFLEN) == 0)
    return H5F_ACC_EXCL;
  else
    sentinel("Unknown file access flag %s", flag);

 error:
  return -1;
};


static ErlNifFunc nif_funcs[] =
{
  {"h5fcreate", 2, h5fcreate}
};

ERL_NIF_INIT(erlhdf5, nif_funcs, &load, NULL, NULL, NULL);
