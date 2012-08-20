#include "hdf5.h"
#include <stdio.h>
#include <stdlib.h>
#include "erl_nif.h"
#include "dbg.h"
#include "erlhdf5.h"

#define MAXBUFLEN       1024
#define ATOM(Id, Value) { Id = enif_make_atom(env, Value); }

ErlNifResourceType* RES_TYPE;

// Atoms (initialized in on_load)
static ERL_NIF_TERM ATOM_TRUE;
static ERL_NIF_TERM ATOM_FALSE;
static ERL_NIF_TERM ATOM_OK;
static ERL_NIF_TERM ATOM_ERROR;

// prototype
static int _convert_flag(char* file_access_flags, unsigned *flags);

typedef struct
{
  hid_t file_id;
} FileHandle;

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

// create file
static ERL_NIF_TERM h5fcreate(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  hid_t file_id;
  FileHandle* res;
  ERL_NIF_TERM ret;
  char file_name[MAXBUFLEN];
  char file_access_flags[MAXBUFLEN];
  unsigned flags;

  // parse arguments
  check(argc == 2, "Incorrent number of arguments");
  check(enif_get_string(env, argv[0], file_name, sizeof(file_name), ERL_NIF_LATIN1) != 0, \
	"Can't get file name from argv");
  check(enif_get_atom(env, argv[1], file_access_flags, sizeof(file_access_flags), ERL_NIF_LATIN1) != 0, \
	"Can't get file_access_flag from argv");

  // convert access flag to format which hdf5 api understand
  check(_convert_flag(file_access_flags, &flags) == 0, "Failed to convert access flag");

  // create a new file using default properties
  file_id = H5Fcreate(file_name, flags, H5P_DEFAULT, H5P_DEFAULT);
  check(file_id > 0, "Failed to create %s.", file_name);

  // create a resource to pass reference to file_id back to erlang
  res = enif_alloc_resource(RES_TYPE, sizeof(FileHandle));
  check(res, "Failed to allocate resource for type %s", "FileHandle");

  // add ref to resource
  res->file_id = file_id;
  ret = enif_make_resource(env, res);
  enif_release_resource(res);
  return enif_make_tuple2(env, ATOM_OK, ret);

 error:
  if(file_id) H5Fclose (file_id);
  return error_tuple(env, "Can not create file");
};

// open file
static ERL_NIF_TERM h5fopen(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  hid_t file_id;
  FileHandle* res;
  ERL_NIF_TERM ret;
  char file_name[MAXBUFLEN];
  char file_access_flags[MAXBUFLEN];
  unsigned flags;

  // parse arguments
  check(argc == 2, "Incorrent number of arguments");
  check(enif_get_string(env, argv[0], file_name, sizeof(file_name), ERL_NIF_LATIN1) != 0, \
	"Can't get file name from argv");
  check(enif_get_atom(env, argv[1], file_access_flags, sizeof(file_access_flags), ERL_NIF_LATIN1) != 0, \
	"Can't get file_access_flag from argv");

  // convert access flag to format which hdf5 api understand
  check(_convert_flag(file_access_flags, &flags) == 0, "Failed to convert access flag");

  // create a new file using default properties
  file_id = H5Fopen(file_name, flags, H5P_DEFAULT);
  check(file_id > 0, "Failed to open %s.", file_name);

  // create a resource to pass reference to file_id back to erlang
  res = enif_alloc_resource(RES_TYPE, sizeof(FileHandle));
  check(res, "Failed to allocate resource for type %s", "FileHandle");

  // add ref to resource
  res->file_id = file_id;
  ret = enif_make_resource(env, res);
  enif_release_resource(res);
  return enif_make_tuple2(env, ATOM_OK, ret);

 error:
  if(file_id) H5Fclose (file_id);
  return error_tuple(env, "Can not open file");
};


/* static ERL_NIF_TERM */
/* read(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) */
/* { */
/*     Example* res; */

/*     if(argc != 1) */
/*     { */
/*         return enif_make_badarg(env); */
/*     } */

/*     if(!enif_get_resource(env, argv[0], RES_TYPE, (void**) &res)) */
/*     { */
/* return enif_make_badarg(env); */
/*     } */

/*     return enif_make_int(env, res->id); */
/* } */


// open file
static ERL_NIF_TERM h5fclose(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  FileHandle* res;
  herr_t err;

  // parse arguments
  check(argc == 1, "Incorrent number of arguments");
  check(enif_get_resource(env, argv[0], RES_TYPE, (void**) &res) != 0,	\
	"Can't get resource from argv");

  // close file
  err = H5Fclose(res->file_id);
  check(err == 0, "Failed to close file.");

  return ATOM_OK;

 error:
  return error_tuple(env, "Can not close file");
};



static int _convert_flag(char* file_access_flags, unsigned *flags)
{
  if(strncmp(file_access_flags, "H5F_ACC_TRUNC", MAXBUFLEN) == 0)
      *flags = H5F_ACC_TRUNC;
  else if(strncmp(file_access_flags, "H5F_ACC_EXCL", MAXBUFLEN) == 0)
    *flags = H5F_ACC_EXCL;
  else if(strncmp(file_access_flags, "H5F_ACC_RDWR", MAXBUFLEN) == 0)
    *flags = H5F_ACC_RDWR;
  else if(strncmp(file_access_flags, "H5F_ACC_RDONLY", MAXBUFLEN) == 0)
    *flags = H5F_ACC_RDONLY;
  else
    sentinel("Unknown file access flag %s", file_access_flags);

  return 0;

 error:
  return -1;
};


static ErlNifFunc nif_funcs[] =
{
  {"h5fcreate", 2, h5fcreate},
  {"h5fopen", 2, h5fopen},
  {"h5fclose", 1, h5fclose}
};

ERL_NIF_INIT(erlhdf5, nif_funcs, &load, NULL, NULL, NULL);
