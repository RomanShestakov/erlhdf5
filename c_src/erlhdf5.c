#include "hdf5.h"
#include <stdio.h>
#include <stdlib.h>
#include "erl_nif.h"

ErlNifResourceType* RES_TYPE;
ERL_NIF_TERM atom_ok;


/* // http://calymirror.appspot.com/github.com/boundary/eleveldb/blob/master/c_src/eleveldb.cc */

/* //static ErlNifResourceType* h5f_file_RESOURCE; */
/* ErlNifResourceType* RES_TYPE; */


/* ErlNifResourceType* RES_TYPE; */
/* ERL_NIF_TERM atom_ok; */

typedef struct
{
  int count;
} Tracker;

typedef struct
{
  int id;
} Example;

void
free_res(ErlNifEnv* env, void* obj)
{
    Tracker* tracker = (Tracker*) enif_priv_data(env);
    tracker->count -= 1;
}

static int
load(ErlNifEnv* env, void** priv, ERL_NIF_TERM load_info)
{
    const char* mod = "resources";
    const char* name = "Example";
    int flags = ERL_NIF_RT_CREATE | ERL_NIF_RT_TAKEOVER;
    Tracker* tracker;

    RES_TYPE = enif_open_resource_type(env, mod, name, free_res, flags, NULL);
    if(RES_TYPE == NULL) return -1;

    atom_ok = enif_make_atom(env, "ok");

    tracker = (Tracker*) enif_alloc(sizeof(Tracker));
    tracker->count = 0;
    *priv = (void*) tracker;

    return 0;
}

/* typedef struct */
/* { */
/*   int id; */
/* } Example; */


/* typedef struct */
/* { */
/*     hid_t* file_id; */
/* } file_handle; */

/* // atoms (init on load) */
/* static ERL_NIF_TERM ATOM_OK; */

// Prototypes
#define NIF(name) \
  ERL_NIF_TERM name(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])

//NIF(hello);
NIF(h5fcreate);

/* NIF(hello) */
/* { */
/*   return enif_make_string(env, "Hello world", ERL_NIF_LATIN1); */
/* } */


NIF(h5fcreate)
{
  hid_t file_id; /* file identifier */
  //herr_t status;
  Example* res;
  ERL_NIF_TERM ret;
  /* // create a new file using default properties */
  file_id = H5Fcreate("file.h5", H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);

  /* //setup handle */
  //file_handle* handle = (file_handle*) enif_alloc_resource(h5f_file_RESOURCE, sizeof(file_handle));
  res = enif_alloc_resource(RES_TYPE, sizeof(Example));
  /* handle->file_id = file_id; */
  /* ERL_NIF_TERM result = enif_make_resource(env, handle); */
  /* enif_release_resource(handle); */

  ret = enif_make_resource(env, res);
  enif_release_resource(res);

  res->id = 22;

  //return enif_make_tuple2(env, ATOM_OK, result);
  return enif_make_tuple2(env, atom_ok, ret);
};


static ERL_NIF_TERM
create(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    Example* res;
    ERL_NIF_TERM ret;
    unsigned int id;
    Tracker* tracker;

    if(argc != 1)
    {
        return enif_make_badarg(env);
    }

    if(!enif_get_uint(env, argv[0], &id))
    {
        return enif_make_badarg(env);
    }

    res = enif_alloc_resource(RES_TYPE, sizeof(Example));
    if(res == NULL) return enif_make_badarg(env);

    ret = enif_make_resource(env, res);
    enif_release_resource(res);

    res->id = id;

    tracker = (Tracker*) enif_priv_data(env);
    tracker->count += 1;

    return enif_make_tuple2(env, atom_ok, ret);
}


static ErlNifFunc nif_funcs[] =
{
  /* {"hello", 0, hello}, */
  {"h5fcreate", 0, h5fcreate},
  {"create", 1, create}
};

/* #define ATOM(Id, Value) { Id = enif_make_atom(env, Value); } */
/* static int on_load(ErlNifEnv* env, void** priv_data, ERL_NIF_TERM load_info) */
/* { */
/*     // Initialize common atoms */
/*     ATOM(ATOM_OK, "ok"); */
/*     /\* ATOM(ATOM_ERROR, "error"); *\/ */
/*     /\* ATOM(ATOM_TRUE, "true"); *\/ */
/*     /\* ATOM(ATOM_FALSE, "false"); *\/ */
/*     return 0; */
/* }; */

ERL_NIF_INIT(erlhdf5, nif_funcs, &load, NULL, NULL, NULL);
