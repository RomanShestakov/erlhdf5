#ifndef __erlhdf5_h__
#define __erlhdf5_h__

#include "hdf5.h"
#include "erl_nif.h"

#define MAXBUFLEN       1024

// Atoms (initialized in on_load)
#define ATOM(Id, Value) { Id = enif_make_atom(env, Value); }
ERL_NIF_TERM ATOM_TRUE;
ERL_NIF_TERM ATOM_FALSE;
ERL_NIF_TERM ATOM_OK;
ERL_NIF_TERM ATOM_ERROR;

ErlNifResourceType* RES_TYPE;
// Resouce type to pass pointers to erlang from c
typedef struct
{
  hid_t id;
} Handle;

// prototypes
ERL_NIF_TERM error_tuple(ErlNifEnv* env, char* reason);
ERL_NIF_TERM h5fcreate(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
ERL_NIF_TERM h5fopen(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
ERL_NIF_TERM h5fclose(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
ERL_NIF_TERM h5screate(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);

#endif
