#ifndef __erlhdf5_h__
#define __erlhdf5_h__

#include "erl_nif.h"

#define MAXBUFLEN       1024
#define ATOM(Id, Value) { Id = enif_make_atom(env, Value); }

ErlNifResourceType* RES_TYPE;

// Atoms (initialized in on_load)
ERL_NIF_TERM ATOM_TRUE;
ERL_NIF_TERM ATOM_FALSE;
ERL_NIF_TERM ATOM_OK;
ERL_NIF_TERM ATOM_ERROR;

// prototype
ERL_NIF_TERM h5fcreate(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
ERL_NIF_TERM h5fopen(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
ERL_NIF_TERM h5fclose(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);

#endif
