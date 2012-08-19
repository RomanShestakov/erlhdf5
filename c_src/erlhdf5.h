#ifndef __erlhdf5_h__
#define __erlhdf5_h__

#define ATOM(Id, Value) { Id = enif_make_atom(env, Value); }

// Atoms (initialized in on_load)
static ERL_NIF_TERM ATOM_TRUE;
static ERL_NIF_TERM ATOM_FALSE;
static ERL_NIF_TERM ATOM_OK;
static ERL_NIF_TERM ATOM_ERROR;

static int _convert_file_access_flag(char* flag, unsigned *access_flag);

typedef struct
{
  hid_t* file_id;
} FileHandle;

#endif
