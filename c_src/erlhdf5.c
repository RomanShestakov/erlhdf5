/* Copyright (C) 2012 Roman Shestakov */

/* This file is part of erlhdf5 */

/* elrhdf5 is free software: you can redistribute it and/or modify */
/* it under the terms of the GNU Lesser General Public License as */
/* published by the Free Software Foundation, either version 3 of */
/* the License, or (at your option) any later version. */

/* elrhdf5 is distributed in the hope that it will be useful, */
/* but WITHOUT ANY WARRANTY; without even the implied warranty of */
/* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the */
/* GNU Lesser General Public License for more details. */

/* You should have received a copy of the GNU Lesser General Public */
/* License along with Erlsom.  If not, see */
/* <http://www.gnu.org/licenses/>. */
/* Author contact: romanshestakov@yahoo.co.uk */

#include <stdio.h>
#include <stdlib.h>
#include "erl_nif.h"
#include "dbg.h"
#include "erlhdf5.h"

void free_res(ErlNifEnv* env, void* obj)
{
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
    /* ATOM(ATOM_TRUE, "true"); */
    /* ATOM(ATOM_FALSE, "false"); */

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

static ErlNifFunc nif_funcs[] =
{
  {"h5fcreate", 2, h5fcreate},
  {"h5fopen", 2, h5fopen},
  {"h5fclose", 1, h5fclose},
  {"h5screate_simple", 2, h5screate_simple},
  {"h5sclose", 1, h5sclose},
  {"h5pcreate", 1, h5pcreate},
  {"h5pclose", 1, h5pclose},
  {"h5tcopy", 1, h5tcopy},
  {"h5tclose", 1, h5tclose},
  {"h5tget_class", 1, h5tget_class},
  {"h5dcreate", 5, h5dcreate},
  {"h5dopen", 2, h5dopen},
  {"h5dclose", 1, h5dclose},
  {"h5dget_type", 1, h5dget_type},
  {"h5d_get_space_status", 1, h5d_get_space_status},
  {"h5dwrite", 2, h5dwrite},
  {"h5d_get_storage_size", 1, h5d_get_storage_size},
  //  {"h5dwrite_example", 2, h5dwrite_example}
};


ERL_NIF_INIT(erlhdf5, nif_funcs, &load, NULL, NULL, NULL);
