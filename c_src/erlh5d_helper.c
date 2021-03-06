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
#include "hdf5.h"
#include "erl_nif.h"
#include "dbg.h"
#include "erlhdf5.h"


int convert_array_to_nif_array(ErlNifEnv* env, hsize_t size, hsize_t *arr_from, ERL_NIF_TERM* arr_to)
{
  int i;
  for(i = 0; i < size; i++) {
    arr_to[i] = enif_make_int(env, arr_from[i]);
  }
  return 0;
};

int convert_int_array_to_nif_array(ErlNifEnv* env, hsize_t size, int *arr_from, ERL_NIF_TERM* arr_to)
{
  int i;
  for(i = 0; i < size; i++) {
    arr_to[i] = enif_make_int(env, arr_from[i]);
  }
  return 0;
};


int convert_nif_to_hsize_array(ErlNifEnv* env, hsize_t size, const ERL_NIF_TERM* arr_from, hsize_t *arr_to)
{
  int n, i;
  for(i = 0; i < size; i++) {
    check(enif_get_int(env, arr_from[i], &n), "error getting diskspace dimensions");
    arr_to[i] = (hsize_t)n;
  }
  return 0;

 error:
  return -1;
};
