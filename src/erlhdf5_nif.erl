-module(erlhdf5_nif).
-export([create/1, h5fcreate/0]).

-on_load(init/0).

init() ->
    erlang:load_nif(filename:join(["priv", "erlhdf5_nif"]), 0).

h5fcreate() ->
    exit(nit_library_not_loaded).

create(_) ->
    exit(nit_library_not_loaded).
