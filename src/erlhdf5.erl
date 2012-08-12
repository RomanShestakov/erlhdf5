-module(erlhdf5).
-export([hello/0]).

-on_load(init/0).

init() ->
    erlang:load_nif(filename:join(["priv", "erlhdf5"]), 0).

hello() ->
    exit(nit_library_not_loaded).
