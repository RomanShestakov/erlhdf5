-module(erlhdf5).
-export([create/1, h5fcreate/2]).

-on_load(init/0).

init() ->
    erlang:load_nif(filename:join(["priv", "erlhdf5"]), 0).

h5fcreate(_FileName, _File_Access) ->
    nif_error(?LINE).

create(_) ->
    nif_error(?LINE).

nif_error(Line) ->
    exit({nit_library_not_loaded, module, ?MODULE, line, Line}).
