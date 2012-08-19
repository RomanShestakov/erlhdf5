-module(erlhdf5).
-export([h5fcreate/2]).

-on_load(init/0).

init() ->
    erlang:load_nif(filename:join(["priv", "erlhdf5"]), 0).

%%--------------------------------------------------------------------
%% @doc
%% create hdf5 file
%% AccessMode flag : true - to truncate existing file
%% @end
%%--------------------------------------------------------------------
-spec h5fcreate(FileName::string(), Truncate::boolean()) -> {ok, binary()} | {error, atom()}.
h5fcreate(_FileName, _Truncate) ->
    nif_error(?LINE).

nif_error(Line) ->
    exit({nit_library_not_loaded, module, ?MODULE, line, Line}).
