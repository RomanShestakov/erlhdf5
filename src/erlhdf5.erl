-module(erlhdf5).
-export([h5fcreate/2, h5fopen/2, h5fclose/1, h5screate/2]).

-include("../include/erlhdf5.hrl").

-on_load(init/0).

init() ->
    erlang:load_nif(filename:join(["priv", "erlhdf5"]), 0).

%%--------------------------------------------------------------------
%% @doc
%% create hdf5 file
%% @end
%%--------------------------------------------------------------------
-spec h5fcreate(FileName::string(), Flag::atom()) -> {ok, binary()} | {error, atom()}.
h5fcreate(_FileName, _Flag) ->
    nif_error(?LINE).

%%--------------------------------------------------------------------
%% @doc
%% open hdf5 file
%% @end
%%--------------------------------------------------------------------
-spec h5fopen(FileName::string(), Flag::atom()) -> {ok, FileHandler::binary()} | {error, Reason::atom()}.
h5fopen(_FileName, _Flag) ->
    nif_error(?LINE).

%%--------------------------------------------------------------------
%% @doc
%% close hdf5 file
%% @end
%%--------------------------------------------------------------------
-spec h5fclose(FileHandler::binary()) -> ok | {error, Reason::atom()}.
h5fclose(_FileHandler) ->
    nif_error(?LINE).


%%--------------------------------------------------------------------
%% @doc
%% create hds5 file
%% @end
%%--------------------------------------------------------------------
-spec h5screate(Rank::integer(), Dimensions::tuple()) -> {ok, binary()} | {error, atom()}.
h5screate(_Rank, _Dimensions) ->
    nif_error(?LINE).


nif_error(Line) ->
    exit({nit_library_not_loaded, module, ?MODULE, line, Line}).
