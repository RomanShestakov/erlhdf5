%% Copyright (C) 2012 Roman Shestakov
%%%
%%% This file is part of erlhdf5
%%%
%%% elrhdf5 is free software: you can redistribute it and/or modify
%%% it under the terms of the GNU Lesser General Public License as
%%% published by the Free Software Foundation, either version 3 of
%%% the License, or (at your option) any later version.
%%%
%%% elrhdf5 is distributed in the hope that it will be useful,
%%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%%% GNU Lesser General Public License for more details.
%%%
%%% You should have received a copy of the GNU Lesser General Public
%%% License along with Erlsom.  If not, see
%%% <http://www.gnu.org/licenses/>.
%%%
%%% Author contact: romanshestakov@yahoo.co.uk

-module(erlhdf5).
-export([h5fcreate/2, h5fopen/2, h5fclose/1]).
-export([h5screate/2]).
-export([h5pcreate/1, h5pclose/1]).
-export([h5tcopy/1]).

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

%%--------------------------------------------------------------------
%% @doc
%% create a new property list as an instance of a property list class
%% @end
%%--------------------------------------------------------------------
-spec h5pcreate(Class::string()) -> {ok, binary()} | {error, atom()}.
h5pcreate(_Class) ->
    nif_error(?LINE).

%%--------------------------------------------------------------------
%% @doc
%% close properties list
%% @end
%%--------------------------------------------------------------------
-spec h5pclose(Handler::binary()) -> ok | {error, Reason::atom()}.
h5pclose(_Handler) ->
    nif_error(?LINE).

%%--------------------------------------------------------------------
%% @doc
%% Copies an existing datatype.
%% @end
%%--------------------------------------------------------------------
-spec h5tcopy(Handler::binary()) -> ok | {error, Reason::atom()}.
h5tcopy(_Handler) ->
    nif_error(?LINE).

nif_error(Line) ->
    exit({nit_library_not_loaded, module, ?MODULE, line, Line}).
