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
-export([h5screate_simple/2, h5sclose/1]).
-export([h5pcreate/1, h5pclose/1]).
-export([h5tcopy/1, h5tclose/1, h5tget_class/1, h5tget_order/1]).
-export([h5dcreate/5, h5dopen/2, h5dclose/1, h5dget_type/1, h5d_get_space_status/1, h5dwrite/2, h5d_get_storage_size/1]).
%, h5dwrite_example/2

-include("../include/erlhdf5.hrl").

-on_load(init/0).

init() ->
    erlang:load_nif(filename:join([get_priv_dir(?MODULE), "erlhdf5"]), 0).

%%--------------------------------------------------------------------
%% @doc
%% get a path to priv dir
%% @end
%%--------------------------------------------------------------------
-spec get_priv_dir(atom()) -> file:filename().
get_priv_dir(Module) ->
    filename:join([filename:dirname(filename:dirname(code:which(Module))), "priv"]).

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
%% create dataspace
%% @end
%%--------------------------------------------------------------------
-spec h5screate_simple(Rank::integer(), Dimensions::tuple()) -> {ok, binary()} | {error, atom()}.
h5screate_simple(_Rank, _Dimensions) ->
    nif_error(?LINE).

%%--------------------------------------------------------------------
%% @doc
%% close dataspace
%% @end
%%--------------------------------------------------------------------
-spec h5sclose (Handler::binary()) -> ok | {error, Reason::atom()}.
h5sclose(_Handler) ->
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

%%--------------------------------------------------------------------
%% @doc
%% Releases a datatype.
%% @end
%%--------------------------------------------------------------------
-spec h5tclose(Handler::binary()) -> ok | {error, Reason::atom()}.
h5tclose(_Handler) ->
    nif_error(?LINE).

%%--------------------------------------------------------------------
%% @doc
%% Returns the datatype class identifier.
%% @end
%%--------------------------------------------------------------------
-spec h5tget_class(Handler::binary()) -> ok | {error, Reason::atom()}.
h5tget_class(_Handler) ->
    nif_error(?LINE).

%%--------------------------------------------------------------------
%% @doc
%% Returns the byte order of an atomic datatype.
%% @end
%%--------------------------------------------------------------------
-spec h5tget_order(Handler::binary()) -> {ok, integer} | {error, Reason::atom()}.
h5tget_order(_Handler) ->
    nif_error(?LINE).

%%--------------------------------------------------------------------
%% @doc
%% create a new dataset
%% @end
%%--------------------------------------------------------------------
-spec h5dcreate(File::binary(), Name::string(), Type::binary(), Space::binary(), Prop::binary()) ->
		       {ok, binary()} | {error, atom()}.
h5dcreate(_File, _Name, _Type, _Space, _Prop) ->
    nif_error(?LINE).

%%--------------------------------------------------------------------
%% @doc
%% Opens an existing dataset.
%% @end
%%--------------------------------------------------------------------
-spec h5dopen(File::binary(), Name::string()) ->
		       {ok, binary()} | {error, atom()}.
h5dopen(_File, _Name) ->
    nif_error(?LINE).

%%--------------------------------------------------------------------
%% @doc
%% close dataset
%% @end
%%--------------------------------------------------------------------
-spec h5dclose (Handler::binary()) -> ok | {error, Reason::atom()}.
h5dclose(_Handler) ->
    nif_error(?LINE).

%%--------------------------------------------------------------------
%% @doc
%% Returns an identifier for a copy of the datatype for a dataset.
%% @end
%%--------------------------------------------------------------------
-spec h5dget_type (Handler::binary()) -> ok | {error, Reason::atom()}.
h5dget_type(_Handler) ->
    nif_error(?LINE).

%%--------------------------------------------------------------------
%% @doc
%% Determines whether space has been allocated for a dataset.
%% @end
%%--------------------------------------------------------------------
-spec h5d_get_space_status(Handler::binary()) -> {ok, Status::atom()} | {error, Reason::atom()}.
h5d_get_space_status(_Handler) ->
    nif_error(?LINE).


%% -spec h5dwrite(DSetHandler::binary(), TypeHandler::binary(), DLPHandler::binary()) ->
%% 		      ok | {error, Reason::atom()}.
h5dwrite(_DSetHandler, _Data) ->
    nif_error(?LINE).

%%--------------------------------------------------------------------
%% @doc
%% Returns the amount of storage allocated for a dataset.
%% @end
%%--------------------------------------------------------------------
-spec h5d_get_storage_size(Handler::binary()) -> {ok, Size::integer()} | {error, Reason::atom()}.
h5d_get_storage_size(_Handler) ->
    nif_error(?LINE).


%% %%-spec h5dwrite_example(Handler::binary()) -> {ok, Size::integer()} | {error, Reason::atom()}.
%% h5dwrite_example(_FileHandler, _Space) ->
%%     nif_error(?LINE).


nif_error(Line) ->
    exit({nit_library_not_loaded, module, ?MODULE, line, Line}).
