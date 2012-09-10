-module(erlh5f_SUITE).

-compile(export_all).

-include_lib("common_test/include/ct.hrl").
-include("../include/erlhdf5.hrl").

suite() ->
    [{timetrap,{seconds, 60}}].

init_per_suite(Config) ->
    {ok, File} = erlhdf5:h5fcreate("test_file_hdf5.h5", 'H5F_ACC_TRUNC'),
    {ok, Space} = erlhdf5:h5screate_simple(1, {100}),
    {ok, Dcpl} = erlhdf5:h5pcreate('H5P_DATASET_CREATE'),
    {ok, Type} = erlhdf5:h5tcopy('H5T_NATIVE_INT'),
    [{file, File}, {space, Space}, {dcpl, Dcpl}, {type, Type} | Config].

end_per_suite(Config) ->
    File = ?config(file, Config),
    Space = ?config(space, Config),
    Dcpl = ?config(dcpl, Config),
    Type = ?config(type, Config),
    ok = erlhdf5:h5fclose(File),
    ok = erlhdf5:h5sclose(Space),
    ok = erlhdf5:h5pclose(Dcpl),
    ok = erlhdf5:h5tclose(Type),
    ok.

init_per_testcase(_TestCase, Config) ->
    Config.

end_per_testcase(_TestCase, _Config) ->
    ok.

all() ->
    [
     h5_write
     %write_example
    ].



%%--------------------------------------------------------------------
%% @doc
%% h5_write.c
%% @end
%%--------------------------------------------------------------------
h5_write(Config) ->
    File = ?config(file, Config),
    Space = ?config(space, Config),
    Dcpl = ?config(dcpl, Config),
    Type = ?config(type, Config),

    % create dataset
    {ok, DS} = erlhdf5:h5dcreate(File, "/dset", Type, Space, Dcpl),

    {ok, Status} = erlhdf5:h5d_get_space_status(DS),
    {ok, Size} = erlhdf5:h5d_get_storage_size(DS),
    ct:log("dataset status: ~p, size: ~p ", [Status, Size]),

    %write some data into dataset
    ok = erlhdf5:h5dwrite(DS, [1,2,3,4,5,6,7,8,9]),

    {ok, Status1} = erlhdf5:h5d_get_space_status(DS),
    {ok, Size1} = erlhdf5:h5d_get_storage_size(DS),
    ct:log("dataset status after write: ~p, size: ~p ", [Status1, Size1]),

    % close dataset
    ok = erlhdf5:h5dclose(DS),
    ok.

%% write_example(_Config) ->
%%     %% File = ?config(file, Config),
%%     %% Space = ?config(space, Config),
%%     %% Dcpl = ?config(dcpl, Config),
%%     %% Type = ?config(type, Config),

%%     %% % create dataset
%%     %% {ok, DS} = erlhdf5:h5dcreate(File, "/dset", Type, Space, Dcpl),

%%     %% {ok, Status} = erlhdf5:h5d_get_space_status(DS),
%%     %% {ok, Size} = erlhdf5:h5d_get_storage_size(DS),
%%     %% ct:log("dataset status: ~p, size: ~p ", [Status, Size]),

%%     %write some data into dataset
%%     {ok, File} = erlhdf5:h5fcreate("SDS1.h5", 'H5F_ACC_TRUNC'),
%%     {ok, Space} = erlhdf5:h5screate_simple(2, {3, 5}),

%%     ok = erlhdf5:h5dwrite_example(File, Space),

%%     %% {ok, Status1} = erlhdf5:h5d_get_space_status(DS),
%%     %% ct:log("dataset status after write, ~p ", [Status1]),

%%     % close dataset
%%     %ok = erlhdf5:h5dclose(DS),
%%     ok.

