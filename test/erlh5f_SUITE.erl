-module(erlh5f_SUITE).

-compile(export_all).

-include_lib("common_test/include/ct.hrl").
-include("../include/erlhdf5.hrl").

suite() ->
    [{timetrap,{seconds, 60}}].

init_per_suite(Config) ->
    %% create a data file
    {ok, File} = erlhdf5:h5fcreate("hdf5.h5", 'H5F_ACC_TRUNC'),
    %% 2d array with 3 columns
    {ok, Space} = erlhdf5:h5screate_simple(2, {100, 3}),
    {ok, Dcpl} = erlhdf5:h5pcreate('H5P_DATASET_CREATE'),
    {ok, Type} = erlhdf5:h5tcopy('H5T_NATIVE_INT'),
    %% create new dataset
    {ok, DS} = erlhdf5:h5dcreate(File, "/dset", Type, Space, Dcpl),

    %% close resources
    ok = erlhdf5:h5sclose(Space),
    ok = erlhdf5:h5pclose(Dcpl),
    ok = erlhdf5:h5tclose(Type),
    ok = erlhdf5:h5dclose(DS),
    [{file, File} | Config].

end_per_suite(Config) ->
    File = ?config(file, Config),
    ok = erlhdf5:h5fclose(File),
    ok.

init_per_testcase(_TestCase, Config) ->
    File = ?config(file, Config),
    %% open existing dataset
    {ok, DS} = erlhdf5:h5dopen(File, "/dset"),
    [{file, File}, {dataset, DS} | Config].

end_per_testcase(_TestCase, Config) ->
    DS = ?config(dataset, Config),
    ok = erlhdf5:h5dclose(DS),
    ok.

all() ->
    [
     h5_write,
     h5_read
     %write_example
    ].

%%--------------------------------------------------------------------
%% @doc
%% h5_write.c
%% @end
%%--------------------------------------------------------------------
h5_write(Config) ->
    DS = ?config(dataset, Config),
    {ok, Status} = erlhdf5:h5d_get_space_status(DS),
    {ok, Size} = erlhdf5:h5d_get_storage_size(DS),
    ct:log("dataset status: ~p, size: ~p ", [Status, Size]),

    %write some data into dataset
    ok = erlhdf5:h5dwrite(DS, [{1, 3, 4},{2, 5, 5},{3, 6, 6},{4, 7, 6},{5, 7, 7},{6, 8, 7},{7, 9, 10},{8, 9, 11},{9, 7, 12}]),

    {ok, Status1} = erlhdf5:h5d_get_space_status(DS),
    {ok, Size1} = erlhdf5:h5d_get_storage_size(DS),
    ct:log("dataset status after write: ~p, size: ~p ", [Status1, Size1]),
    ok.


%%--------------------------------------------------------------------
%% @doc
%% h5_read.c
%% @end
%%--------------------------------------------------------------------
h5_read(Config) ->
    DS = ?config(dataset, Config),

    {ok, Type} = erlhdf5:h5dget_type(DS),
    {ok, Class} = erlhdf5:h5tget_class(Type),
    Class = ?H5T_INTEGER,
    ct:log("Class Type: ~p", [Class]),
    ok = erlhdf5:h5tclose(Type),
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

