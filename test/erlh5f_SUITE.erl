-module(erlh5f_SUITE).

-compile(export_all).

-include_lib("common_test/include/ct.hrl").
-include("../include/erlhdf5.hrl").

suite() ->
    [{timetrap,{seconds, 60}}].

init_per_suite(Config) ->
    Config.

end_per_suite(_Config) ->
    ok.

init_per_testcase(_TestCase, Config) ->
    Config.

end_per_testcase(_TestCase, _Config) ->
    ok.

all() ->
    [
     h5_write,
     h5_read,
     h5_lite_write_read
     %% h5_lite_read
     %write_example
    ].

%%--------------------------------------------------------------------
%% @doc
%% h5_write.c
%% @end
%%--------------------------------------------------------------------
h5_write(_Config) ->
    %% create data file
    {ok, File} = erlhdf5:h5fcreate("hdf5.h5", 'H5F_ACC_TRUNC'),

    %% 2d array with 3 columns
    {ok, Space} = erlhdf5:h5screate_simple(2, {100, 3}),
    {ok, Dcpl} = erlhdf5:h5pcreate('H5P_DATASET_CREATE'),
    {ok, Type} = erlhdf5:h5tcopy('H5T_NATIVE_INT'),

    %% create new dataset
    {ok, DS} = erlhdf5:h5dcreate(File, "/dset", Type, Space, Dcpl),

    {ok, Status} = erlhdf5:h5d_get_space_status(DS),
    {ok, Size} = erlhdf5:h5d_get_storage_size(DS),
    ct:log("dataset status: ~p, size: ~p ", [Status, Size]),

    %write some data into dataset
    ok = erlhdf5:h5dwrite(DS, [{1, 3, 4},{2, 5, 5},{3, 6, 6},{4, 7, 6},
			       {5, 7, 7},{6, 8, 7},{7, 9, 10},{8, 9, 11},{9, 7, 12}]),

    {ok, Status1} = erlhdf5:h5d_get_space_status(DS),
    {ok, Size1} = erlhdf5:h5d_get_storage_size(DS),
    ct:log("dataset status after write: ~p, size: ~p ", [Status1, Size1]),

    %% close resources
    ok = erlhdf5:h5sclose(Space),
    ok = erlhdf5:h5pclose(Dcpl),
    ok = erlhdf5:h5tclose(Type),
    ok = erlhdf5:h5dclose(DS),
    ok.

%%--------------------------------------------------------------------
%% @doc
%% h5_write.c
%% @end
%%--------------------------------------------------------------------
h5_lite_write_read(_Config) ->
    %% set some params
    FileName = "hdf5_lt.h5",
    Rank = 2,
    DS_Name = "/dset_lt",
    TestData = [1, 2, 3, 4, 5, 6, 7, 8],

    %% open new file
    {ok, File} = erlhdf5:h5fcreate(FileName, 'H5F_ACC_TRUNC'),
    %% create dataset with rank = 2

    ok = erlhdf5:h5lt_make_dataset(File, DS_Name, Rank, {2, 4}, TestData),
    %% get dataset rank - should match to rank used in dataset creation

    {ok, Rank} = erlhdf5:h5ltget_dataset_ndims(File, DS_Name),
    ct:log("dataset rank : ~p ", [Rank]),

    %% check ds info
    {ok, DS_Info} = erlhdf5:h5ltget_dataset_info(File, DS_Name, Rank),
    ct:log("dataset info : ~p ", [DS_Info]),

    %% read dataset
    {ok, TestData} = erlhdf5:h5lt_read_dataset_int(File, DS_Name),
    ct:log("data : ~p ", [TestData]),

    %% close file
    ok = erlhdf5:h5fclose(File),
    ok.


%%--------------------------------------------------------------------
%% @doc
%% h5_read.c
%% @end
%%--------------------------------------------------------------------
h5_read(_Config) ->
    %% DS = ?config(dataset, Config),
    {ok, File} = erlhdf5:h5fopen("hdf5.h5", 'H5F_ACC_RDONLY'),
    {ok, DS} = erlhdf5:h5dopen(File, "/dset"),

    {ok, Type} = erlhdf5:h5dget_type(DS),
    {ok, Class} = erlhdf5:h5tget_class(Type),
    Class = ?H5T_INTEGER,
    ct:log("Class Type: ~p", [Class]),

    {ok, Order} = erlhdf5:h5tget_order(Type),
    Order = ?H5T_ORDER_LE,
    ct:log("Order: ~p", [Order]),

    {ok, Size} = erlhdf5:h5tget_size(Type),
    ct:log("Size: ~p", [Size]),

    {ok, Dataspace} = erlhdf5:h5dget_space(DS),

    {ok, NDims} = erlhdf5:h5sget_simple_extent_ndims(Dataspace),
    ct:log("NDims: ~p", [NDims]),

    {ok, Dims, MaxDims} = erlhdf5:h5sget_simple_extent_dims(Dataspace, NDims),
    ct:log("Dims: ~p, MaxDims: ~p", [Dims, MaxDims]),

    %% close resources
    ok = erlhdf5:h5tclose(Type),
    ok = erlhdf5:h5sclose(Dataspace),
    ok = erlhdf5:h5dclose(DS),
    ok = erlhdf5:h5fclose(File),
    ok.
