-module(erlh5f_SUITE).

-compile(export_all).

-include_lib("common_test/include/ct.hrl").
-include("../include/erlhdf5.hrl").

suite() ->
    [{timetrap,{seconds,30}}].

init_per_suite(Config) ->
    {ok, File} = erlhdf5:h5fcreate("test_file_hdf5.h5", 'H5F_ACC_TRUNC'),
    {ok, Space} = erlhdf5:h5screate_simple(2, {4, 7}),
    {ok, Dcpl} = erlhdf5:h5pcreate('H5P_DATASET_CREATE'),
    [{file, File}, {space, Space}, {dcpl, Dcpl} | Config].

end_per_suite(Config) ->
    File = ?config(file, Config),
    Space = ?config(space, Config),
    Dcpl = ?config(dcpl, Config),
    ok = erlhdf5:h5fclose(File),
    ok = erlhdf5:h5sclose(Space),
    ok = erlhdf5:h5pclose(Dcpl),
    ok.

init_per_testcase(_TestCase, Config) ->
    Config.

end_per_testcase(_TestCase, _Config) ->
    ok.

all() ->
    [
     create_dataset
    ].


create_dataset(_Config) ->
    ok.

