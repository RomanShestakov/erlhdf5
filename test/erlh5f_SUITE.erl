-module(erlh5f_SUITE).

-compile(export_all).

-include_lib("common_test/include/ct.hrl").
-include("../include/erlhdf5.hrl").

suite() ->
    [{timetrap,{seconds,30}}].

init_per_suite(Config) ->
    {ok, File} = erlhdf5:h5fcreate("test_file_hdf5.h5", 'H5F_ACC_TRUNC'),
    [{file, File} | Config].

end_per_suite(Config) ->
    File = ?config(file, Config),
    ok = erlhdf5:h5fclose(File),
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

