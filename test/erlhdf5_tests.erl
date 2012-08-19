-module(erlhdf5_tests).
-compile([export_all]).
-include_lib("eunit/include/eunit.hrl").

hdf5_test_() ->
    [
     ?_assertMatch({ok, _}, erlhdf5:h5fcreate("test_file_hdf5.h5"))
    ].
