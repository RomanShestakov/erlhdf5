-module(erlhdf5_tests).
-compile([export_all]).
-include_lib("eunit/include/eunit.hrl").

hdf5_test_() ->
    {inorder,
     [
      ?_assertMatch({ok, _}, erlhdf5:h5fcreate("test_file_hdf5.h5", true)),
      ?_assertMatch({error, _}, erlhdf5:h5fcreate("test_file_hdf5.h5", false)),
      ?_assertMatch({error, _}, erlhdf5:h5fcreate("test_file_hdf5.h5", false1))
     ]}.
