-module(erlhdf5_tests).
-compile([export_all]).
-include_lib("eunit/include/eunit.hrl").

-include("../include/erlhdf5.hrl").

hdf5_test_() ->
    {inorder,
     [
      ?_assertMatch({ok, _}, erlhdf5:h5fcreate("test_file_hdf5.h5", 'H5F_ACC_TRUNC')),
      ?_assertMatch({error, _}, erlhdf5:h5fcreate("test_file_hdf5.h5", 'H5F_ACC_EXCL')),
      ?_assertMatch({error, _}, erlhdf5:h5fcreate("test_file_hdf5.h5", unknown_flag)),
      ?_assertMatch({error, _}, erlhdf5:h5fcreate(incorrect_file_name, 'H5F_ACC_TRUNC'))
     ]}.
