-module(erlhdf5_tests).
-compile([export_all]).
-include_lib("eunit/include/eunit.hrl").
-include("../include/erlhdf5.hrl").

hdf5_test_() ->
    {inorder,
     [
      %% test file creation
      ?_assertMatch({ok, _}, erlhdf5:h5fcreate("test_file_hdf5.h5", 'H5F_ACC_TRUNC')),
      ?_assertMatch({error, _}, erlhdf5:h5fcreate("test_file_hdf5.h5", 'H5F_ACC_EXCL')),
      ?_assertMatch({error, _}, erlhdf5:h5fcreate("test_file_hdf5.h5", unknown_flag)),
      ?_assertMatch({error, _}, erlhdf5:h5fcreate(incorrect_file_name, 'H5F_ACC_TRUNC')),
      %% test existing file
      ?_assertMatch({ok, _}, erlhdf5:h5fopen("test_file_hdf5.h5", 'H5F_ACC_RDWR')),
      ?_assertMatch({error, _}, erlhdf5:h5fopen("unknown_name", 'H5F_ACC_RDWR')),
      ?_assertMatch({ok, _}, erlhdf5:h5fopen("test_file_hdf5.h5", 'H5F_ACC_RDONLY')),
      ?_assertMatch({error, _}, erlhdf5:h5fopen("test_file_hdf5.h5", 'unknown_flag'))
      %% test file close
      %% ?_assertMatch(ok, erlhdf5:h5fclose(erlhdf5:h5fopen("test_file_hdf5.h5", 'H5F_ACC_RDONLY'))),
      %% ?_assertMatch({error, _}, erlhdf5:h5fclose(erlhdf5:h5fopen("unknown_file", 'H5F_ACC_RDONLY')))
     ]}.
