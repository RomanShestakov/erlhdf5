-module(erlh5s_tests).
-compile([export_all]).
-include_lib("eunit/include/eunit.hrl").
-include("../include/erlhdf5.hrl").

erlhdf5_prop_list_create_test_() ->
    {setup, local,
     fun create/0,
     fun close/1,
     fun run/1}.

create() ->
    {ok, H} = erlhdf5:h5screate_simple(2, {4, 7}),
    H.

close(H) ->
    ok = erlhdf5:h5sclose(H).

run(_P) ->
    [
     %?_assertMatch({ok, _}, erlhdf5:h5screate(2, {2, 2}))
     ?_assertMatch({ok, _}, {ok, ok})
    ].
