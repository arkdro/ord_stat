-module(ord_stat_SUITE).

-include_lib("eunit/include/eunit.hrl").
-include_lib("common_test/include/ct.hrl").

-define(ASSERT, true).

-compile([export_all]).

suite() ->
    [
     {timetrap, {seconds, 180}}
    ].

all() ->
    [
     {group, all}
    ].

groups() ->
    [
     {all, [], [
                {group, read}
               ]},
     {read, [], [
                 rank2,
                 rank1
                ]}
    ].

init_per_suite(Config) ->
    Config.

end_per_suite(_Config) ->
    ok.

rank1(_C) ->
    L = [22, 33, 11],
    ?assertMatch(11, ord_stat:rank(1, L)),
    ?assertMatch(22, ord_stat:rank(2, L)),
    ?assertMatch(33, ord_stat:rank(3, L)),
    ok.

rank2(_C) ->
    ?assertError({wrong_input_rank, 0}, ord_stat:rank(0, [1, 2])),
    ?assertError({too_short_list, [2, [22]]}, ord_stat:rank(3, [11, 22])),
    ok.

%% ===================================================================
%% Internal functions
%% ===================================================================

