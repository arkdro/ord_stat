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

%% ===================================================================
%% Internal functions
%% ===================================================================

