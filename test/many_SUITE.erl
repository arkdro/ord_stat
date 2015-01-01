-module(many_SUITE).

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
                {group, compare}
               ]},
     {compare, [parallel], [
                            compare_with_sort %% check correctness
                           ]}
    ].

init_per_suite(Config) ->
    random:seed(now()),
    Local = local_config(Config),
    [{local, Local} | Config].

end_per_suite(_Config) ->
    ok.

compare_with_sort(Config) ->
    set_timeout(Config),
    Dur = get_duration(Config),
    compare_with_sort_till_timeout(Config, Dur),
    ok.

%% ===================================================================
%% Internal functions
%% ===================================================================

get_local_value(Key, Config) ->
    Local = proplists:get_value(local, Config),
    proplists:get_value(Key, Local).

set_timeout(Config) ->
    Dur = get_duration(Config),
    T = {seconds, Dur + 300},
    ct:pal("timetrap: ~p", [T]),
    ct:timetrap(T).

get_duration(Config) ->
    case get_local_value(duration, Config) of
        undefined ->
            30;
        Val ->
            Val
    end.

local_config(Config) ->
    File = "local.conf",
    Dir = ?config(data_dir, Config),
    Path = filename:join([Dir, File]),
    {ok, [L]} = file:consult(Path),
    L.

compare_with_sort_till_timeout(Config, Dur) ->
    Cur = timestamp(),
    Stop = Cur + Dur - 1,
    compare_with_sort_till_timeout(Config, Cur, Stop).

compare_with_sort_till_timeout(_, Cur, Stop) when Cur > Stop ->
    ok;
compare_with_sort_till_timeout(Config, _, Stop) ->
    compare_with_sort_one_round(Config),
    compare_with_sort_till_timeout(Config, timestamp(), Stop).

compare_with_sort_one_round(Config) ->
    {Min, Max, Step} = get_local_value(length, Config),
    Prob = get_local_value(probability_of_repeat, Config),
    compare_with_sort_one_round(Min, Max, Step, Prob).

compare_with_sort_one_round(Cur, Max, _, _) when Cur > Max ->
    ok;
compare_with_sort_one_round(Cur, Max, Step, Prob) ->
    compare_with_sort_one_item(Cur, Max, Prob),
    compare_with_sort_one_round(Cur + Step, Max, Step, Prob).

compare_with_sort_one_item(Len, Max, Prob) ->
    Data = gen_data(Len, Max, Prob),
    By_sort = get_items_by_sort(Data),
    By_stat = get_items_by_stat(Len, Data),
    case By_stat of
        By_sort ->
            ok;
        _ ->
            ct:log("data mismatch, len=~p~nby sort:~n~p~nby stat:~n~p~n",
                   [Len, By_sort, By_stat]),
            erlang:error(data_mismatch)
    end.

timestamp() ->
    timestamp(os:timestamp()).

timestamp({MS, S, _}) ->
    MS * 1000000 + S.

gen_data(Len, Max, Prob) ->
    gen_data(Len, Max, Prob, []).

gen_data(0, _, _, Acc) ->
    Acc;
gen_data(Len, Max, Prob, []) ->
    X = random:uniform(Max),
    gen_data(Len - 1, Max, Prob, [X]);
gen_data(Len, Max, Prob, [Prev | _] = Acc) ->
    X = gen_value(Max, Prob, Prev),
    gen_data(Len - 1, Max, Prob, [X | Acc]).

gen_value(Max, Prob, Prev) ->
    case random:uniform() < Prob of
        true ->
            Prev;
        false ->
            random:uniform(Max)
    end.

get_items_by_sort(Data) ->
    lists:sort(Data).

get_items_by_stat(Len, Data) ->
    Indices = lists:seq(1, Len),
    [ord_stat:rank(Idx, Data) || Idx <- Indices].

