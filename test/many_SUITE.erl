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
    N = compare_with_sort_till_timeout(Config, Dur),
    ct:pal("compare acc: ~p", [N]),
    ok.

timing(Config) ->
    set_timeout(Config),
    Dur = get_duration(Config),
    Data = timing_till_timeout(Config, Dur),
    ct:pal("timing data:~n~p", [dict:to_list(Data)]),
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
    compare_with_sort_till_timeout(Config, Cur, Stop, {0, 0}).

compare_with_sort_till_timeout(_, Cur, Stop, Acc) when Cur > Stop ->
    Acc;
compare_with_sort_till_timeout(Config, _, Stop, {Rounds, Lengths}) ->
    Lengths_round = compare_with_sort_one_round(Config),
    compare_with_sort_till_timeout(Config, timestamp(), Stop,
                                   {Rounds + 1, Lengths + Lengths_round}).

compare_with_sort_one_round(Config) ->
    {Min, Max, Step} = get_local_value(length, Config),
    Prob = get_local_value(probability_of_repeat, Config),
    compare_with_sort_one_round(Min, Max, Step, Prob, 0).

compare_with_sort_one_round(Cur, Max, _, _, Acc) when Cur > Max ->
    Acc;
compare_with_sort_one_round(Cur, Max, Step, Prob, Lengths) ->
    compare_with_sort_one_item(Cur, Max, Prob),
    Next = calc_next_length(Cur, Step),
    ct:log("round, acc=~p, cur=~p", [Lengths, Cur]),
    compare_with_sort_one_round(Next, Max, Step, Prob, Lengths + Cur).

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

calc_next_length(Cur, {add, N}) ->
    Cur + N;
calc_next_length(Cur, {mult, N}) ->
    round(Cur * N).

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

timing_till_timeout(Config, Dur) ->
    Cur = timestamp(),
    Stop = Cur + Dur - 1,
    timing_till_timeout(Config, Cur, Stop, dict:new()).

timing_till_timeout(_, Cur, Stop, Acc) when Cur > Stop ->
    Acc;
timing_till_timeout(Config, _, Stop, Acc) ->
    New_acc = timing_one_round(Config, Acc),
    timing_till_timeout(Config, timestamp(), Stop, New_acc).

timing_one_round(Config, Acc) ->
    {Min, Max, Step} = get_local_value(length, Config),
    timing_one_round_of_length(Min, Max, Step, Config, Acc).

timing_one_round_of_length(Cur, Max, _, _, Acc) when Cur > Max ->
    Acc;
timing_one_round_of_length(Cur, Max, Step, Config, Acc) ->
    Results = timing_one_round_of_reqs(Cur, Max, Config),
    New_acc = add_results_one_length(Cur, Results, Acc),
    Next = calc_next_length(Cur, Step),
    timing_one_round_of_length(Next, Max, Step, Config, New_acc).

timing_one_round_of_reqs(Len, Max, Config) ->
    {Nstart, Nstop, Nstep} = get_local_value(requests, Config),
    timing_one_round_of_reqs(Nstart, Nstop, Nstep, Len, Max, Config,
                             dict:new()).

timing_one_round_of_reqs(Cur, Stop, _, _, _, _, Acc) when Cur > Stop ->
    Acc;
timing_one_round_of_reqs(Cur, Stop, Step, Len, Lmax, Config, Acc) ->
    Cur_result = timing_one_item_n_times(Cur, Len, Lmax, Config),
    New_acc = add_one_item_results_to_acc(Cur, Cur_result, Acc),
    Next = calc_next_length(Cur, Step),
    timing_one_round_of_reqs(Next, Stop, Step, Len, Lmax, Config, New_acc).

timing_one_item_n_times(Cur, Len, Max, Config) ->
    Prob = get_local_value(probability_of_repeat, Config),
    Nrepeats = get_local_value(one_round_repeats, Config),
    [{Dsort, Dstat} = timing_one_item(Cur, Len, Max, Prob)
     || _ <- lists:seq(1, Nrepeats)].

timing_one_item(Nreqs, Len, Max, Prob) when Nreqs > Len ->
    timing_one_item(Len, Len, Max, Prob);
timing_one_item(Nreqs, Len, Max, Prob) ->
    Data = gen_data(Len, Max, Prob),
    Reqs = prepare_requests(Len, Nreqs),
    T1 = os:timestamp(),
    By_sort = get_timing_and_items_by_sort(Reqs, Data),
    T2 = os:timestamp(),
    By_stat = get_timing_and_items_by_stat(Reqs, Data),
    T3 = os:timestamp(),
    case By_stat of
        By_sort ->
            Dsort = timer:now_diff(T2, T1),
            Dstat = timer:now_diff(T3, T2),
            {Dsort, Dstat};
        _ ->
            ct:log("timing data mismatch, len=~p~n"
                   "by sort:~n~p~nby stat:~n~p~n",
                   [Len, By_sort, By_stat]),
            erlang:error(data_mismatch)
    end.

prepare_requests(Len, Nreqs) ->
    Ratio = trunc(Len / Nreqs),
    L = lists:seq(Len, 1, -Ratio),
    lists:sublist(L, 1, Nreqs).

get_timing_and_items_by_sort(Reqs, Data) ->
    Sorted = lists:sort(Data),
    [lists:nth(Idx, Sorted) || Idx <- Reqs].

get_timing_and_items_by_stat(Reqs, Data) ->
    [ord_stat:rank(Idx, Data) || Idx <- Reqs].

add_results_one_length(Len, Results, Acc) ->
    %% Acc :: len -> dict()
    %% Results :: nreqs -> [{sort, stat}]
    case dict:find(Len, Acc) of
        {ok, Prev} ->
            Upd = merge_results_by_nreq(Results, Prev),
            dict:store(Len, Upd, Acc);
        error ->
            dict:store(Len, Results, Acc)
    end.

merge_results_by_nreq(Results, Prev) ->
    %% Results, Prev :: nreqs -> [{sort, stat}]
    F = fun(_Nreq, L1, L2) ->
                L1 ++ L2
        end,
    dict:merge(F, Results, Prev).

add_one_item_results_to_acc(Nreqs, Result, Acc) ->
    dict:store(Nreqs, Result, Acc).

