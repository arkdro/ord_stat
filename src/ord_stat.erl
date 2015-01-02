%%%
%%% Order statistics. Randomized divide and conquer. Expected linear time.
%%% [see CLRS]
%%%
%%% The original algorithm requires unique items in the input data.
%%% To handle any input items, the program implements a custom partitioning
%%% function, that returns an additional info: all-items-are-same flag
%%% for every part of the result. It also returns a length as a small
%%% optimization.
%%%

-module(ord_stat).

-export([
         rank/2
        ]).

rank(I, List) ->
    Len = length(List),
    if
        I =< 0 ->
            erlang:error({wrong_input_rank, I});
        I > Len ->
            erlang:error({too_big_input_rank, [I, Len]});
        true ->
            rank2(I, {false, Len, List})
    end.

rank2(I, {_, _, []}) ->
    erlang:error({empty_list, I});
rank2(_, {true, _, [H | _]}) ->
    H;
rank2(1, {_, _, List}) ->
    lists:min(List);
rank2(I, {_, _, [_] = List}) ->
    erlang:error({too_short_list, [I, List]});
rank2(I, {_, Len, List}) ->
    {Pivot, Smaller, Bigger} = rand_partition(Len, List),
    {_, K, _} = Smaller,
    if I == K ->
            Pivot;
       I < K ->
            rank2(I, Smaller);
       true ->
            rank2(I - K, Bigger)
    end.

rand_partition(Len, List) ->
    Pivot = get_pivot(Len, List),
    F = fun(X) ->
                X =< Pivot
        end,
    {Smaller, Bigger} = partition(F, List),
    {Pivot, Smaller, Bigger}.

get_pivot(Len, List) ->
    Idx = random:uniform(Len),
    lists:nth(Idx, List).

-spec partition(F, List) -> Result when
      F :: fun((Elem :: T) -> boolean()),
      List :: [T],
      Result :: {{Same_items_flag, Length, Satisfying},
                 {Same_items_flag, Length, Not_satisfying}},
      Same_items_flag :: boolean(),
      Length :: non_neg_integer(),
      Satisfying :: [T],
      Not_satisfying :: [T].

partition(F, List) ->
    partition(F, List, {true, 0, []}, {true, 0, []}).

partition(_, [], {All_same_a, Len_a, Acc_a}, {All_same_b, Len_b, Acc_b}) ->
    {{All_same_a, Len_a, lists:reverse(Acc_a)},
     {All_same_b, Len_b, lists:reverse(Acc_b)}};
partition(F, [H | T],
          {All_same_a, Len_a, Acc_a} = Sat,
          {All_same_b, Len_b, Acc_b} = Not_sat) ->
    case F(H) of
        true ->
            New_same = calc_same_flag(All_same_a, H, Acc_a),
            partition(F, T, {New_same, Len_a + 1, [H | Acc_a]}, Not_sat);
        false ->
            New_same = calc_same_flag(All_same_b, H, Acc_b),
            partition(F, T, Sat, {New_same, Len_b + 1, [H | Acc_b]})
    end.

calc_same_flag(Flag, _, []) ->
    Flag;
calc_same_flag(Flag, H, [H | _]) ->
    Flag;
calc_same_flag(_, _, _) ->
    false.

