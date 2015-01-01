-module(ord_stat).

-export([
         rank/2
        ]).

rank(1, [X]) ->
    X;
rank(I, _) when I =< 0 ->
    erlang:error({wrong_input_rank, I});
rank(I, []) ->
    erlang:error(empty_list, [I]);
rank(I, [_] = List) ->
    erlang:error(too_short_list, [I, List]);
rank(I, List) ->
    {Pivot, Smaller, Bigger} = rand_partition(List),
    K = length(Smaller), %% FIXME: calculate length during partitioning
    if I == K ->
            Pivot;
       I < K ->
            rank(I, Smaller);
       true ->
            rank(I - K, Bigger)
    end.

rand_partition(List) ->
    Pivot = get_pivot(List),
    F = fun(X) ->
                X =< Pivot
        end,
    {Smaller, Bigger} = lists:partition(F, List),
    {Pivot, Smaller, Bigger}.

get_pivot(List) ->
    %% FIXME: calculate length in the beginning
    Idx = random:uniform(length(List)),
    lists:nth(Idx, List).

