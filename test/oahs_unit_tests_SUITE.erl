-module(oahs_unit_tests_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").

-export([all/0]).
-export([
    is_oahs_test/1,
    add_test/1,
    remove_test/1,
    fold_test/1,
    filter_test/1,
    map_test/1,
    sum_test/1
]).

all() ->
    [
        is_oahs_test,
        add_test,
        remove_test,
        fold_test,
        filter_test,
        map_test,
        sum_test
    ].

is_oahs_test(_) ->
    Array = array:from_list(["a", "b", "c"]),
    Set = oahs:from_list(["a", "b", "c"]),
    ?assert(oahs:is_oahs(Set)),
    ?assertNot(oahs:is_oahs({Array, 3})),
    ?assertNot(oahs:is_oahs(3)).

add_test(_) ->
    Set = oahs:from_list([1, 2, 4]),
    ?assertNot(oahs:contains(Set, 3)),
    NewSet = oahs:add(Set, 3),
    ?assert(oahs:contains(NewSet, 3)).

remove_test(_) ->
    Set = oahs:from_list(["hello", "world", "!"]),
    ?assert(oahs:contains(Set, "!")),
    NewSet = oahs:remove(Set, "!"),
    ?assertNot(oahs:contains(NewSet, "!")).

fold_test(_) ->
    Set = oahs:from_list([1, 2, 3, 4]),
    Sum = oahs:fold(fun (Value, Acc) -> Acc + Value end, 0, Set),
    Prod = oahs:fold(fun (Value, Acc) -> Acc * Value end, 1, Set),
    Max = oahs:fold(
        fun (Value, Acc) ->
            case Value > Acc of
                true ->
                    Value;
                _ ->
                    Acc
            end
        end,
        0,
        Set
    ),
    ?assert(Sum =:= 10),
    ?assert(Prod =:= 24),
    ?assert(Max =:= 4).

filter_test(_) ->
    Set = oahs:from_list([-4, -1337, 322, 1234, 0]),
    Positive = oahs:from_list([0, 322, 1234]),
    ?assert(oahs:equal(oahs:filter(Set, fun (Value) -> Value >= 0 end), Positive)).

map_test(_) ->
    Set = oahs:from_list([[1, 2, 3], [0], [1000, 1]]),
    ToCompare = oahs:from_list([6, 0, 1001]),
    ?assert(oahs:equal(oahs:map(Set, fun (List) -> lists:sum(List) end), ToCompare)).

sum_test(_) ->
    Set1 = oahs:from_list([1, 3, 4]),
    Set2 = oahs:from_list([2, 3, 5]),
    Sum = oahs:from_list([1, 2, 3, 4, 5]),
    ?assert(oahs:equal(oahs:sum(Set1, Set2), Sum)).
