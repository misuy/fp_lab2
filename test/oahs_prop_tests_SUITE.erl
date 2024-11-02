-module(oahs_prop_tests_SUITE).

-include_lib("stdlib/include/assert.hrl").
-include_lib("proper/include/proper.hrl").

-define(PROPERTY_TESTS_CNT, 200).

-export([all/0, prop_test/1]).

all() ->
    [prop_test].

prop_test(_) ->
    ?assert(prop_check(prop_sum_associativity())),
    ?assert(prop_check(prop_sum_neutral())),
    ?assert(prop_check(prop_add())),
    ?assert(prop_check(prop_remove())).

prop_check(Property) ->
    proper:quickcheck(Property, [{numtests, ?PROPERTY_TESTS_CNT}, {to_file, user}]).


prop_sum_associativity() ->
    ?FORALL({List1, List2, List3}, {list(integer()), list(integer()), list(integer())},
        case 1 of
            2 ->
                false;
            _ ->
                Set1 = oahs:from_list(List1),
                Set2 = oahs:from_list(List2),
                Set3 = oahs:from_list(List3),
                oahs:equal(
                    oahs:sum(Set1, oahs:sum(Set2, Set3)),
                    oahs:sum(oahs:sum(Set1, Set2), Set3)
                )
        end
    ).

prop_sum_neutral() ->
    ?FORALL(List, list(integer()),
        case 1 of
            2 ->
                false;
            _ ->
                Set = oahs:from_list(List),
                Neutral = oahs:new(),
                oahs:equal(oahs:sum(Set, Neutral), Set) andalso
                    oahs:equal(oahs:sum(Neutral, Set), Set)
        end
    ).

prop_add() ->
    ?FORALL({List, Value}, {list(integer()), integer()},
        oahs:contains(oahs:add(oahs:from_list(List), Value), Value)
    ).

prop_remove() ->
    ?FORALL({List, Value}, {list(integer()), integer()},
        oahs:contains(oahs:remove(oahs:from_list(List), Value), Value) =:= false
    ).
