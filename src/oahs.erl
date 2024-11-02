-module(oahs).

-include("oahs.hrl").

-opaque oahs(Key) :: #oahs{array :: array:array(Key), used :: integer()}.
-export_type([oahs/1]).

-export([
    is_oahs/1,
    new/0,
    add/2,
    remove/2,
    contains/2,
    fold/3,
    filter/2,
    map/2,
    equal/2,
    sum/2,
    from_list/1,
    to_list/1
]).


is_oahs(#oahs{array = Array, used = Used}) ->
    array:is_array(Array) andalso is_integer(Used);
is_oahs(_) ->
    false.

new(Capacity) ->
    #oahs{array = array:new([{size, Capacity}, {fixed, true}]), used = 0}.
new() ->
    new(?INITIAL_CAPACITY).


probe_linear(Idx, _, _) ->
    Idx + 1.

probe_slots(Array, Value, Slot, ProbeFun, N) ->
    DefaultValue = array:default(Array),
    case array:get(Slot, Array) of
        DefaultValue ->
            {empty, Slot};
        Value ->
            {found, Slot};
        _ ->
            probe_slots(
                Array, Value,
                ProbeFun(Slot, Value, N) rem array:size(Array),
                ProbeFun, N + 1
            )
    end.
probe_slots(Array, Value, Slot, ProbeFun) ->
    probe_slots(Array, Value, Slot, ProbeFun, 0).


build_hash_fun(Array) ->
    fun(Value) -> erlang:phash2(Value, array:size(Array)) end.

find_slot(Array, Value, HashFun, ProbeFun) ->
    probe_slots(Array, Value, HashFun(Value), ProbeFun).
find_slot(Array, Value) ->
    find_slot(Array, Value, build_hash_fun(Array), fun probe_linear/3).


add_internal(#oahs{array = Array, used = Used}, Value) ->
    case find_slot(Array, Value) of
        {empty, Slot} ->
            #oahs{array = array:set(Slot, Value, Array), used = Used + 1};
        _ ->
            #oahs{array = Array, used = Used}
    end.

rehash(#oahs{array = OldArray}, #oahs{array = NewArray, used = NewUsed}, Slot) ->
    case Slot == array:size(OldArray) of
        true ->
            NewArray;
        _ ->
            DefaultValue = array:default(OldArray),
            rehash(
                OldArray,
                case array:get(Slot, OldArray) of
                    DefaultValue ->
                        NewArray;
                    Value ->
                        add_internal(#oahs{array = NewArray, used = NewUsed}, Value)
                end,
                Slot + 1
            )
    end.
rehash(OldArray, NewArray) ->
    rehash(OldArray, NewArray, 0).

grow(#oahs{array = Array, used = Used}) ->
    #oahs{
        array =
            case (Used / array:size(Array)) > ?LOAD_FACTOR of
                true ->
                    rehash(
                        Array,
                        array:new([
                            {size, array:size(Array) * ?GROW_FACTOR},
                            {fixed, true}
                        ])
                    );
                _ ->
                    Array
            end,
        used = Used
    }.


add(Set, Value) ->
    add_internal(grow(Set), Value).


remove(#oahs{array = Array, used = Used}, Value) ->
    case find_slot(Array, Value) of
        {found, Slot} ->
            #oahs{array = array:reset(Slot, Array), used = Used - 1};
        _ ->
            #oahs{array = Array, used = Used}
    end.


contains(#oahs{array = Array, used = _}, Value) ->
    case find_slot(Array, Value) of
        {found, _} ->
            true;
        _ ->
            false
    end.


% direction isn't matter
fold(Fun, Acc, #oahs{array = Array, used = _}) ->
    array:sparse_foldl(
        fun (_, Value, Acc1) -> Fun(Value, Acc1) end,
        Acc, Array
    ).


build_fold_filter_fun(Pred) ->
    fun(Value, Acc) ->
        case Pred(Value) of
            true ->
                add(Acc, Value);
            _ ->
                Acc
        end
    end.

filter(Set, Pred) ->
    #oahs{array = Array} = Set,
    fold(build_fold_filter_fun(Pred), new(array:size(Array)), Set).


build_fold_map_fun(Fun) ->
    fun (Value, Acc) -> add(Acc, Fun(Value)) end.

map(Set, Fun) ->
    fold(build_fold_map_fun(Fun), new(), Set).


build_fold_equal_fun(Set2) ->
    fun (Value, Acc) -> Acc andalso contains(Set2, Value) end.

equal(Set1, Set2) ->
    fold(build_fold_equal_fun(Set2), true, Set1).


fold_add_fun(Value, Acc) ->
    add(Acc, Value).

sum(Set1, Set2) ->
    fold(fun fold_add_fun/2, fold(fun fold_add_fun/2, new(), Set1), Set2).


from_list(List) ->
    lists:foldl(fun fold_add_fun/2, new(), List).

to_list(Set) ->
    fold(fun (Value, Acc) -> [Value | Acc] end, [], Set).
