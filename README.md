Передрий Михаил Сергеевич, P34102 \
Вариант: oa-set
=====
# Цель
Освоиться с построением пользовательских типов данных, полиморфизмом, рекурсивными алгоритмами и средствами тестирования (unit testing, property-based testing).

# Требования
Необходимо реализовать множество на хэш-таблице.

1. Функции:
    - добавление и удаление элементов;
    - фильтрация;
    - отображение (map);
    - свертки (левая и правая);
    - структура должна быть [моноидом](https://ru.m.wikipedia.org/wiki/Моноид).
2. Структуры данных должны быть неизменяемыми.
3. Библиотека должна быть протестирована в рамках unit testing.
4. Библиотека должна быть протестирована в рамках property-based тестирования (как минимум 3 свойства, включая свойства моноида).
5. Структура должна быть полиморфной.
6. Требуется использовать идиоматичный для технологии стиль программирования.

# Реализация
Реализация основывается на типе array, встроенном в Erlang.

### Тип
```erlang
-opaque oahs(Key) :: #oahs{array :: array:array(Key), used :: integer()}.
```

### Добавление элемента
Разделено на 2 функции, т.к. `add_internal` также используется в других функциях.

`grow` описана далее.
```erlang
add_internal(#oahs{array = Array, used = Used}, Value) ->
    case find_slot(Array, Value) of
        {empty, Slot} ->
            #oahs{array = array:set(Slot, Value, Array), used = Used + 1};
        _ ->
            #oahs{array = Array, used = Used}
    end.

add(Set, Value) ->
    add_internal(grow(Set), Value).
```

### Расширение хэш-таблицы
Расширяем таблицу (массив), если загрузка таблицы превысила `?LOAD_FACTOR`.
```erlang
grow(#oahs{array = Array, used = Used}) ->
    #oahs{
        array =
            case (Used / array:size(Array)) > ?LOAD_FACTOR of
                true ->
                    rehash(
                        #oahs{array = Array, used = Used},
                        new(array:size(Array) * ?GROW_FACTOR)
                    );
                _ ->
                    Array
            end,
        used = Used
    }.
```

### Удаление элемента
```erlang
remove(#oahs{array = Array, used = Used}, Value) ->
    case find_slot(Array, Value) of
        {found, Slot} ->
            #oahs{array = array:reset(Slot, Array), used = Used - 1};
        _ ->
            #oahs{array = Array, used = Used}
    end.
```

### Свертка
Операция свертки, по сути, является оберткой над операцией свертки для массива.

Направление свертки не имеет значения, т.к. хэш-таблица не сохраняет порядок операций.

```erlang
fold(Fun, Acc, #oahs{array = Array, used = _}) ->
    array:sparse_foldl(
        fun (_, Value, Acc1) -> Fun(Value, Acc1) end,
        Acc, Array
    ).
```

Дальнейшие операции легко выражаются через свертку.

### Фильтрация
```erlang
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
```

### Отображение
```erlang
build_fold_map_fun(Fun) ->
    fun (Value, Acc) -> add(Acc, Fun(Value)) end.

map(Set, Fun) ->
    fold(build_fold_map_fun(Fun), new(), Set).
```

### Отображение
```erlang
```

### Отображение
```erlang
```