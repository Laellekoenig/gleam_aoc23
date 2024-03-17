-module(day2).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch]).

-export([main/0]).

-spec get_game_id(binary()) -> {ok, integer()} | {error, nil}.
get_game_id(Game) ->
    Start = begin
        _pipe = Game,
        _pipe@1 = gleam@string:split(_pipe, <<":"/utf8>>),
        gleam@list:first(_pipe@1)
    end,
    String_id = case Start of
        {ok, <<"Game "/utf8, Id/binary>>} ->
            {ok, Id};

        _ ->
            {error, nil}
    end,
    case String_id of
        {ok, X} ->
            gleam@int:parse(X);

        _ ->
            {error, nil}
    end.

-spec to_ints(list(binary())) -> list({ok, integer()} | {error, nil}).
to_ints(Lst) ->
    _pipe = Lst,
    _pipe@1 = gleam@list:map(
        _pipe,
        fun(X) -> gleam@string:split(X, <<" "/utf8>>) end
    ),
    _pipe@2 = gleam@list:map(_pipe@1, fun gleam@list:first/1),
    _pipe@3 = gleam@list:map(
        _pipe@2,
        fun(X@1) -> gleam@result:map(X@1, fun gleam@int:parse/1) end
    ),
    gleam@list:map(_pipe@3, fun gleam@result:flatten/1).

-spec get_max_helper(list({ok, integer()} | {error, nil}), integer()) -> integer().
get_max_helper(Lst, Curr_max) ->
    case Lst of
        [] ->
            Curr_max;

        [{error, _}] ->
            Curr_max;

        [{ok, X}] when X > Curr_max ->
            X;

        [{ok, X@1}] when X@1 =< Curr_max ->
            Curr_max;

        [{ok, X@2} | Rest] when X@2 > Curr_max ->
            get_max_helper(Rest, X@2);

        [{ok, _} | Rest@1] ->
            get_max_helper(Rest@1, Curr_max);

        [{error, _} | Rest@2] ->
            get_max_helper(Rest@2, Curr_max)
    end.

-spec get_max(list({ok, integer()} | {error, nil})) -> {ok, integer()} |
    {error, nil}.
get_max(Lst) ->
    case Lst of
        [] ->
            {error, nil};

        [{ok, X}] ->
            {ok, X};

        [{error, _}] ->
            {error, nil};

        [{ok, X@1} | Rest] ->
            {ok, get_max_helper(Rest, X@1)};

        [{error, _} | Rest@1] ->
            get_max(Rest@1)
    end.

-spec get_power(binary()) -> {ok, integer()} | {error, nil}.
get_power(Game) ->
    Cubes = begin
        _pipe = Game,
        _pipe@1 = gleam@string:split(_pipe, <<": "/utf8>>),
        _pipe@2 = gleam@list:last(_pipe@1),
        _pipe@3 = gleam@result:map(
            _pipe@2,
            fun(_capture) -> gleam@string:split(_capture, <<"; "/utf8>>) end
        ),
        _pipe@4 = gleam@result:map(
            _pipe@3,
            fun(_capture@1) ->
                gleam@list:map(
                    _capture@1,
                    fun(X) -> gleam@string:split(X, <<", "/utf8>>) end
                )
            end
        ),
        gleam@result:map(
            _pipe@4,
            fun(_capture@2) -> gleam@list:flatten(_capture@2) end
        )
    end,
    Green_cubes = begin
        _pipe@5 = Cubes,
        _pipe@6 = gleam@result:map(
            _pipe@5,
            fun(_capture@3) ->
                gleam@list:filter(
                    _capture@3,
                    fun(X@1) ->
                        gleam_stdlib:contains_string(X@1, <<"green"/utf8>>)
                    end
                )
            end
        ),
        _pipe@7 = gleam@result:map(_pipe@6, fun to_ints/1),
        _pipe@8 = gleam@result:map(_pipe@7, fun get_max/1),
        gleam@result:flatten(_pipe@8)
    end,
    Blue_cubes = begin
        _pipe@9 = Cubes,
        _pipe@10 = gleam@result:map(
            _pipe@9,
            fun(_capture@4) ->
                gleam@list:filter(
                    _capture@4,
                    fun(X@2) ->
                        gleam_stdlib:contains_string(X@2, <<"blue"/utf8>>)
                    end
                )
            end
        ),
        _pipe@11 = gleam@result:map(_pipe@10, fun to_ints/1),
        _pipe@12 = gleam@result:map(_pipe@11, fun get_max/1),
        gleam@result:flatten(_pipe@12)
    end,
    Red_cubes = begin
        _pipe@13 = Cubes,
        _pipe@14 = gleam@result:map(
            _pipe@13,
            fun(_capture@5) ->
                gleam@list:filter(
                    _capture@5,
                    fun(X@3) ->
                        gleam_stdlib:contains_string(X@3, <<"red"/utf8>>)
                    end
                )
            end
        ),
        _pipe@15 = gleam@result:map(_pipe@14, fun to_ints/1),
        _pipe@16 = gleam@result:map(_pipe@15, fun get_max/1),
        gleam@result:flatten(_pipe@16)
    end,
    case {Green_cubes, Blue_cubes, Red_cubes} of
        {{ok, X@4}, {ok, Y}, {ok, Z}} ->
            {ok, (X@4 * Y) * Z};

        {_, _, _} ->
            {error, nil}
    end.

-spec is_legal(binary()) -> boolean().
is_legal(S) ->
    Split = begin
        _pipe = S,
        gleam@string:split(_pipe, <<" "/utf8>>)
    end,
    Number = begin
        _pipe@1 = Split,
        _pipe@2 = gleam@list:first(_pipe@1),
        _pipe@3 = gleam@result:map(_pipe@2, fun gleam@int:parse/1),
        gleam@result:flatten(_pipe@3)
    end,
    Color = begin
        _pipe@4 = Split,
        gleam@list:last(_pipe@4)
    end,
    case {Number, Color} of
        {{ok, X}, {ok, <<"green"/utf8>>}} ->
            X =< 13;

        {{ok, X@1}, {ok, <<"red"/utf8>>}} ->
            X@1 =< 12;

        {{ok, X@2}, {ok, <<"blue"/utf8>>}} ->
            X@2 =< 14;

        {_, _} ->
            false
    end.

-spec is_legal_subgame(binary()) -> boolean().
is_legal_subgame(Subgame) ->
    _pipe = Subgame,
    _pipe@1 = gleam@string:split(_pipe, <<", "/utf8>>),
    gleam@list:all(_pipe@1, fun is_legal/1).

-spec is_legal_game(binary()) -> boolean().
is_legal_game(Game) ->
    All_legal = begin
        _pipe = Game,
        _pipe@1 = gleam@string:split(_pipe, <<": "/utf8>>),
        _pipe@2 = gleam@list:last(_pipe@1),
        _pipe@3 = gleam@result:map(
            _pipe@2,
            fun(_capture) -> gleam@string:split(_capture, <<"; "/utf8>>) end
        ),
        gleam@result:map(
            _pipe@3,
            fun(_capture@1) ->
                gleam@list:all(_capture@1, fun(X) -> is_legal_subgame(X) end)
            end
        )
    end,
    case All_legal of
        {ok, X@1} ->
            X@1;

        _ ->
            false
    end.

-spec add_game(binary()) -> integer().
add_game(Game) ->
    Id = begin
        _pipe = Game,
        get_game_id(_pipe)
    end,
    Is_legal = begin
        _pipe@1 = Game,
        is_legal_game(_pipe@1)
    end,
    case {Id, Is_legal} of
        {{ok, X}, true} ->
            X;

        {_, _} ->
            0
    end.

-spec main() -> nil.
main() ->
    Input = begin
        _pipe = <<"input.txt"/utf8>>,
        _pipe@1 = simplifile:read(_pipe),
        _pipe@2 = gleam@result:map(
            _pipe@1,
            fun(_capture) -> gleam@string:split(_capture, <<"\n"/utf8>>) end
        ),
        gleam@result:map(
            _pipe@2,
            fun(_capture@1) ->
                gleam@list:filter(_capture@1, fun(X) -> X /= <<""/utf8>> end)
            end
        )
    end,
    Sum = begin
        _pipe@3 = Input,
        gleam@result:map(
            _pipe@3,
            fun(_capture@2) ->
                gleam@list:fold(
                    _capture@2,
                    0,
                    fun(Acc, Game) -> add_game(Game) + Acc end
                )
            end
        )
    end,
    gleam@io:print(<<"Part 1: "/utf8>>),
    case Sum of
        {ok, X@1} ->
            gleam@io:println(gleam@int:to_string(X@1));

        _ ->
            gleam@io:println(<<"Error"/utf8>>)
    end,
    Power_sum = begin
        _pipe@4 = Input,
        gleam@result:map(
            _pipe@4,
            fun(_capture@3) ->
                gleam@list:fold(
                    _capture@3,
                    0,
                    fun(Acc@1, Game@1) ->
                        gleam@result:unwrap(get_power(Game@1), 0) + Acc@1
                    end
                )
            end
        )
    end,
    gleam@io:print(<<"Part 2: "/utf8>>),
    case Power_sum of
        {ok, X@2} ->
            gleam@io:println(gleam@int:to_string(X@2));

        _ ->
            gleam@io:println(<<"Error"/utf8>>)
    end.
