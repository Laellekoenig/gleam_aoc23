-module(day1).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch]).

-export([main/0]).
-export_type([get_left_error/0]).

-type get_left_error() :: no_number_error.

-spec get_left(binary(), boolean()) -> {ok, integer()} |
    {error, get_left_error()}.
get_left(S, Reversed) ->
    case {S, Reversed} of
        {<<"0"/utf8, _/binary>>, _} ->
            {ok, 0};

        {<<"1"/utf8, _/binary>>, _} ->
            {ok, 1};

        {<<"2"/utf8, _/binary>>, _} ->
            {ok, 2};

        {<<"3"/utf8, _/binary>>, _} ->
            {ok, 3};

        {<<"4"/utf8, _/binary>>, _} ->
            {ok, 4};

        {<<"5"/utf8, _/binary>>, _} ->
            {ok, 5};

        {<<"6"/utf8, _/binary>>, _} ->
            {ok, 6};

        {<<"7"/utf8, _/binary>>, _} ->
            {ok, 7};

        {<<"8"/utf8, _/binary>>, _} ->
            {ok, 8};

        {<<"9"/utf8, _/binary>>, _} ->
            {ok, 9};

        {<<"one"/utf8, _/binary>>, false} ->
            {ok, 1};

        {<<"two"/utf8, _/binary>>, false} ->
            {ok, 2};

        {<<"three"/utf8, _/binary>>, false} ->
            {ok, 3};

        {<<"four"/utf8, _/binary>>, false} ->
            {ok, 4};

        {<<"five"/utf8, _/binary>>, false} ->
            {ok, 5};

        {<<"six"/utf8, _/binary>>, false} ->
            {ok, 6};

        {<<"seven"/utf8, _/binary>>, false} ->
            {ok, 7};

        {<<"eight"/utf8, _/binary>>, false} ->
            {ok, 8};

        {<<"nine"/utf8, _/binary>>, false} ->
            {ok, 9};

        {<<"eno"/utf8, _/binary>>, true} ->
            {ok, 1};

        {<<"owt"/utf8, _/binary>>, true} ->
            {ok, 2};

        {<<"eerht"/utf8, _/binary>>, true} ->
            {ok, 3};

        {<<"ruof"/utf8, _/binary>>, true} ->
            {ok, 4};

        {<<"evif"/utf8, _/binary>>, true} ->
            {ok, 5};

        {<<"xis"/utf8, _/binary>>, true} ->
            {ok, 6};

        {<<"neves"/utf8, _/binary>>, true} ->
            {ok, 7};

        {<<"thgie"/utf8, _/binary>>, true} ->
            {ok, 8};

        {<<"enin"/utf8, _/binary>>, true} ->
            {ok, 9};

        {<<""/utf8>>, _} ->
            {error, no_number_error};

        {_, _} ->
            get_left(gleam@string:drop_left(S, 1), Reversed)
    end.

-spec get_number(binary()) -> {ok, integer()} | {error, get_left_error()}.
get_number(S) ->
    Left = begin
        _pipe = S,
        get_left(_pipe, false)
    end,
    Right = begin
        _pipe@1 = S,
        _pipe@2 = gleam@string:reverse(_pipe@1),
        get_left(_pipe@2, true)
    end,
    case {Left, Right} of
        {{ok, Left@1}, {ok, Right@1}} ->
            {ok, (Left@1 * 10) + Right@1};

        {_, _} ->
            {error, no_number_error}
    end.

-spec main() -> nil.
main() ->
    Result = begin
        _pipe = <<"input.txt"/utf8>>,
        _pipe@1 = simplifile:read(_pipe),
        _pipe@2 = gleam@result:map(
            _pipe@1,
            fun(_capture) -> gleam@string:split(_capture, <<"\n"/utf8>>) end
        ),
        _pipe@3 = gleam@result:map(
            _pipe@2,
            fun(_capture@1) ->
                gleam@list:filter(_capture@1, fun(X) -> X /= <<""/utf8>> end)
            end
        ),
        _pipe@4 = gleam@result:map(
            _pipe@3,
            fun(_capture@2) -> gleam@list:map(_capture@2, fun get_number/1) end
        ),
        gleam@result:map(
            _pipe@4,
            fun(_capture@3) ->
                gleam@list:fold(_capture@3, 0, fun(X@1, Y) -> case {X@1, Y} of
                            {_, {ok, Z}} ->
                                X@1 + Z;

                            {_, _} ->
                                X@1
                        end end)
            end
        )
    end,
    case Result of
        {ok, X@2} ->
            gleam@io:println(gleam@int:to_string(X@2));

        {error, _} ->
            gleam@io:println(<<"Error when parsing strings"/utf8>>)
    end.
