%%%-------------------------------------------------------------------
%%% @author snorri
%%% @copyright (C) 2017, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 30. Sep 2017 15:55
%%%-------------------------------------------------------------------
-module(tokens).
-author("snorri").

%% API
-export([tokenize/1]).

-include_lib("eunit/include/eunit.hrl").

token_type(Char) when Char >= $0 andalso Char =< $9 ->
    number;

token_type(Char) when Char == $  orelse Char == $\n ->
    whitespace;

token_type(${) ->
    open_brace;

token_type($}) ->
    close_brace;

token_type($[) ->
    open_square_bracket;

token_type($]) ->
    close_square_bracket;

token_type($,) ->
    comma;

token_type($") ->
    qoute;

token_type($:) ->
    colon;

token_type(_) ->
    other.

tokenize(<<>>) ->
    {none, <<>>};

tokenize(<<Char>>) ->
    tokenize(Char, <<>>);

tokenize(<<First, Rest/binary>>) ->
    tokenize(First, Rest).

consume(number, [First], Rest) ->
    Input = <<First, Rest/binary>>,
    {FloatValue, FloatRemainder} = string:to_float(binary_to_list(Input)),
    case FloatValue of
        error ->
            {IntValue, IntRemainder} = string:to_integer(binary_to_list(Input)),
            case IntValue of
                error ->
                    {{error, none}, Input};
                _ ->
                    {{number, IntValue}, list_to_binary(IntRemainder)}
            end;
        _ ->
            {{number, FloatValue}, list_to_binary(FloatRemainder)}
    end;

consume(whitespace, [First], Rest) ->
    T = token_type(First),
    consume_whitespace(T, First, Rest);

consume(qoute, [_Qoute], <<First, Rest/binary>>) ->
    T = token_type(First),
    consume_until_qoute(T, <<>>, First, Rest);

consume(TokenType, [_First], Rest) ->
    {TokenType, Rest}.

consume_whitespace(whitespace, _Prev, <<>>) ->
    {whitespace, <<>>};

consume_whitespace(whitespace, _Prev, <<Last>>) ->
    T = token_type(Last),
    consume_whitespace(T, Last, <<>>);

consume_whitespace(whitespace, _Prev, <<First, Rest/binary>>) ->
    T = token_type(First),
    consume_whitespace(T, First, Rest);

consume_whitespace(Tokentype, Prev, Rest) ->
    consume(Tokentype, [Prev], Rest).

consume_until_qoute(qoute, Acc, _Qoute, Rest) ->
    {{qouted_string, Acc}, Rest};

consume_until_qoute(_TokenType, _Acc, _Prev, <<>>) ->
    {error};

consume_until_qoute(_TokenType, Acc, Prev, <<Last>>) ->
    T = token_type(Last),
    consume_until_qoute(T, <<Acc/binary, Prev>>, Last, <<>>);

consume_until_qoute(_TokenType, Acc, Prev, <<First, Rest/binary>>) ->
    T = token_type(First),
    consume_until_qoute(T, <<Acc/binary, Prev>>, First, Rest).

tokenize(First, Rest) ->
    T = token_type(First),
    consume(T, [First], Rest).
