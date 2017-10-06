-module(parse_json_test).
-include_lib("eunit/include/eunit.hrl").

empty_string_returns_error_test() ->
    error = parse_json:parse_json(<<"">>).

empty_object_returns_empty_dict_test() ->
    D = parse_json:parse_json(<<"{}">>),
    true = dict:is_empty(D).

single_field_test() ->
    D = parse_json:parse_json(<<"{\"bingo\": \"bongo\"}">>),
    <<"bongo">> = dict:fetch(<<"bingo">>, D).

two_field_test() ->
    D = parse_json:parse_json(<<"{\"bingo\": \"bongo\", \"foo\": \"bar\"}">>),
    <<"bongo">> = dict:fetch(<<"bingo">>, D),
    <<"bar">> = dict:fetch(<<"foo">>, D).

numeric_field_test() ->
    D = parse_json:parse_json(<<"{\"bingo\": 42, \"foo\": 3.14}">>),
    42 = dict:fetch(<<"bingo">>, D),
    3.14 = dict:fetch(<<"foo">>, D).

