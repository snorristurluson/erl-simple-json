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

empty_list_field_test() ->
    D = parse_json:parse_json(<<"{\"bingo\": []}">>),
    [] = dict:fetch(<<"bingo">>, D).

list_field_test() ->
    D = parse_json:parse_json(<<"{\"bingo\": [42, 3.14, 123]}">>),
    [42, 3.14, 123] = dict:fetch(<<"bingo">>, D).

true_test() ->
    true = parse_json:parse_json(<<"true">>).

true_field_test() ->
    D = parse_json:parse_json(<<"{\"bingo\": true}">>),
    true = dict:fetch(<<"bingo">>, D).

false_field_test() ->
    D = parse_json:parse_json(<<"{\"bingo\": false}">>),
    false = dict:fetch(<<"bingo">>, D).

null_field_test() ->
    D = parse_json:parse_json(<<"{\"bingo\": null}">>),
    null = dict:fetch(<<"bingo">>, D).

list_of_bool_and_null_test() ->
    [true, false, null] = parse_json:parse_json(<<"[true, false, null]">>).

list_of_empty_objects_test() ->
    [D1, D2, D3] = parse_json:parse_json(<<"[{}, {}, {}]">>),
    true = dict:is_empty(D1),
    true = dict:is_empty(D2),
    true = dict:is_empty(D3).

list_with_newlines_test() ->
    [1, 2, 3] = parse_json:parse_json(<<"[1,  \n2, 3\n]">>).
