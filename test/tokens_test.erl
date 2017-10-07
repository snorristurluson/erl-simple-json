-module(tokens_test).

-include_lib("eunit/include/eunit.hrl").

tokenize_empty_test() ->
    {none, <<>>} = tokens:tokenize(<<>>).

tokenize_single_digit_test() ->
    {{number, 4}, <<>>} = tokens:tokenize(<<"4">>).

tokenize_int_test() ->
    {{number, 423}, <<>>} = tokens:tokenize(<<"423">>).

tokenize_float_test() ->
    {{number, 42.3}, <<>>} = tokens:tokenize(<<"42.3">>).

tokenize_two_floats_test() ->
    {{number, 42.3}, <<", 12.3">>} = tokens:tokenize(<<"42.3, 12.3">>).

tokenize_whitespace_test() ->
    {whitespace, <<>>} = tokens:tokenize(<<"   ">>).

tokenize_whitespace_2_test() ->
    {{number, 4}, <<>>} = tokens:tokenize(<<"   4">>).

tokenize_start_object_test() ->
    {open_brace, <<"bla">>} = tokens:tokenize(<<"{bla">>).

tokenize_start_list_test() ->
    {open_square_bracket, <<"bla">>} = tokens:tokenize(<<"[bla">>).

tokenize_string_test() ->
    {{qouted_string, <<"test">>}, <<>>} = tokens:tokenize(<<"\"test\"">>).

tokenize_string_2_test() ->
    {{qouted_string, <<"test">>}, <<", 123">>} = tokens:tokenize(<<"\"test\", 123">>).

tokenize_string_no_end_qoute_test() ->
    {error} = tokens:tokenize(<<"\"test">>).

tokenize_true_test() ->
    {true, <<>>} = tokens:tokenize(<<"true">>).

tokenize_true_2_test() ->
    {error, <<>>} = tokens:tokenize(<<"truetrail">>).

tokenize_true_3_test() ->
    {true, <<"}">>} = tokens:tokenize(<<"true}">>).

tokenize_false_test() ->
    {false, <<>>} = tokens:tokenize(<<"false">>).

tokenize_null_test() ->
    {null, <<>>} = tokens:tokenize(<<"null">>).
