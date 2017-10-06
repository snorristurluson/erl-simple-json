-module(tokens_test).
-compile(export_all).

-include_lib("eunit/include/eunit.hrl").

tokenize_empty_test() ->
    {none, <<>>} = tokens:tokenize(<<>>).

tokenize_single_digit_test() ->
    {{number, 4}, <<>>} = tokens:tokenize(list_to_binary("4")).

tokenize_int_test() ->
    {{number, 423}, <<>>} = tokens:tokenize(list_to_binary("423")).

tokenize_float_test() ->
    {{number, 42.3}, <<>>} = tokens:tokenize(list_to_binary("42.3")).

tokenize_two_floats_test() ->
    {{number, 42.3}, <<", 12.3">>} = tokens:tokenize(list_to_binary("42.3, 12.3")).

tokenize_whitespace_test() ->
    {whitespace, <<>>} = tokens:tokenize(list_to_binary("   ")).

tokenize_whitespace_2_test() ->
    {{number, 4}, <<>>} = tokens:tokenize(list_to_binary("   4")).

tokenize_start_object_test() ->
    {open_brace, <<"bla">>} = tokens:tokenize(list_to_binary("{bla")).

tokenize_string_test() ->
    {{qouted_string, <<"test">>}, <<>>} = tokens:tokenize(list_to_binary("\"test\"")).

tokenize_string_2_test() ->
    {{qouted_string, <<"test">>}, <<", 123">>} = tokens:tokenize(list_to_binary("\"test\", 123")).

tokenize_string_no_end_qoute_test() ->
    {error} = tokens:tokenize(list_to_binary("\"test")).
