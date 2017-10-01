-module(simple_json_test).
-compile(export_all).

-include_lib("eunit/include/eunit.hrl").

empty_string_test() ->
    Result = simple_json:parse_simple_json(""),
    ?assert(dict:is_empty(Result)).

empty_object_test() ->
    Result = simple_json:parse_simple_json("{}"),
    ?assert(dict:is_empty(Result)).

single_field_test() ->
    Result = simple_json:parse_simple_json("{\"key\":\"value\"}"),
    1 = dict:size(Result),
    "value" = dict:fetch("key", Result).

number_field_test() ->
    Result = simple_json:parse_simple_json("{\"key\":46}"),
    1 = dict:size(Result),
    "46" = dict:fetch("key", Result).

multiple_fields_test() ->
    Result = simple_json:parse_simple_json("{\"ExpiresOn\":\"2017-09-28T15:19:13\",\"Scopes\":\"myScope\",\"TokenType\":\"Service\",\"ApplicationID\":42,\"ClientIdentifier\":\"myApp\",\"IntellectualProperty\":\"myIP\"}"),
    6 = dict:size(Result),
    "Service" = dict:fetch("TokenType", Result).

multiple_fields_with_spaces_test() ->
    Result = simple_json:parse_simple_json("{\"ExpiresOn\":\"2017-09-28T15:19:13\",     \"Scopes\":\"myScope\", \"TokenType\":\"Service\", \"ApplicationID\":42, \"ClientIdentifier\":\"myApp\", \"IntellectualProperty\":\"myIP\"}"),
    6 = dict:size(Result),
    "Service" = dict:fetch("TokenType", Result).

fields_match_test() ->
    Result = simple_json:parse_simple_json("{\"ExpiresOn\":\"2017-09-28T15:19:13\",\"Scopes\":\"myScope\",\"TokenType\":\"Service\",\"ApplicationID\":42,\"ClientIdentifier\":\"myApp\",\"IntellectualProperty\":\"myIP\"}"),
    Desired = dict:from_list([
        {"Scopes", "myScope"},
        {"TokenType", "Service"},
        {"ClientIdentifier", "myApp"}
    ]),
    true = simple_json:fields_match(Desired, Result).
