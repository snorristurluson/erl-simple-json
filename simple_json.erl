%%%-------------------------------------------------------------------
%%% @author Snorri Sturluson
%%% @doc
%%%
%%% @end
%%% Created : 27. Sep 2017 16:11
%%%-------------------------------------------------------------------
-module(simple_json).
-author("snorri.sturluson@gmail.com").

-include_lib("eunit/include/eunit.hrl").

-export([parse_simple_json/1, fields_match/2]).

split_field(Field) ->
    SplitAt = string:str(Field, ":"),
    FieldName = string:strip(string:substr(Field, 1, SplitAt - 1), both, $"),
    Value = string:strip(string:substr(Field, SplitAt + 1), both, $"),
    {FieldName, Value}.


list_of_fields_to_dict(D, []) ->
    D;

list_of_fields_to_dict(D, [Field]) ->
    {Key, Value} = split_field(Field),
    dict:store(Key, Value, D);

list_of_fields_to_dict(D, [Field|Tail]) ->
    D2 = list_of_fields_to_dict(D, [Field]),
    list_of_fields_to_dict(D2, Tail).


parse_simple_json(Input) ->
    Stripped = string:strip(string:strip(Input, right, $}), left, ${),
    Fields = string:tokens(Stripped, ","),
    list_of_fields_to_dict(dict:new(), Fields).


fields_match([], _Desired, _Actual) ->
    true;

fields_match([Key], Desired, Actual) ->
    DesiredValue = dict:fetch(Key, Desired),
    ActualValue = dict:fetch(Key, Actual),
    DesiredValue == ActualValue;

fields_match([Key|Tail], Desired, Actual) ->
    case fields_match([Key], Desired, Actual) of
        true -> fields_match(Tail, Desired, Actual);
        false -> false
    end.


fields_match(Desired, Actual) ->
    Keys = dict:fetch_keys(Desired),
    fields_match(Keys, Desired, Actual).


split_field_simple_test() ->
    {"key", "value"} = split_field("key:value").

split_field_quoted_test() ->
    {"key", "value"} = split_field("\"key\":\"value\"").

empty_string_test() ->
    Result = parse_simple_json(""),
    ?assert(dict:is_empty(Result)).

empty_object_test() ->
    Result = parse_simple_json("{}"),
    ?assert(dict:is_empty(Result)).

single_field_test() ->
    Result = parse_simple_json("{\"key\":\"value\"}"),
    1 = dict:size(Result),
    "value" = dict:fetch("key", Result).

number_field_test() ->
    Result = parse_simple_json("{\"key\":46}"),
    1 = dict:size(Result),
    "46" = dict:fetch("key", Result).

multiple_fields_test() ->
    Result = parse_simple_json("{\"ExpiresOn\":\"2017-09-28T15:19:13\",\"Scopes\":\"myScope\",\"TokenType\":\"Service\",\"ApplicationID\":42,\"ClientIdentifier\":\"myApp\",\"IntellectualProperty\":\"myIP\"}"),
    6 = dict:size(Result),
    "Service" = dict:fetch("TokenType", Result).

fields_match_test() ->
    Result = parse_simple_json("{\"ExpiresOn\":\"2017-09-28T15:19:13\",\"Scopes\":\"myScope\",\"TokenType\":\"Service\",\"ApplicationID\":42,\"ClientIdentifier\":\"myApp\",\"IntellectualProperty\":\"myIP\"}"),
    Desired = dict:from_list([
        {"Scopes", "myScope"},
        {"TokenType", "Service"},
        {"ClientIdentifier", "myApp"}
    ]),
    true = fields_match(Desired, Result).
