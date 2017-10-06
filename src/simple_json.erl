-module(simple_json).
-author("snorri.sturluson@gmail.com").

-include_lib("eunit/include/eunit.hrl").

%% API exports
-export([parse_simple_json/1, fields_match/2]).

%%====================================================================
%% API functions
%%====================================================================

parse_simple_json(Input) ->
    Stripped = string:strip(string:strip(Input, right, $}), left, ${),
    Fields = string:tokens(Stripped, ","),
    list_of_fields_to_dict(dict:new(), Fields).


fields_match(Desired, Actual) ->
    Keys = dict:fetch_keys(Desired),
    fields_match(Keys, Desired, Actual).

%%====================================================================
%% Internal functions
%%====================================================================

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


split_field(Field) ->
    SplitAt = string:str(Field, ":"),
    FieldName = string:strip(string:substr(Field, 1, SplitAt - 1), both, $"),
    Value = string:strip(string:substr(Field, SplitAt + 1), both, $"),
    {FieldName, Value}.


list_of_fields_to_dict(D, []) ->
    D;

list_of_fields_to_dict(D, [Field]) ->
    StrippedField = string:strip(Field, both, $ ),
    {Key, Value} = split_field(StrippedField),
    dict:store(Key, Value, D);

list_of_fields_to_dict(D, [Field|Tail]) ->
    D2 = list_of_fields_to_dict(D, [Field]),
    list_of_fields_to_dict(D2, Tail).

%%====================================================================
%% Internal functions test
%%====================================================================

split_field_simple_test() ->
    {"key", "value"} = split_field("key:value").

split_field_quoted_test() ->
    {"key", "value"} = split_field("\"key\":\"value\"").
