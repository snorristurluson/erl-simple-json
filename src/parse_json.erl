-module(parse_json).

%% API
-export([parse_json/1]).

parse_json(<<>>) ->
    error;

parse_json(Input) ->
    {Token, Rest} = tokens:tokenize(Input),
    case Token of
        open_brace ->
            {D, Rest2} = populate_object(dict:new(), Rest),
            D
    end.

populate_object(Obj, Input) ->
    {Token, Rest} = tokens:tokenize(Input),
    case Token of
        close_brace ->
            {Obj, Rest};
        {qouted_string, Field} ->
            {colon, Rest2} = tokens:tokenize(Rest),
            {Value, Rest3} = parse_value(Rest2),
            Obj2 = dict:store(Field, Value, Obj),
            populate_object(Obj2, Rest3);
        comma ->
            populate_object(Obj, Rest)
    end.

parse_value(Input) ->
    {Token, Rest} = tokens:tokenize(Input),
    case Token of
        {qouted_string, StringValue} ->
            {StringValue, Rest};
        {number, NumericValue} ->
            {NumericValue, Rest}
    end.