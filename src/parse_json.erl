-module(parse_json).

%% API
-export([parse_json/1]).

parse_json(<<>>) ->
    error;

parse_json(Input) ->
    {Value, <<>>} = parse_json_value(Input),
    Value.

parse_json_value(Input) ->
    {Token, Rest} = tokens:tokenize(Input),
    case Token of
        open_brace ->
            populate_object(dict:new(), Rest);
        open_square_bracket ->
            populate_list([], Rest);
        {_, Value} ->
            {Value, Rest};
        Other ->
            {Other, Rest}
    end.

populate_object(Obj, Input) ->
    {Token, Rest} = tokens:tokenize(Input),
    case Token of
        close_brace ->
            {Obj, Rest};
        {qouted_string, Field} ->
            {colon, Rest2} = tokens:tokenize(Rest),
            {Value, Rest3} = parse_json_value(Rest2),
            Obj2 = dict:store(Field, Value, Obj),
            populate_object(Obj2, Rest3);
        comma ->
            populate_object(Obj, Rest)
    end.

populate_list(List, Input) ->
    {Token, Rest} = tokens:tokenize(Input),
    case Token of
        close_square_bracket ->
            {List, Rest};
        comma ->
            populate_list(List, Rest);
        open_brace ->
            {Obj, Rest2} = populate_object(dict:new(), Rest),
            populate_list(lists:append(List, [Obj]), Rest2);
        {_, Value} ->
            populate_list(lists:append(List, [Value]), Rest);
        Other ->
            populate_list(lists:append(List, [Other]), Rest)

    end.