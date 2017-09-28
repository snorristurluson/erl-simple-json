# erl-simple-json
A simple JSON parser in Erlang.

This Erlang module provides two functions, **parse_simple_json/1** and **fields_match/2**.

## parse_simple_json(Json) -> Dict
Takes in a simple JSON string - i.e. a JSON representation of a single object
with only simple attributes (no lists or objects).

It returns a dict object.

## fields_match(Desired, Actual) -> bool
Returns true if all keys from Desired exist in Actual and have equivalent values.
