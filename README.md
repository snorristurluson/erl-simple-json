# erl-simple-json
[![Build status](https://travis-ci.org/snorristurluson/erl-simple-json.svg?branch=master)](https://travis-ci.org/snorristurluson/erl-simple-json#)

A simple JSON parser in Erlang.

## parse_json
The **parse_json** module provides one function, **parse_json/1**.

### parse_json(Json) -> Value
Takes in a JSON string, returns a value. The value can be dict (for
JSON objects), a list of values, a string or a number.

## simple_json
The **simple_json** module provides two functions,
**parse_simple_json/1** and **fields_match/2**.

This module only handles a limited (but useful) subset of JSON.
This was my first pass - I'm leaving it in here for now as a
simpler alternative. 

### parse_simple_json(Json) -> Dict
Takes in a simple JSON string - i.e. a JSON representation of a single object
with only simple attributes (no lists or objects).

It returns a dict object.

### fields_match(Desired, Actual) -> bool
Returns true if all keys from Desired exist in Actual and have equivalent values.
