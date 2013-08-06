-module(oc_time).

-export([
         convert_units/2
         ]).

convert_units({Val, SourceUnit}, TargetUnit) ->
    Val * to_ms(SourceUnit, TargetUnit).

to_ms(_Unit, _Unit) ->
    1;
to_ms(min, ms) ->
    60000;
to_ms(sec, ms) ->
    1000.
