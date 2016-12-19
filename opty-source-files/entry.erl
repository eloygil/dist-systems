-module(entry).
-export([new/1]).

new(Value) ->
    spawn_link(fun() -> init(Value) end).

init(Value) ->
    entry(Value, make_ref()).

entry(Value, Time) ->
    receive
        {read, Ref, From} ->
            %% TODO: ADD SOME CODE
            From ! {Ref, self(), Value, Time},
            entry(Value, Time);
        {write, New} ->
            entry(New, make_ref());  %% TODO: COMPLETE
        {check, Ref, Readtime, From} ->
            if 
                Readtime == Time ->   %% TODO: COMPLETE
                %% TODO: ADD SOME CODE
                    From ! {Ref, ok};
                true ->
                    From ! {Ref, abort}
            end,
            entry(Value, Time);
        stop ->
            ok
    end.
