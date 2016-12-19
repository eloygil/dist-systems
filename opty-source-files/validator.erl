-module(validator).
-export([start/0]).

start() ->
    spawn_link(fun() -> init() end).

init()->
    validator().

validator() ->
    receive
        {validate, Ref, Reads, Writes, Client} ->
            Tag = make_ref(),
            send_read_checks(Reads, Tag),  %% TODO: COMPLETE
            case check_reads(length(Reads), Tag) of  %% TODO: COMPLETE
                ok ->
                    update(Writes),  %% TODO: COMPLETE
                    Client ! {Ref, ok};
                abort ->
                    %% TODO: ADD SOME CODE
                    Client ! {Ref, abort}
            end,
            validator();
        stop ->
            ok;
        _Old ->
            validator()
    end.
    
update(Writes) ->
    lists:foreach(fun({_, Entry, Value}) -> 
                  %% TODO: ADD SOME CODE
                  Entry ! {write, Value}
                  end, 
                  Writes).

send_read_checks(Reads, Tag) ->
    Self = self(),
    lists:foreach(fun({Entry, Time}) -> 
                  %% TODO: ADD SOME CODE
                  Entry ! {check, Tag, Time, Self}
                  end, 
                  Reads).

check_reads(0, _) ->
    ok;
check_reads(N, Tag) ->
    receive
        {Tag, ok} ->
            check_reads(N-1, Tag);
        {Tag, abort} ->
            abort
    end.
