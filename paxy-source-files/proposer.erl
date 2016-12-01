-module(proposer).
-export([start/5]).

-define(timeout, 2000).
-define(backoff, 10).

start(Name, Proposal, Acceptors, Sleep, PanelId) ->
    spawn(fun() -> init(Name, Proposal, Acceptors, Sleep, PanelId) end).

init(Name, Proposal, Acceptors, Sleep, PanelId) ->
    timer:sleep(Sleep),
    Round = order:first(Name),
    round(Name, ?backoff, Round, Proposal, Acceptors, PanelId).

round(Name, Backoff, Round, Proposal, Acceptors, PanelId) ->
    % Update gui
    io:format("[Proposer ~w] Phase 1: round ~w proposal ~w~n",
    [Name, Round, Proposal]),
    PanelId ! {updateProp, "Round: " 
            ++ io_lib:format("~p", [Round]), "Proposal: "
            ++ io_lib:format("~p", [Proposal]), Proposal},
    case ballot(Name, ..., ..., ..., PanelId) of
        {ok, Decision} ->
            io:format("[Proposer ~w] ~w DECIDED ~w in round ~w~n", 
            [Name, Acceptors, Decision, Round]),
            PanelId ! stop,
            {ok, Decision};
        abort ->
            timer:sleep(rand:uniform(Backoff)),
            Next = order:inc(...),
            round(Name, (2*Backoff), ..., Proposal, Acceptors, PanelId)
    end.

ballot(Name, Round, Proposal, Acceptors, PanelId) ->
    prepare(..., ...),
    Quorum = (length(...) div 2) + 1,
    MaxVoted = order:null(),
    case collect(..., ..., ..., ...) of
        {accepted, Value} ->
            % update gui
            io:format("[Proposer ~w] Phase 2: round ~w proposal ~w (was ~w)~n", 
            [Name, Round, Value, Proposal]),
            PanelId ! {updateProp, "Round: " 
                    ++ io_lib:format("~p", [Round]), "Proposal: "
                    ++ io_lib:format("~p", [Value]), Value},
            accept(..., ..., ...),
            case vote(..., ...) of
                ok ->
                    {ok, ...};
                abort ->
                    abort
            end;
        abort ->
            abort
    end.

collect(0, _, _, Proposal) ->
    {accepted, Proposal};
collect(N, Round, MaxVoted, Proposal) ->
    receive 
        {promise, Round, _, na} ->
            collect(..., ..., ..., ...);
        {promise, Round, Voted, Value} ->
            case order:gr(..., ...) of
                true ->
                    collect(..., ..., ..., ...);
                false ->
                    collect(..., ..., ..., ...)
            end;
        {promise, _, _,  _} ->
            collect(N, Round, MaxVoted, Proposal);
        {sorry, {prepare, Round}} ->
            collect(..., ..., ..., ...);
        {sorry, _} ->
            collect(N, Round, MaxVoted, Proposal)
    after ?timeout ->
            abort
    end.

vote(0, _) ->
    ok;
vote(N, Round) ->
    receive
        {vote, Round} ->
            vote(..., ...);
        {vote, _} ->
            vote(N, Round);
        {sorry, {accept, Round}} ->
            vote(..., ...);
        {sorry, _} ->
            vote(N, Round)
    after ?timeout ->
            abort
    end.

prepare(Round, Acceptors) ->
    Fun = fun(Acceptor) -> 
        send(Acceptor, {prepare, self(), Round}) 
    end,
    lists:foreach(Fun, Acceptors).

accept(Round, Proposal, Acceptors) ->
    Fun = fun(Acceptor) -> 
        send(Acceptor, {accept, self(), Round, Proposal}) 
    end,
    lists:foreach(Fun, Acceptors).

send(Name, Message) ->
    Name ! Message.
