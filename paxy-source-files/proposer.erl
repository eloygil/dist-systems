-module(proposer).
-export([start/5, startdist/5]).

-define(timeout, 2000).
-define(backoff, 10).

start(Name, Proposal, Acceptors, Sleep, PanelId) ->
    spawn(fun() -> init(Name, Proposal, Acceptors, Sleep, PanelId) end).

startdist(Name, Proposal, Acceptors, Sleep, PanelId) ->
    init(Name, Proposal, Acceptors, Sleep, PanelId).

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
    case ballot(Name, Round, Proposal, Acceptors, PanelId) of
        {ok, Decision} ->
            io:format("[Proposer ~w] ~w DECIDED ~w in round ~w~n", 
            [Name, Acceptors, Decision, Round]),
            PanelId ! stop,
            {ok, Decision};
        abort ->
            timer:sleep(rand:uniform(Backoff)),
            Next = order:inc(Round),
            round(Name, (2*Backoff), Next, Proposal, Acceptors, PanelId)
    end.

ballot(Name, Round, Proposal, Acceptors, PanelId) ->
    prepare(Round, Acceptors),
    Quorum = (length(Acceptors) div 2) + 1,
    MaxVoted = order:null(),
    %case collect(Quorum, Quorum, Round, MaxVoted, Proposal) of
    case collect(Quorum, Round, MaxVoted, Proposal) of
        {accepted, Value} ->
            % update gui
            io:format("[Proposer ~w] Phase 2: round ~w proposal ~w (was ~w)~n", 
            [Name, Round, Value, Proposal]),
            PanelId ! {updateProp, "Round: " 
                    ++ io_lib:format("~p", [Round]), "Proposal: "
                    ++ io_lib:format("~p", [Value]), Value},
            accept(Round, Value, Acceptors),
            %case vote(Quorum, Quorum, Round) of
            case vote(Quorum, Round) of
                ok ->
                    {ok, Value};
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
            collect(N-1, Round, MaxVoted, Proposal);
        {promise, Round, Voted, Value} ->
            case order:gr(Voted, MaxVoted) of
                true ->
                    collect(N-1, Round, Voted, Value);
                false ->
                    collect(N-1, Round, MaxVoted, Proposal)
            end;
        {promise, _, _, _} ->
            collect(N, Round, MaxVoted, Proposal);
        {sorry, {prepare, Round}} ->
            collect(N, Round, MaxVoted, Proposal);
        {sorry, _} ->
            collect(N, Round, MaxVoted, Proposal)
    after ?timeout ->
            abort
    end.

collect(0, _, _, _, Proposal) ->
    {accepted, Proposal};
collect(_, 0, _, _, _) ->
    abort;
collect(N, M, Round, MaxVoted, Proposal) ->
    receive 
        {promise, Round, _, na} ->
            collect(N-1, M,Round, MaxVoted, Proposal);
        {promise, Round, Voted, Value} ->
            case order:gr(Voted, MaxVoted) of
                true ->
                    collect(N-1, M, Round, Voted, Value);
                false ->
                    collect(N-1, M, Round, MaxVoted, Proposal)
            end;
        {promise, _, _, _} ->
            collect(N, M, Round, MaxVoted, Proposal);
        {sorry, {prepare, Round}} ->
            collect(N, M-1, Round, MaxVoted, Proposal);
        {sorry, _} ->
            collect(N, M, Round, MaxVoted, Proposal)
    after ?timeout ->
            abort
    end.

vote(0, _) ->
    ok;
vote(N, Round) ->
    receive
        {vote, Round} ->
            vote(N-1, Round);
        {vote, _} ->
            vote(N, Round);
        {sorry, {accept, Round}} ->
            vote(N, Round);
        {sorry, _} ->
            vote(N, Round)
    after ?timeout ->
            abort
    end.

vote(0, _, _) ->
    ok;
vote(_, 0, _) ->
    abort;
vote(N, M, Round) ->
    receive
        {vote, Round} ->
            vote(N-1, M, Round);
        {vote, _} ->
            vote(N, M, Round);
        {sorry, {accept, Round}} ->
            vote(N, M-1, Round);
        {sorry, _} ->
            vote(N, M, Round)
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

%send(Name, Message) ->
%    Name ! Message.

send(Name, Message) ->
    if is_tuple(Name) -> %remote
        Name ! Message;
    true -> %local
        case whereis(Name) of
            undefined ->
                down;
            Pid ->
                Pid ! Message
        end
end.

%send(Name, Message) ->
%    global:send(Name, Message).

