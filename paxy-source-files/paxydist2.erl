-module(paxydist2).
-export([start/1, stop/0, stop/1, crash/1]).

-define(RED, {255,0,0}).
-define(BLUE, {0,0,255}).
-define(GREEN, {0,255,0}).
-define(YELLOW, {255,255,0}).
-define(PINK, {255,0,255}).
-define(CYAN, {0,255,255}).

%Having local registers

% Sleep is a list with the initial sleep time for each proposer
start(Sleep) ->
    AcceptorNames = ["Acceptor a", "Acceptor b", "Acceptor c", 
    "Acceptor d", "Acceptor e"],
    % NewAcceptorNames = ["Acceptor a", "Acceptor b", "Acceptor c", 
    % "Acceptor d", "Acceptor e", "Acceptor f", "Acceptor g", "Acceptor h", "Acceptor i" ],
    AccRegister = [a, b, c, d, e],
    TupleRegister = [{a, 'acceptors@127.0.0.1'}, {b, 'acceptors@127.0.0.1'},
    {c, 'acceptors@127.0.0.1'}, {d, 'acceptors@127.0.0.1'}, {e, 'acceptors@127.0.0.1'}],
    %NewAccRegister = [a, b, c, d, e, f, g, h, i],
    ProposerNames = [{"Proposer kurtz", ?RED}, {"Proposer kilgore", ?GREEN}, 
                     {"Proposer willard", ?BLUE}],
    % NewProposerNames = [{"Proposer kurtz", ?RED}, {"Proposer kilgore", ?GREEN}, 
    %                 {"Proposer willard", ?BLUE}, {"Proposer albert", ?YELLOW}, {"Proposer eloy", ?PINK}, {"Proposer luis", ?CYAN}],
    PropInfo = [{kurtz, ?RED}, {kilgore, ?GREEN}, {willard, ?BLUE}],
    % NewPropInfo = [{kurtz, ?RED}, {kilgore, ?GREEN}, {willard, ?BLUE}, {albert, ?YELLOW}, {eloy, ?PINK}, {luis, ?CYAN}],
    register(gui, spawn(fun() -> gui:start(AcceptorNames, ProposerNames) end)),
    gui ! {reqState, self()},
    receive
        {reqState, State} ->
            {AccIds, PropIds} = State,
            start_acceptors(AccIds, AccRegister),
            start_proposers(PropIds, PropInfo, TupleRegister, Sleep)
    end,
    true.
    
start_acceptors(AccIds, AccReg) ->
    case AccIds of
        [] ->
            ok;
        [AccId|Rest] ->
            [RegName|RegNameRest] = AccReg,
            spawn('acceptors@127.0.0.1', acceptor, startdist2, [RegName, AccId]),
            start_acceptors(Rest, RegNameRest)
    end.


start_proposers(PropIds, PropInfo, Acceptors, Sleep) ->
    case PropIds of
        [] ->
            ok;
        [PropId|Rest] ->
            [{RegName, Colour}|RestInfo] = PropInfo,
            [FirstSleep|RestSleep] = Sleep,
            spawn('proposers@127.0.0.1', proposer, startdist, [RegName, Colour, Acceptors, FirstSleep, PropId]),
            start_proposers(Rest, RestInfo, Acceptors, RestSleep)
        end.

stop() ->
    stop(gui).

stop(Name) ->
    case whereis(Name) of
        undefined ->
            ok;
        Pid ->
            Pid ! stop
    end.

crash(Name) ->
    case whereis(Name) of
    undefined ->
        ok;
    Pid ->
        pers:open(Name),
        {_, _, _, Pn} = pers:read(Name),
        Pn ! {updateAcc, "Voted: CRASHED", "Promised: CRASHED", {0,0,0}},
        dets:close(Name),
        unregister(Name),
        exit(Pid, "crash"),
        timer:sleep(2000),
        register(Name, acceptor:start(Name, na))
end.
 
