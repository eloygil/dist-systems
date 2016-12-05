-module(acceptor).
-export([start/2]).

-define(delay, 1500).
-define(drop, 2).

start(Name, PanelId) ->
    spawn(fun() -> init(Name, PanelId) end).
        
init(Name, PanelId) ->
    Promised = order:null(), 
    Voted = order:null(),
    Value = na,
    acceptor(Name, Promised, Voted, Value, PanelId).

acceptor(Name, Promised, Voted, Value, PanelId) ->
  receive
    {prepare, Proposer, Round} ->
        %R = rand:uniform(?delay),
        %timer:sleep(R),
        case order:gr(Round, Promised) of
            true ->

            case rand:uniform(?drop) of
                ?drop ->
                    io:format("message dropped~n");
                _ -> Proposer ! {promise, Round, Voted, Value}   
            end,
                            
                % Update gui
                if
                    Value == na ->
                        Colour = {0,0,0};
                    true ->
                        Colour = Value
                end,                
    io:format("[Acceptor ~w] Phase 1: promised ~w voted ~w colour ~w~n",
                [Name, Round, Voted, Value]),
                PanelId ! {updateAcc, "Voted: " 
                        ++ io_lib:format("~p", [Voted]), "Promised: " 
                        ++ io_lib:format("~p", [Round]), Colour},
                acceptor(Name, Round, Voted, Value, PanelId);
            false ->
                %Proposer ! {sorry, {prepare, Round}},
                acceptor(Name, Promised, Voted, Value, PanelId)
        end;
    {accept, Proposer, Round, Proposal} ->
        %R = rand:uniform(?delay),
        %timer:sleep(R),
        case order:goe(Round, Promised) of
            true ->
                case rand:uniform(?drop) of
                    ?drop ->
                        io:format("message dropped~n");
                    _ -> Proposer ! {vote, Round}
                end,
                case order:goe(Voted, Round) of
                    true ->
                        % Update gui
    io:format("[Acceptor ~w] Phase 2: promised ~w voted ~w colour ~w~n",
                        [Name, Promised, Voted, Value]),
                        PanelId ! {updateAcc, "Voted: " 
                                ++ io_lib:format("~p", [Voted]), "Promised: " 
                                ++ io_lib:format("~p", [Promised]), Value},
                        acceptor(Name, Promised, Voted, Value, PanelId);
                    false ->
                        % Update gui
    io:format("[Acceptor ~w] Phase 2: promised ~w voted ~w colour ~w~n",
                        [Name, Promised, Round, Proposal]),
                        PanelId ! {updateAcc, "Voted: " 
                                ++ io_lib:format("~p", [Round]), "Promised: " 
                                ++ io_lib:format("~p", [Promised]), Proposal},
                        acceptor(Name, Promised, Round, Proposal, PanelId)
                end;                            
            false ->
                %Proposer ! {sorry, {accept, Round}},
                acceptor(Name, Promised, Voted, Value, PanelId)
        end;
    stop ->
        PanelId ! stop,
        ok
  end.
