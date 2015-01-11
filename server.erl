-module(server).
-export([start/2,init/2,call/2,cast/2]).

start(Mod,Args) ->
    erlang:spawn(?MODULE,init,[Mod,Args]).

init(Mod,Args)->
    erlang:register(Mod,self()),
    State = Mod:init(Args),
    loop(Mod,State).

loop(Mod,State) ->
    receive
        {call,From,Req} ->
            {Res,NewState} = Mod:handle_call(Req,State),
            From ! {Mod,Res},
            loop(Mod,NewState);
        {cast,Req} ->
            NewState = Mod:handle_cast(Req,State),
            loop(Mod,NewState)
    end.

call(Name,Req) ->
    Name ! {call,self(),Req},
    receive
        {Name,Res} ->
            Res
    end.

cast(Name,Req) ->
    Name ! {cast,Req},
    ok.

