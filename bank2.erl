-module(bank2).

-export([start/1,init/1,handle_call/2,handle_cast/2]).
-export([withdraw/1,deposit/1,print/0]).

start(Money) ->
    server:start(?MODULE,Money).

init(Money) ->
    Money.

handle_call({withdraw,Num},Money) ->
   NewMoney =  Money - Num,
   NewState = NewMoney,
   {NewMoney,NewState};
handle_call({deposit,Num},Money) ->
    NewMoney = Money + Num,
    NewState = NewMoney,
    {NewMoney,NewState}.

handle_cast(print,Money) ->
    io:format("account money:~w~n",[Money]),
    Money.


withdraw(Money) ->
    server:call(?MODULE,{withdraw,Money}).

deposit(Money) ->
    server:call(?MODULE,{deposit,Money}).

print() ->
    server:cast(?MODULE,print).
