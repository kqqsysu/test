-module(bank1).
-export([start/1,
         init/1,
         withdraw/1, %% 取款
         deposit/1,  %% 存款
         print/0     %% 打印帐号信息
     ]).

start(Money) ->
    %% spawn,开启进程，并注册进程名为当前模块名
    erlang:spawn(?MODULE,init,[Money]).

%% 取钱，向?MODULE发关消息，并等待返回结果 
withdraw(Money) ->
    ?MODULE ! {self(),withdraw,Money},
    receive
        {?MODULE,NewMoney} ->
            NewMoney
    end.

%% 存钱，向?MODULE发关消息，并等待返回结果 
deposit(Money) ->
    ?MODULE ! {self(),deposit,Money},
    receive
        {?MODULE,NewMoney} ->
            NewMoney
    end.

%% 打印帐号信息，发送消息后直接返回
print()->
    ?MODULE ! print,
    ok.

init(Money)->
    erlang:register(?MODULE,self()),
    loop(Money).

%% loop 循环，接收进程消息
loop(Money) ->
    receive
        {From,withdraw,Num} ->
            NewMoney = Money - Num,
            From ! {?MODULE,NewMoney},
            loop(NewMoney);
        {From,deposit,Num} ->
            NewMoney = Money + Num,
            From ! {?MODULE,NewMoney},
            loop(NewMoney);
        print ->
            io:format("account money:~w~n",[Money]),
            loop(Money)
    end.

    
