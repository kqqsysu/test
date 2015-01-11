-module(bank_account).
-behaviour(gen_server).
-author("kongqingquan kqqsysu@gmail.com").

%% 回调接口
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         code_change/3,
         terminate/2
        ]).


-export([
        start/3,    %% 开启进程
        withdraw/2, %% 取款
        deposit/2,  %% 存取
        print/1,    %% 打印帐号信息
        stop/1      %% 停止进程
        ]).

%% 定义帐号
-record(account,{
                id = 0,     %% ID  
                name = "",  %% 帐号名
                money = 0   %% 帐号余额
            }).


-define(PRINT(Msg),io:format(Msg ++ "\n")).
-define(PRINT(Format,Msg),io:format(Format ++ "\n",Msg)).

%% 初始化account
init([ID,Name,Money]) ->
    ?PRINT("bank account init,ID:~w,Name:~p,Money:~w",[ID,Name,Money]),
    State = #account{id = ID,name = Name,money = Money},
    {ok,State}.

%% 打印帐号信息
handle_cast(print,State = #account{id = ID,name = Name,money = Money}) ->
    ?PRINT("account info,ID:~w,Name:~p,Money:~w",[ID,Name,Money]),
    {noreply,State};
handle_cast(stop,State) ->
    %% 返回stop，进程将停止，调用terminate
    ?PRINT("handle cast stop"),
    {stop,normal,State}.

%% 取款
handle_call({withdraw,Num},_From,State = #account{name = Name,money = Money}) when Num > 0, Num =< Money ->
    NewMoney = Money - Num,
    NewState = State#account{money = NewMoney},
    ?PRINT("~p withdraw:~w,NewMoney:~w",[Name,Num,NewMoney]),
    {reply,true,NewState};
%% 存款
handle_call({deposit,Num},_From,State = #account{name = Name,money = Money}) when Num > 0 ->
    NewMoney = Money + Num,
    NewState = State#account{money = NewMoney},
    ?PRINT("~p deposit:~w,NewMoney:~w",[Name,Num,NewMoney]),
    {reply,true,NewState}.

%% handle_info，处理直接发给进程的消息
handle_info(Msg,State) ->
    ?PRINT("handle_info receive msg:~p",[Msg]),
    {noreply,State}.

code_change(_OldVsn, _State, _Extra) ->
    ok.

%% 进程停止时，回调terminate
terminate(Reason,#account{id = ID,name = Name,money = Money}) ->
    ?PRINT("process stop,Reason:~p",[Reason]),
    ?PRINT("account Info,ID:~w,Name:~p,Money:~w",[ID,Name,Money]),
    ok.

%% 开启帐号进程，将回调init/1函数，返回{ok,Pid}
start(ID,Name,Money) ->
    gen_server:start(?MODULE,[ID,Name,Money],[]).

withdraw(Pid,Num) ->
    gen_server:call(Pid,{withdraw,Num}).

deposit(Pid,Num) ->
    gen_server:call(Pid,{deposit,Num}).

print(Pid) ->
    gen_server:cast(Pid,print).

stop(Pid) ->
    gen_server:cast(Pid,stop).
