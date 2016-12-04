%%%-------------------------------------------------------------------
%% @doc emqtt_publisher top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(emqtt_publisher_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================
%% Supervisor start function.
start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================
%% Function that initiate the cowboy connection and starts the supervisor loop.
init([]) ->
    spawn(fun() -> http_conn:cowboy_run() end),
    process_flag(trap_exit, true),
    loop().

%%====================================================================
%% Internal functions
%%====================================================================
%% Loop function that starts the emqtt server and monitors its state. In case of any unexpected crash it will
%% simple loop and start the server again.
loop() ->
    Pid = spawn_link(fun() -> emqtt_manager:start() end),
    receive
        {'EXIT', _From, shutdown} ->
            exit(shutdown); % will kill the child too
        {'EXIT', Pid, Reason} ->
            io:format("Process ~p exited for reason ~p~n",[Pid,Reason]),
            loop()
    end.