%%
 % Copyright (c) <2016> <CodeHigh>
 %
 % Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:
 %
 % The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.
 %
 % THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 %

%%%-------------------------------------------------------------------
%% @doc emqtt_publisher top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(emqtt_publisher_sup).
-author("Pucci").

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