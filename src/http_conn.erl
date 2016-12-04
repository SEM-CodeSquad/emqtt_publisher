%%%-------------------------------------------------------------------
%%% @author Pucci
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 16. Nov 2016 13:52
%%%-------------------------------------------------------------------
-module(http_conn).
-author("Pucci").

%% API
-export([cowboy_run/0]).

%%====================================================================
%% API
%%====================================================================
%% Function that starts the cowboy server in an specific port and start the request handler where any http request
%% is directed to.
cowboy_run() ->
  {ok, Port} = application:get_env(port),
  Dispatch = cowboy_router:compile([{'_', [{"/", request_handler, []}]}]),
  {ok, _} = cowboy:start_http(http, 100, [{port, Port}],[{env, [{dispatch, Dispatch}]}]).