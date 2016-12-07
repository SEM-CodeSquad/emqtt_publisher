%%
 % Copyright (C) 2016 CodeHigh.
 %     Permission is granted to copy, distribute and/or modify this document
 %     under the terms of the GNU Free Documentation License, Version 1.3
 %     or any later version published by the Free Software Foundation;
 %     with no Invariant Sections, no Front-Cover Texts, and no Back-Cover Texts.
 %     A copy of the license is included in the section entitled "GNU
 %     Free Documentation License".
 %
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