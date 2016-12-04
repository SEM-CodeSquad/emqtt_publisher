%%%-------------------------------------------------------------------
%% @doc emqtt_publisher public API
%% @end
%%%-------------------------------------------------------------------

-module(emqtt_publisher_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%%====================================================================
%% API
%%====================================================================

%% Function that starts the application.
start(_StartType, _StartArgs) ->
    emqtt_publisher_sup:start_link().

%%--------------------------------------------------------------------
%% Function that stops the application.
stop(_State) ->
    ok.

%%====================================================================
%% Internal functions
%%====================================================================

