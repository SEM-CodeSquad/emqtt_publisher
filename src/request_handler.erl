%%%-------------------------------------------------------------------
%%% @author Pucci
%%% @copyright (C) 2016, <COMPANY>
%%% @doc GET echo handler.
%%%
%%% @end
%%% Created : 12. Nov 2016 07:03
%%%-------------------------------------------------------------------
%% Feel free to use, reuse and abuse the code in this file.

-module(request_handler).

-export([init/2]).

-export([terminate/3]).

%%====================================================================
%% API functions
%%====================================================================
%% Initial function that receives the request and forward it to check the request.
init(Req0, Opts) ->
  Method = cowboy_req:method(Req0),
  check_request(Req0, Method),
  {ok, Req0, Opts}.

%% Function that checks if the request method is the expected one and answers the requester according to the request
%% method. If the request method is the one expected it starts the function parse_qs in a new process.
check_request(Req, <<"POST">>) ->
  cowboy_req:reply(200, [{<<"content-type">>, <<"text/plain; charset=utf-8">>}], gen_html(), Req),
  spawn(fun() -> parse_qs(Req) end);
check_request(Req, _) ->
  %% Method not allowed.
  cowboy_req:reply(405, Req).

%% Function that parses the query string sent in the request.
parse_qs(Req) ->
  Method = cowboy_req:method(Req),
  #{broker := Broker, topic := Topic, msg := Msg, qos := QoS, retain := Retain, password := Password} = cowboy_req:match_qs([{broker, [], undefined},
    {topic, [], undefined}, {msg, [], undefined},{qos, [], undefined}, {retain, [], undefined}, {password, [], undefined}], Req),
  publish(Method,Broker, Topic, Msg, Password, QoS, Retain).

%% Function that identify if all need parameters were found and if it is found message them to the emqtt server.
publish(<<"POST">>, undefined, _, _, _, _, _) ->
  io:format("Missing broker Parameter.");
publish(<<"POST">>, _, undefined, _, _, _, _) ->
  io:format("Missing topic Parameter.");
publish(<<"POST">>, _, _, undefined, _, _, _) ->
  io:format("Missing msg Parameter.");
publish(<<"POST">>, _, _, _, undefined, _, _) ->
  io:format("Not Authorized.");
publish(<<"POST">>, _, _, _, _, undefined, _) ->
  io:format("Missing QoS Parameter");
publish(<<"POST">>, _, _, _, _, _, undefined) ->
  io:format("Missing retain Parameter");
publish(<<"POST">>, Broker, Topic, Msg, <<"CodeHigh_SmartMirror">>, QoS, Retain) ->
  emqtt ! {mqtt, Broker, Topic, Msg, QoS, Retain};
publish(_, _, _, _, _, _, _) ->
  io:format("Method not allowed").

%% Function that generate the echo reply that is send to the requester
gen_html() ->
  [<<"Message Received!">>].

%% Function that terminates the request handler.
terminate(_Reason, _Req, _State) ->
  ok.
