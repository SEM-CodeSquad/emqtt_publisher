%%
 % Copyright (c) <2016> <CodeHigh>
 %
 % Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:
 %
 % The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.
 %
 % THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 %
-module(emqtt_manager).
-author("Pucci").

%% API
-export([start/0]).

%%====================================================================
%% API functions
%%====================================================================

%% Function that registers the server name and initiate tune in.
start() ->
  register(emqtt, self()),
  manager_tune_in().

%% Function that waits for the first message for then connect to the broker and initiate to listen for new messages.
manager_tune_in() ->
  io:format("Emqtt Manager Tuned... ~n"),
  receive
    {mqtt, Broker, Topic, Msg, QoS, Retain} -> Pid = init_broker(Broker),
                                  timer:sleep(100),
                                  spawn(fun() -> monitor_publisher(Pid, Topic, Msg, QoS, Retain, 2) end),
                                  manager_listen(Broker, Pid)
  end.

%% Function that receive messages. This function loops with the already connected broker and if it receives a new broker
%% instance it will disconnect from the old broker and connect to this new one. Also it monitors the connection with the
%% and in case that get disconnected it will reconnect.
manager_listen(Broker, Pid) ->
  io:format("Emqtt Manager Listening... ~n"),
  receive
    {'DOWN', _Ref, process, Pid, Reason}       ->   io:format("Broker Connection Crashed Reason = ~p ~n", [Reason]),
                                                    NewPid = init_broker(Broker),
                                                    manager_listen(Broker, NewPid);
    {mqtt, Broker, Topic, Msg, QoS, Retain}                 ->   spawn(fun() -> monitor_publisher(Pid, Topic, Msg, QoS, Retain, 2) end),
                                                    manager_listen(Broker, Pid);
    {mqtt, NewBroker, Topic, Msg, QoS, Retain}              ->   disconnect(Pid),
                                                    NewPid = init_broker(NewBroker),
                                                    timer:sleep(100),
                                                    spawn(fun() -> monitor_publisher(NewPid, Topic, Msg, QoS, Retain, 2) end),
                                                    manager_listen(NewBroker, NewPid)
  end.

%% This function spawns a process which is responsible for publishing then it monitors this process.
monitor_publisher(Hostname, Topic, Msg, QoS, Retain, N) ->
  Pid = spawn(fun() -> publish(Hostname, Topic, Msg, QoS, Retain) end),
  monitor(process, Pid),
  monitor_handler(Pid, Hostname, Topic, Msg, QoS, Retain, N).

%% Auxiliary function that monitors the publisher if the publisher crashes it will attempt to publish for 2 more times.
monitor_handler(_Pid, _Hostname, _Topic, _Msg, _QoS, _Retain, 0) -> io:format("Failed to Publish! ~n");
monitor_handler(Pid, Hostname, Topic, Msg, QoS, Retain, N) ->
  receive
    {'DOWN', _Ref, process, Pid, normal}  -> io:format("Published with Success! ~n");
    {'DOWN', _Ref, process, Pid, crash}   -> io:format("Trying for the ~p time! ~n", [N-1]),
                                             monitor_publisher(Hostname, Topic, Msg, QoS, Retain, N-1);
    {'DOWN', _Ref, process, Pid, Reason} -> io:format("Failed to Publish. Reason = ~p ~n", [Reason]),
                                            monitor_publisher(Hostname, Topic, Msg, QoS, Retain, N-1)

  after 3000                              -> io:format("Time Out! ~n")
  end.

%% Function to initiate the module connecting to the broker and publishing to it a presence msg.
init_broker(Host) ->
  try
      io:format("Connecting to the Broker ~p... ~n", [Host]),
      Pid = connect(Host),
      monitor(process, Pid),
      io:format("Connected to the Broker... ~n"),
      timer:sleep(100),
      presence_message(Pid),
      io:format("Presence Msg Published! ~n"),
      Pid

  catch
      E:R  -> io:format("Reason for the crash: ~p~n", [{E,R}]), exit(crash)
  end .

%% Function to disconnect from the broker
disconnect(Pid) ->
  disconnect_message(Pid),
  timer:sleep(100),
  emqttc:disconnect(Pid).

%% Function to connect to the broker
connect(H) ->
  {ok, ClientID} = application:get_env(client_id),
  Host = binary_to_list(H),
  {ok, Pid} = emqttc:start_link([{host, Host},
                                 {client_id, ClientID},
                                 {reconnect, 3},
                                 {clean_sess, false},
                                 {logger, {console, info}}]),
  Pid.

%% Function to publish to the broker using the defined QoS or the defined retained msg.
publish(Pid, Topic, Msg, QoS, <<"true">>) ->
  try
      Q = binary_to_integer(QoS),
      Retain = true,
      emqttc:publish(Pid, Topic, Msg, [{qos, Q}, {retain, Retain}])
  catch
    E:R  -> io:format("Not Published Reason: ~p~n", [{E,R}]), exit(crash)
  end;
publish(Pid, Topic, Msg, QoS, <<"false">>) ->
  try
    Q = binary_to_integer(QoS),
    Retain = false,
    emqttc:publish(Pid, Topic, Msg, [{qos, Q}, {retain, Retain}])
  catch
    E:R  -> io:format("Not Published Reason: ~p~n", [{E,R}]), exit(crash)
  end .

%% Function that publishes the presence msg.
presence_message(Pid) ->
  try
      {ok, ClientID} = application:get_env(client_id),
      {Mega, Secs, _} = erlang:timestamp(),
      Timestamp = Mega*1000000 + Secs,
      Msg = "{\"version\":\"1\",\"groupName\":\"CodeHigh\",\"groupNumber\":\"6\",\"connectedAt\":\"" ++ integer_to_list(Timestamp) ++
        "\",\"rfcs\":\"[1,6,10, 11, 12]\",\"clientVersion\":\"1.0\",\"clientSoftware\":SmartMirror\"}",
      Topic = "presence/" ++ ClientID,
      T = list_to_binary(Topic),
      M = list_to_binary(Msg),
      publish(Pid, T, M, <<"1">>, <<"true">>)
  catch
    E:R  -> io:format("Precense Msg Not Published Reason: ~p~n", [{E,R}])
  end .

%% Function that sends the last will when disconnected.
disconnect_message(Pid) ->
  try
    {ok, ClientID} = application:get_env(client_id),
    Msg = [],
    Topic = "presence/" ++ ClientID,
    T = list_to_binary(Topic),
    M = list_to_binary(Msg),
    publish(Pid, T, M, <<"1">>, <<"true">>)
  catch
    E:R  -> io:format("Precense Msg Not Published Reason: ~p~n", [{E,R}])
  end .


