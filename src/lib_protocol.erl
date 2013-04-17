-module(lib_protocol).
-compile(export_all).
%% -import(tcp_socket_handler, [socket_login/1,socket_loop/1]).

%% -record(state, {socket, auth, nick, user, host, name}).

-define(TEST_CONNECTION, 'test-apnse').
-define(DEVICE_TOKEN, "130ab12bc1ef517bc574cb1199051a88057f6a9371028005f5e780cdb1588d48").

%%---------------------------------------------------------------------
%% Protocol handlers for not yet authorized connections.
%%---------------------------------------------------------------------
login(Socket, _State, "quit") ->
  close_socket(Socket);
login(Socket, _State, _) ->
  gen_tcp:send(Socket, "Protocol error, connection not authorized.\n").

%%---------------------------------------------------------------------
%% Protocol handlers for normal connections
%%---------------------------------------------------------------------
protocol(Socket, _State, "time") ->
  {Hour,Minute,Second} = erlang:time(),
  String = io_lib:format("Time: ~.2.0w:~.2.0w:~.2.0w~n", [Hour, Minute,Second]),
  gen_tcp:send(Socket, String);
protocol(Socket, _State, "count") ->
  Length=length(pg2:get_members(socket_handlers)),
  String = io_lib:format("Count: ~p~n", [Length]),
  gen_tcp:send(Socket, String);
protocol(Socket, _State, "data " ++ Data) ->
  String="Data: " ++ Data ++ "\r\n",
  gen_tcp:send(Socket, String);
protocol(Socket, _State, "self") ->
  Pid=self(),
  String=io_lib:format("PID: ~p~n", [Pid]),
  gen_tcp:send(Socket, String);
protocol(Socket, _State, "sleep") ->
  sleep(30000),
  close_socket(Socket);
protocol(Socket, _State, "status") ->
  String=io_lib:format("okay~n", []),
  gen_tcp:send(Socket, String);
protocol(Socket, _State, "quit") ->
  close_socket(Socket);
protocol(Socket, _State, "apnse") ->
	apns:connect(?TEST_CONNECTION, fun log_error/2, fun log_feedback/1),
%% 	Pid = case apns:connect(?TEST_CONNECTION, fun log_error/2, fun log_feedback/1) of
%% 		{ok, Pid} ->
%% 			Pid;
%% 		{error, {already_started, Pid}} ->
%% 			Pid
%% 	end,
	apns:send_message(?TEST_CONNECTION, ?DEVICE_TOKEN, "Test Alert", random:uniform(10), "chime"),
	gen_tcp:send(Socket, io_lib:format("apns enhanced test ~p~n", [self()]));
protocol(Socket, _State, Data) -> 
  io:format("Recieved: ~p~n", [Data]),
  gen_tcp:send(Socket, "Invalid command.\n").


log_error(MsgId, Status) ->
  error_logger:error_msg("Error on msg ~p: ~p~n", [MsgId, Status]).

log_feedback(Token) ->
  error_logger:warning_msg("Device with token ~p removed the app~n", [Token]).

%%---------------------------------------------------------------------
%% Generic function for properly closing the socket.
%%---------------------------------------------------------------------
close_socket(Socket) ->
  gen_tcp:close(Socket),
  exit(normal).

sleep(T) ->
    receive
    after T ->
       true
    end.

