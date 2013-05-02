%% @author zhanghu
%% @doc @todo Add description to apns_manager.

-module(apns_manager).
-behaviour(gen_server).

-include("apns.hrl").
-include("localized.hrl").

-record(state, {
				current_msg_pos = 1 :: integer(),	%% for performance concern, pos is counted from tail
				messages_sent :: list({binary(), apns_msg()}),
				manager_id :: atom()}).
-type state() :: #state{}.

%% ====================================================================
%% API functions
%% ====================================================================
-export([start_link/1, send_message/8, handle_call/3, handle_cast/2,
		 handle_info/2, init/1, terminate/2, code_change/3]).
-export([start_manager/1]).

-export([manager_id_to_connection_id/1, connection_id_to_manager_id/1]).

%% start_link/1
-spec start_link([]) -> {ok, pid()} | {error, {already_started, pid()}}.
start_link(MngId) ->
	gen_server:start_link({local, MngId}, ?MODULE, MngId, []).

start_manager(MngId) ->
	apns_manager_sup:start_manager(MngId).
	
-spec init(Args :: term()) -> Result when
	Result :: {ok, State}
			| {ok, State, Timeout}
			| {ok, State, hibernate}
			| {stop, Reason :: term()}
			| ignore,
	State :: term(),
	Timeout :: non_neg_integer() | infinity.
%% ====================================================================
init(MngId) ->
	io:format("apns_manager init ~p~n", [atom_to_list(MngId)]),
	%% ets:new(MngId, [set,named_table]),
	apns:connect(manager_id_to_connection_id(MngId), fun log_error/3, fun log_feedback/1),
	{ok, #state{manager_id=MngId, messages_sent=[]}}.

%% send_message(ConnId, MsgId, DeviceToken, Alert, Badge, Sound, Expiry, ExtraArgs) ->

handle_cast({sendmsg, MngId, MsgId, DeviceToken, Alert, Badge, Sound, Expiry, ExtraArgs}, State) ->
	%% ets:insert(MngId, #apns_msg{id=MsgId, expiry=Expiry, device_token=DeviceToken, alert=Alert,
	%% 							 badge=Badge, sound=Sound, extra=ExtraArgs}),
	MessagesSent = [{MsgId, #apns_msg{id=MsgId, expiry=Expiry, device_token=DeviceToken, alert=Alert,
	 							 badge=Badge, sound=Sound, extra=ExtraArgs}} | State#state.messages_sent],
	State1 = State#state{messages_sent = MessagesSent},
	% handle_cast(sendmsg, State1),
	gen_server:cast(MngId, sendmsg),
	{noreply, State1};
%%handle_cast(sendmsg, State=#state{current_msg = undefined}) ->
%%	handle_cast(sendmsg, State#state{current_msg = ets:first(State#state.manager_id)}),
%%	{noreply, State};
	%%{noreply, State#state{current_msg = ets:first(State#state.manager_id)}};
%%handle_cast(sendmsg, State=#state{current_msg = '$end_of_table'}) ->
%%	io:format("end_of_table: ~p~n", [State#state.manager_id]),
%%	ets:delete_all_objects(State#state.manager_id),
%%	{noreply, State};
handle_cast(sendmsg, State) ->
	% try
		%% Msg = State#state.current_msg,
		MngId = State#state.manager_id,
		%% NextMsg = ets:next(MngId, Msg),
		RevertMessagesSent = lists:reverse(State#state.messages_sent),
		CurrentMsgPos = State#state.current_msg_pos,
		{_, Msg} = lists:nth(CurrentMsgPos, RevertMessagesSent),

		io:format("current_msg_pos ~p~n~p~n~n~n", [CurrentMsgPos, State#state.messages_sent]),

		ConnId = manager_id_to_connection_id(MngId),
		apns:connect(ConnId, fun log_error/3, fun log_feedback/1),
		apns:send_message(ConnId, Msg#apns_msg.device_token,
			Msg#apns_msg.alert, Msg#apns_msg.badge, Msg#apns_msg.sound),
		%%apns:send_message(ConnId, DeviceToken, Alert, random:uniform(10), "chime"),
		%%apns:send_message(manager_id_to_connection_id(MngId), Msg),
		%%apns:send_message(manager_id_to_connection_id(MngId), State#state.),
		NextMessagePos = CurrentMsgPos + 1,
		{noreply, State#state{current_msg_pos = NextMessagePos}}
	% catch
	% 	_:_ ->
	% 		io:format("handle_cast error"),
	% 		{noreply, State#state{current_msg_pos = 1, messages_sent = []}}
	% end
	;
handle_cast(stop, State) ->
  {stop, normal, State}.

send_message(MngId, MsgId, DeviceToken, Alert, Badge, Sound, Expiry, ExtraArgs) ->
	gen_server:cast(MngId, {
		sendmsg, MngId, MsgId,
		DeviceToken, Alert, Badge, Sound, Expiry, ExtraArgs}).

%% @hidden
-spec handle_call(X, reference(), state()) -> {stop, {unknown_request, X}, {unknown_request, X}, state()}.
handle_call(Request, _From, State) ->
  {stop, {unknown_request, Request}, {unknown_request, Request}, State}.

-spec handle_info({ssl, tuple(), binary()} | {ssl_closed, tuple()} | X, state()) -> {noreply, state()} | {stop, ssl_closed | {unknown_request, X}, state()}.
handle_info(Request, State) ->
  {stop, {unknown_request, Request}, State}.

%% @hidden
-spec terminate(term(), state()) -> ok.
terminate(_Reason, State) -> 
	%%ets:delete(State#state.manager_id), 
	ok.

%% @hidden
-spec code_change(term(), state(), term()) -> {ok, state()}.
code_change(_OldVsn, State, _Extra) ->  {ok, State}.

%% ====================================================================
%% Internal functions
%% ====================================================================
log_error(MsgId, Status, Pid) ->
	ConnProcess = erlang:process_info(Pid, registered_name),
	%% atom_to_binary(ConnProcess),
 	error_logger:error_msg("Error on msg ~p: ~p ~p~n", [MsgId, Status, erlang:process_info(Pid, registered_name)]).
  

log_feedback(Token) ->
  error_logger:warning_msg("Device with token ~p removed the app~n", [Token]).

-spec manager_id_to_connection_id(atom()) -> atom().
manager_id_to_connection_id(MngId) ->
	MngIdBinary = erlang:atom_to_binary(MngId, latin1),
	ConnIdBinary = <<MngIdBinary/binary, <<"_conn">>/binary>>,
	erlang:binary_to_atom(ConnIdBinary, latin1).
	%% list_to_atom(atom_to_list(MngId) ++ "_conn").

-spec connection_id_to_manager_id(atom()) -> atom().
connection_id_to_manager_id(ConnId) ->
	ConnIdBinary = erlang:atom_to_binary(ConnId, latin1),
	Pos = erlang:byte_size(ConnIdBinary) - erlang:byte_size(<<"_conn">>),
	{MngIdBinary, <<"_conn">>} = erlang:split_binary(ConnIdBinary, Pos),
	erlang:binary_to_atom(MngIdBinary, latin1).
