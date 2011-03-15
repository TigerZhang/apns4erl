%%-------------------------------------------------------------------
%% @author Fernando Benavides <fernando.benavides@inakanetworks.com>
%% @copyright (C) 2010 Fernando Benavides <fernando.benavides@inakanetworks.com>
%% @doc Apple Push Notification Server for Erlang
%% @end
%%-------------------------------------------------------------------
-module(apns).
-vsn('0.1').

-include("apns.hrl").
-include("localized.hrl").

-export([start/0, stop/0]).
-export([connect/0, connect/1, connect/2, connect/3, disconnect/1]).
-export([send_badge/3, send_message/2, send_message/3, send_message/4, send_message/5, send_message/6]).
-export([message_id/0]).

%% @type msg() = #apns_msg{id           = binary(),
%%                         expiry       = non_neg_integer(),
%%                         device_token = string(),
%%                         alert        = none | apns:alert(),
%%                         badge        = none | integer(),
%%                         sound        = none | string(),
%%                         extra        = [apns_mochijson2:json_property()]}. A Message to send.
%% To understand each field, check <a href="http://developer.apple.com/library/ios/#documentation/NetworkingInternet/Conceptual/RemoteNotificationsPG/CommunicatingWIthAPS/CommunicatingWIthAPS.html">Apple docs</a>
-type msg() :: #apns_msg{}.

%% @type connection() = #apns_connection{ssl_seed          = string(),
%%                                       apple_host        = string(),
%%                                       apple_port        = integer(),
%%                                       cert_file         = string(),
%%                                       timeout           = integer(),
%%                                       error_fun         = fun((binary(), apns:status()) -> stop | any()),
%%                                       feedback_host     = string(),
%%                                       feedback_port     = integer(),
%%                                       feedback_fun      = fun((string()) -> any()),
%%                                       feedback_timeout  = pos_integer()
%%                                       }
-type connection() :: #apns_connection{}.

%% @type status() = no_errors | processing_error | missing_token | missing_topic | missing_payload | 
%%                  missing_token_size | missing_topic_size | missing_payload_size | invalid_token |
%%                  unknown.  Status report as received from Apple
-type status() :: no_errors | processing_error | missing_token | missing_topic | missing_payload | 
                  missing_token_size | missing_topic_size | missing_payload_size | invalid_token |
                  unknown.
-export_type([status/0, msg/0, connection/0]).

%% @type conn_id() = atom() | pid(). Connection Identifier.
-type conn_id() :: atom() | pid().
-export_type([conn_id/0]).

%% @type alert() = string() | #loc_alert{}.
%%        Possibly localized alert. #loc_alert{} is defined in include/localized.hrl
-type alert() :: string() | #loc_alert{}.
-export_type([alert/0]).

%% @doc Starts the application
%% @spec start() -> ok | {error, {already_started, apns}}
-spec start() -> ok | {error, {already_started, apns}}.
start() ->
  _ = application:start(public_key),
  _ = application:start(ssl),
  application:start(apns).

%% @doc Stops the application
%% @spec stop() -> ok
-spec stop() -> ok.
stop() ->
  application:stop(apns).

%% @doc Opens an unnamed connection using the default parameters
%% @spec connect() -> {ok, pid()} | {error, Reason::term()}
-spec connect() -> {ok, pid()} | {error, Reason::term()}.
connect() ->
  connect(default_connection()).

%% @doc Opens an unnamed connection using the given certificate file
%%      or using the given feedback or error function
%%      or using the given #apns_connection{} parameters
%%      or the name and default configuration if a name is given
%% @spec connect(atom() | string() | fun((binary(), apns:status()) -> stop | any()) | fun((string()) -> any()) | #apns_connection{}) -> {ok, pid()} | {error, {already_started, pid()}} | {error, Reason::term()}
-spec connect(atom() | string() | fun((string()) -> _) | #apns_connection{}) -> {ok, pid()} | {error, {already_started, pid()}} | {error, Reason::term()}.
connect(Name) when is_atom(Name) ->
  connect(Name, default_connection());
connect(Connection) when is_record(Connection, apns_connection) ->
  apns_sup:start_connection(Connection);
connect(Fun) when is_function(Fun, 1) ->
  connect((default_connection())#apns_connection{feedback_fun = Fun});
connect(Fun) when is_function(Fun, 2) ->
  connect((default_connection())#apns_connection{error_fun = Fun});
connect(CertFile) ->
  connect((default_connection())#apns_connection{cert_file = CertFile}).

%% @doc Opens an connection named after the atom()
%%      using the given certificate file
%%      using the given feedback or error function
%%      or using the given #apns_connection{} parameters
%% @spec connect(atom(), string() | fun((binary(), apns:status()) -> stop | any()) | fun((string()) -> any()) | #apns_connection{}) -> {ok, pid()} | {error, {already_started, pid()}} | {error, Reason::term()}
-spec connect(atom(), string() | fun((string()) -> _) | #apns_connection{}) -> {ok, pid()} | {error, {already_started, pid()}} | {error, Reason::term()}.
connect(Name, Connection) when is_record(Connection, apns_connection) ->
  apns_sup:start_connection(Name, Connection);
connect(Name, Fun) when is_function(Fun, 1) ->
  connect(Name, (default_connection())#apns_connection{feedback_fun = Fun});
connect(Name, Fun) when is_function(Fun, 2) ->
  connect(Name, (default_connection())#apns_connection{error_fun = Fun});
connect(Name, CertFile) ->
  connect(Name, (default_connection())#apns_connection{cert_file = CertFile}).

%% @doc Opens an connection named after the atom()
%%      using the given feedback and error functions
%% @spec connect(atom(), fun((binary(), apns:status()) -> stop | any()), fun((string()) -> any())) -> {ok, pid()} | {error, {already_started, pid()}} | {error, Reason::term()}
-spec connect(atom(), fun((binary(), apns:status()) -> stop | _), fun((string()) -> _)) -> {ok, pid()} | {error, {already_started, pid()}} | {error, Reason::term()}.
connect(Name, ErrorFun, FeedbackFun) ->
  connect(Name, (default_connection())#apns_connection{error_fun    = ErrorFun,
                                                       feedback_fun = FeedbackFun}).

%% @doc Closes an open connection
%% @spec disconnect(conn_id()) -> ok
-spec disconnect(conn_id()) -> ok.
disconnect(ConnId) ->
  apns_connection:stop(ConnId).

%% @doc Sends a message to Apple
%% @spec send_message(conn_id(), #apns_msg{}) -> ok
-spec send_message(conn_id(), #apns_msg{}) -> ok.
send_message(ConnId, Msg) ->
  apns_connection:send_message(ConnId, Msg).

%% @doc Sends a message to Apple with just a badge
%% @spec send_badge(conn_id(), Token::string(), Badge::integer()) -> ok
-spec send_badge(conn_id(), string(), integer()) -> ok.
send_badge(ConnId, DeviceToken, Badge) -> 
  send_message(ConnId, #apns_msg{device_token = DeviceToken,
                                 badge = Badge}).

%% @doc Sends a message to Apple with just an alert
%% @spec send_message(conn_id(), Token::string(), Alert::alert()) -> ok
-spec send_message(conn_id(), string(), alert()) -> ok.
send_message(ConnId, DeviceToken, Alert) -> 
  send_message(ConnId, #apns_msg{device_token = DeviceToken,
                                 alert = Alert}).

%% @doc Sends a message to Apple with an alert and a badge
%% @spec send_message(conn_id(), Token::string(), Alert::alert(), Badge::integer()) -> ok
-spec send_message(conn_id(), Token::string(), Alert::alert(), Badge::integer()) -> ok.
send_message(ConnId, DeviceToken, Alert, Badge) -> 
  send_message(ConnId, #apns_msg{device_token = DeviceToken,
                                 badge = Badge,
                                 alert = Alert}).

%% @doc Sends a full message to Apple
%% @spec send_message(conn_id(), Token::string(), Alert::alert(), Badge::integer(), Sound::string()) -> ok
-spec send_message(conn_id(), Token::string(), Alert::alert(), Badge::integer(), Sound::string()) -> ok.
send_message(ConnId, DeviceToken, Alert, Badge, Sound) -> 
  send_message(ConnId, #apns_msg{alert = Alert,
                                 badge = Badge,
                                 sound = Sound,
                                 device_token = DeviceToken}).

%% @doc Sends a full message to Apple with extra arguments
%% @spec send_message(conn_id(), Token::string(), Alert::alert(), Badge::integer(), Sound::string(), ExtraArgs::[apns_mochijson2:json_property()]) -> ok
-spec send_message(conn_id(), Token::string(), Alert::alert(), Badge::integer(), Sound::string(), ExtraArgs::[apns_mochijson2:json_property()]) -> ok.
send_message(ConnId, DeviceToken, Alert, Badge, Sound, ExtraArgs) -> 
  send_message(ConnId, #apns_msg{alert = Alert,
                                 badge = Badge,
                                 sound = Sound,
                                 extra = ExtraArgs,
                                 device_token = DeviceToken}).

%% @doc  Generates an "unique" and valid message Id
%% @spec message_id() -> binary()
-spec message_id() -> binary().
message_id() ->
  {_, _, MicroSecs} = erlang:now(),
  Secs = calendar:datetime_to_gregorian_seconds(calendar:universal_time()),
  First = Secs rem 65536,
  Last = MicroSecs rem 65536,
  <<First:2/unsigned-integer-unit:8, Last:2/unsigned-integer-unit:8>>.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
get_env(K, Def) ->
  case application:get_env(apns, K) of
    {ok, V} -> V;
    _ -> Def
  end.

default_connection() ->
  DefaultConn = #apns_connection{},
  DefaultConn#apns_connection{apple_host      = get_env(apple_host,       DefaultConn#apns_connection.apple_host),
                              apple_port      = get_env(apple_port,       DefaultConn#apns_connection.apple_port),
                              cert_file       = get_env(cert_file,        DefaultConn#apns_connection.cert_file),
                              ssl_seed        = get_env(ssl_seed,         DefaultConn#apns_connection.ssl_seed),
                              timeout         = get_env(timeout,          DefaultConn#apns_connection.timeout),
                              error_fun       = case get_env(error_fun,   DefaultConn#apns_connection.error_fun) of
                                                  {M, F} -> fun(I, S) -> M:F(I, S) end;
                                                  Other -> Other
                                                end,
                              feedback_timeout= get_env(feedback_timeout, DefaultConn#apns_connection.feedback_timeout),
                              feedback_fun    = case get_env(feedback_fun,DefaultConn#apns_connection.feedback_fun) of
                                                  {M, F} -> fun(T) -> M:F(T) end;
                                                  Other -> Other
                                                end,
                              feedback_host   = get_env(feedback_host,    DefaultConn#apns_connection.feedback_host),
                              feedback_port   = get_env(feedback_port,    DefaultConn#apns_connection.feedback_port)
                             }.