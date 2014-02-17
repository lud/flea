-module(chat).
%% chat app API.
-export([start/0]).
-export([broadcast/1]). %% export for apply_after
%% Yaws appmod API
-export([out/1]).
%% Flea API
-export([init/3,info/2,stream/2,terminate/1]).
-record(chatclient, {web_client_pid,messages,userid,nickname}).
%%% -----------------------------------------------------------------
%%% chat API.
%%% -----------------------------------------------------------------

start() ->
	PrivDir =
		fun(Mod) ->
			Ebin = filename:dirname(code:which(Mod)),
			filename:join(filename:dirname(Ebin), "priv")
		end,
	SL =
		[ {servername, "localhost"},
		  {listen, {0,0,0,0}},
		  {appmods, [{"/flea", ?MODULE}]},
		  {arg_rewrite_mod, yaws_vdir},
		  {opaque,[{"vdir","/js/ "++ PrivDir(flea) }]}
		],
	GL =
		[ {cache_refresh_secs,1},
		  {logdir, "log"}
		],
	io:format("Bullet dir : ~p\n",[PrivDir(flea)]),
	Docroot = PrivDir(?MODULE),
	ok = application:start(gproc),
	yaws:start_embedded(Docroot,SL,GL).

broadcast(Nickname,Message) ->
	broadcast([Nickname,": ",Message]).
broadcast(Message) ->
	gproc:send({p,l,chat},{message,Message}),
	ok.

chat_subscribe(Userid,NickName) ->
	case chatclient_pid(Userid)
		of undefined -> chatclient_init(Userid,NickName)
		 ; _ -> ok
	end,
	Chatclient_pid = chatclient_pid(Userid),
	io:format("Chatclient pid is ~p\n",[Chatclient_pid]),
	Chatclient_pid ! {set_client,self()}.

chat_unsubscribe(Userid) ->
	catch chatclient_pid(Userid) ! {set_client,undefined}.

chatclient_pid(Userid) -> gproc:where({n,l,{chatclient,Userid}}).

chatclient_exists(Userid) ->
	case gproc:where({n,l,{chatclient,Userid}})
		of undefined -> false
		 ; _ -> true
	end.

chatclient_init(Userid,NickName) ->
	Waiter = self(),
	spawn(fun() -> chatclient_loop_init(Userid,NickName,Waiter) end),
	receive chatclient_ready -> ok after 1000 -> exit(bad_init) end.

chatclient_loop_init(Userid,Nickname,Waiter) ->
 	%% register gproc userid to identify the client
 	gproc:reg({n,l,{chatclient,Userid}}),
 	gproc:reg({p,l,chat}),
 	Waiter ! chatclient_ready,
	io:format("Chatclient ''~p''/~p initialized \n",[{Userid,Nickname},self()]),
	timer:apply_after(1000, ?MODULE, broadcast, [[Nickname," joined the chat."]]),
	chatclient_loop(#chatclient{userid=Userid,nickname=Nickname,messages=[],web_client_pid=undefined}).

chatclient_loop(State=#chatclient{nickname=Nickname,web_client_pid=undefined}) ->
	receive
		{set_client,Pid} when is_pid(Pid)->
			erlang:monitor(process,Pid),
			Pid ! incoming_messages, %% @todo test to not send if no messages even if there is no message
			io:format("Chatclient got subscriber ~p\n",[Pid]),
			chatclient_loop(State#chatclient{web_client_pid=Pid})
	after 3*1000 -> %% bullet heartbeat is 20*1000
			broadcast([Nickname," left the chat."]),
			io:format("Chatclient timeout\n")
	end;
chatclient_loop(State=#chatclient{userid=Userid,nickname=Nickname,messages=Messages,web_client_pid=Client}) ->
	receive
		{message,M} ->
			io:format("Chatclient got message ~p\n",[M]),
			case Client
				of undefined -> ok
				 ; Pid -> Pid ! incoming_messages
			end,
			chatclient_loop(State#chatclient{messages=[M|Messages]});
		{fetch,From} ->
			From ! {messages,Messages},
			chatclient_loop(State#chatclient{messages=[]});
		{set_client,undefined} ->
			io:format("Chatclient lost subscriber (unsubscribe)\n"),
			chatclient_loop(State#chatclient{web_client_pid=undefined});
		{'DOWN',_,_,_,_} = XXX ->
			io:format("Chatclient lost subscriber (DOWN)\n ~p",[XXX]),
			chatclient_loop(State#chatclient{web_client_pid=undefined});
		keepalive ->
			chatclient_loop(State);
		_Any ->
			io:format("Chatclient got unknown info ~p\n",[_Any]),
			chatclient_loop(State)
	after 25*1000 -> %% bullet heartbeat is 20*1000
			broadcast([Nickname," left the chat."]),
			io:format("Chatclient timeout\n")
	end.

fetch_chatclient(Userid) ->
	case chatclient_pid(Userid)
		of undefined -> no_chatclient
		 ; Pid ->
		 	Pid ! {fetch,self()},
		 	receive
		 		{messages,Messages} -> {ok,Messages}
	 		after
	 			1000 -> {ok,[]}
	 		end
	end.

keepalive(Userid) ->
	catch chatclient_pid(Userid) ! keepalive.
%%% -----------------------------------------------------------------
%%% Yaws appmod callbacks
%%% -----------------------------------------------------------------

out(A) ->
	flea:out(A,[{handler,?MODULE}]).

%%% -----------------------------------------------------------------
%%% Flea callbacks
%%% -----------------------------------------------------------------

init(Arg,_,false) ->
	%% Handling incoming message for polling and eventsource
	Pathinfo = yaws_api:arg_pathinfo(Arg),
	[NickName,Userid|_] = string:tokens(Pathinfo,"/"),
	{ok, {NickName,Userid}};

init(Arg,_,Active) ->
	Pathinfo = yaws_api:arg_pathinfo(Arg),
	[NickName,Userid|_] = string:tokens(Pathinfo,"/"),
	chat_subscribe(Userid,NickName),
	{ok, {NickName,Userid}}.

info(incoming_messages,{NickName,Userid}=State)  ->
	% io:format("info received in handler ~p~n", [Message]),
	{ok,Messages} = fetch_chatclient(Userid),
	case Messages
		of [] ->
			{ok,State}
		 ; Messages ->
			Rep = lists:append(Messages),
			{reply,Rep,State}
	end;

info(Message,{NickName,Userid}=State)  ->
	io:format("info received in handler ~p~n", [Message]),
	{ok,{NickName,Userid}}.

stream(<<"ping">>, {NickName,Userid}=State) ->
	io:format("Ping ~p~n",[NickName]),
	keepalive(Userid),
	{ok, State};

stream(Data, {NickName,Userid}=State) ->
	io:format("stream received ~p~n", [Data]),
	broadcast(NickName,Data),
	{ok, State}.

terminate(Userid) ->
	chat_unsubscribe(Userid),
	ok.
