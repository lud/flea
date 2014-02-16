-module(chat).
%% clock API.
-export([start/0]).
%% Yaws appmod API
-export([out/1]).
%% Flea API
-export([init/3,info/2,stream/2,terminate/1]).
-compile(export_all).
-record(mailbox, {client_pid,messages,name}).
%%% -----------------------------------------------------------------
%%% clock API.
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
	Docroot = PrivDir(?MODULE),
	ok = application:ensure_started(gproc),
	yaws:start_embedded(Docroot,SL,GL).

broadcast(Message) ->
	gproc:send({p,l,chat},{message,Message}),
	ok.

chat_subscribe(UserID) ->
	case mailbox_pid(UserID)
		of undefined -> mailbox_init(UserID)
		 ; _ -> ok
	end,
	Mailbox_pid = mailbox_pid(UserID),
	io:format("Mailbox pid is ~p\n",[Mailbox_pid]),
	Mailbox_pid ! {set_client,self()}.

chat_unsubscribe(UserID) ->
	catch mailbox_pid(UserID) ! {set_client,undefined}.

mailbox_pid(Name) -> gproc:where({n,l,{mailbox,Name}}).

mailbox_exists(Name) ->
	case gproc:where({n,l,{mailbox,Name}})
		of undefined -> false
		 ; _ -> true
	end.

mailbox_init(UserID) ->
	Waiter = self(),
	spawn(fun() -> mailbox_loop_init(UserID,Waiter) end),
	receive mailbox_ready -> ok after 1000 -> exit(bad_init) end.

mailbox_loop_init(UserID,Waiter) ->
 	%% register gproc name to identify the client
 	gproc:reg({n,l,{mailbox,UserID}}),
 	gproc:reg({p,l,chat}),
 	Waiter ! mailbox_ready,
	io:format("Mailbox ''~p''/~p initialized \n",[UserID,self()]),
	mailbox_loop(#mailbox{name=UserID,messages=[],client_pid=undefined}).

mailbox_loop(State=#mailbox{name=Name,messages=Messages,client_pid=Client}) ->
	receive
		{message,M} ->
			io:format("Mailbox got message ~p\n",[M]),
			case Client
				of undefined -> ok
				 ; Pid -> Pid ! incoming_messages
			end,
			mailbox_loop(State#mailbox{messages=[M|Messages]});
		{fetch,From} ->
			From ! {messages,Messages},
			mailbox_loop(State#mailbox{messages=[]});
		{set_client,Pid} when is_pid(Pid)->
			erlang:monitor(process,Pid),
			Pid ! incoming_messages, %% even if there is no message
			io:format("Mailbox got subscriber ~p\n",[Pid]),
			mailbox_loop(State#mailbox{client_pid=Pid});
		{set_client,undefined} ->
			io:format("Mailbox lost subscriber (unsubscribe)\n"),
			mailbox_loop(State#mailbox{client_pid=undefined});
		{'DOWN',_,_,_,_} = XXX ->
			io:format("Mailbox lost subscriber (DOWN)\n ~p",[XXX]),
			mailbox_loop(State#mailbox{client_pid=undefined});
		keepalive ->
			mailbox_loop(State);
		_Any ->
			io:format("Mailbox got unknown info ~p\n",[_Any]),
			mailbox_loop(State)
	after 25*1000 -> %% bullet heartbeat is 20*1000
			broadcast([Name," left the chat."]),
			io:format("Mailbox timeout\n")
	end.

fetch_mailbox(Name) ->
	case mailbox_pid(Name)
		of undefined -> no_mailbox
		 ; Pid ->
		 	Pid ! {fetch,self()},
		 	receive
		 		{messages,Messages} -> {ok,Messages}
	 		after
	 			1000 -> {ok,[]}
	 		end
	end.

keepalive(Name) ->
	catch mailbox_pid(Name) ! keepalive.
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
	UserID = yaws_api:arg_appmoddata(Arg),
	{ok, UserID};

init(Arg,_,Active) ->
	UserID = yaws_api:arg_appmoddata(Arg),
	chat_subscribe(UserID),
	% io:format("/~s init, active=~p in ~p~n",[yaws_api:arg_appmoddata(Arg),Active,self()]),
	{ok, UserID}.

info(incoming_messages,UserID)  ->
	% io:format("info received in handler ~p~n", [Message]),
	{ok,Messages} = fetch_mailbox(UserID),
	case Messages
		of [] ->
			{ok,UserID}
		 ; Messages ->
			Rep = lists:append(Messages),
			{reply,Rep,UserID}
	end;

info(Message,UserID)  ->
	io:format("info received in handler ~p~n", [Message]),
	{ok,UserID}.

stream(<<"ping">>, UserID) ->
	io:format("Ping~n"),
	keepalive(UserID),
	{ok, UserID};
stream(Data, State) ->
	io:format("stream received ~p~n", [Data]),
	broadcast(Data),
	{ok, State}.

terminate(UserID) ->
	chat_unsubscribe(UserID),
	ok.
