-module(flea).
-export([out/2]).
-export([init/1,handle_message/1,handle_message/2,handle_info/2,terminate/2]).

-define(Y,yaws_api).
-define(POLL_TIMEOUT, 30000).
-define(WS_TIMEOUT, 60000).

-include_lib("yaws/include/yaws_api.hrl").


%% todo gros refactoring à prévoir. Écrire les functions
%% handler_init,handler_stream,handler_info,handler_terminate pour
%% gérer de façon commune les appels au handler

%% Eventsource & polling : GET. En parallèle d'Eventsource et polling
%% on peut recevoir des requêtes POST.

%% Possible improvements : stop passing Arg into stream, info &
%% terminate, pass it only in init so the user is responsible to keep
%% it in it's own state if ever it needs it. So the flea_eventsource
%% gen_server will be more lightweight


-record(state, {
	handler :: module(),
	handler_state :: term()
}).


% @todo out/1 with Opts/Handler taken from arg.opaque

out(Arg,Opts) ->
	case ?Y:get_header(?Y:arg_headers(Arg), upgrade)
		of "websocket" ->
				WebSocketOpts = [{callback,{basic,Opts}}],
				{websocket,?MODULE,WebSocketOpts}
		 ; _ ->
				Method = ?Y:http_request_method(?Y:arg_req(Arg)),
				handle_req(Arg,Method,Opts)
	end.

%%% ------------------------------------------------------------------
%%% Normal requests
%%% ------------------------------------------------------------------

%% GET request : it's a polling or an eventsource initialization
handle_req(Arg,'GET',Opts) ->
	GetMode = get_mode(Arg),
	Active =
		case GetMode
			of poll -> once
			 ; eventsource -> true
		end,
	Handler = lkup(handler,Opts),
	case GetMode
		of poll ->
				{ok, HSt} = Handler:init(Arg,Opts,Active), %% @todo handle {error, Reason} | close / check if Yaws sends 500 errors
				poll(Arg,#state{handler_state=HSt,handler=Handler})
		 ; _otherwise ->
				{ok, Pid} = gen_server:start(flea_eventsource, [Arg,Handler,Opts], []), %% @todo add a simple_one_for_one supervisor, use supervisor:start_child
				yaws_sse:headers(Pid)
	end;

%% POST request : it's an incomimng message for a polling or an
%% eventsource connection
handle_req(Arg,'POST',Opts) ->
	Data = ?Y:arg_clidata(Arg),
	Handler = lkup(handler,Opts),
	%% Active is always false because an active connection
	%% (eventsource or polling may be running in parrallel to handle
	%% push messages with Handler:info/)
	{ok, HSt} = Handler:init(Arg,Opts,Active=false), %% @todo handle {error, Reason} | close / check if Yaws sends 500 errors
	{Reply,NewHandlerState} =
		case Handler:stream(Data,Arg,HSt)
			of {reply,R,NewHSt}	-> {{html,R} ,NewHSt} %% todo check content, default to JSON
			 ; {ok,NewHSt}     	-> {ok,NewHSt}
		end,
	Handler:terminate(Arg,NewHandlerState),
	Reply.

%% This function is the long-polling handler. It waits for a message
%% until POLL_TIMEOUT
poll(Arg,State=#state{handler_state=HSt,handler=Handler}) ->
	receive Message ->
		case Handler:info(Message,Arg,HSt)
			of {reply,Reply,NewHSt} ->
					Handler:terminate(Arg,NewHSt),
					{html,Reply} %% todo check content-types and default to JSON
			 ; {ok,NewHSt} ->
				poll(Arg,State=#state{handler_state=NewHSt,handler=Handler})
		end
	after
		?POLL_TIMEOUT ->
			Handler:terminate(Arg,HSt),
			ok
	end.

%%% ------------------------------------------------------------------
%%% Websockets
%%% ------------------------------------------------------------------

%% Websocket state initialisation
init([Arg,Opts]) ->
	Handler = lkup(handler,Opts),
	{ok, HSt} =
	{ok, HSt} = Handler:init(Arg,Opts,Active=true), %% @todo handle {error, Reason} | close / check if Yaws sends 500 errors
	{ok,#state{handler=Handler,handler_state=HSt},?WS_TIMEOUT}.

handle_message(Message) ->
	%% State must be used
	nomod:fail().

handle_message({_Text_or_Bin,Message},State=#state{handler=Handler,handler_state=HSt}) ->
	case Handler:stream(Message,arg,HSt)
		of {reply,R,NewHSt}	-> websocket_rep_state(R,NewHSt,State)
		 ; {ok,NewHSt}     	-> {noreply,State#state{handler_state=NewHSt}}
	end.

handle_info(timeout,State) ->
	{close,timeout,State};

handle_info(Info,State=#state{handler=Handler,handler_state=HSt}) ->
	case Handler:info(Info,arg,HSt)
		of {reply,R,NewHSt}	-> websocket_rep_state(R,NewHSt,State)
		 ; {ok,NewHSt}     	-> {noreply,State#state{handler_state=NewHSt}}
	end.

websocket_rep_state(Reply,NewHSt,State) ->
	R = iolist_to_binary(Reply),
	{reply,{text,R},State#state{handler_state=NewHSt}}.

terminate(_Reason,#state{handler=Handler,handler_state=HSt}) ->
	Handler:terminate(arg,HSt),
	ok.

lkup(K,L) ->
	{K,V} = lists:keyfind(K,1,L),
	V.

lkup(K,L,Def) ->
	case lists:keyfind(K,1,L)
		of {K,V} -> V
		 ; _     -> Def
	end.

get_mode(Arg) ->
	case ?Y:get_header(?Y:arg_headers(Arg), accept)
		of undefined ->
				poll
		 ; Accept ->
				case string:str(Accept, "text/event-stream")
					of 0 -> poll
					 ; _otherwise -> eventsource
				end
	end.





