-module(flea_eventsource).

%%% ne pas oublier de citer s.vinoski pour ce module

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
		 terminate/2, code_change/3]).

-record(state, { sock, yaws_pid , arg, handler, handler_state
	}).

init([Arg,Handler,Opts]) ->
	process_flag(trap_exit, true),
	{ok, HSt} = Handler:init(Arg,Opts,Active = true),
	{ok, #state{sock=yaws_api:arg_clisock(Arg),arg=Arg,handler=Handler,handler_state=HSt}}.

handle_call(Request, _From, State) ->
	{reply, {unknonw_request,Request}, State}.

handle_cast(_Msg, State) ->
	{noreply, State}.

handle_info({ok, YawsPid}, State) when is_pid(YawsPid) ->
	{noreply, State#state{yaws_pid=YawsPid}};

handle_info({discard, _YawsPid}, State) ->
	%% nothing to do
	{stop, normal, State};
handle_info({tcp_closed, _}, State) ->
	{stop, normal, State#state{sock=closed}};

handle_info(Message, #state{sock=Socket,arg=Arg,handler=Handler,handler_state=HSt}=State) ->
	case Handler:info(Message,HSt)
		of {reply,Data,NewHSt} ->
			case yaws_sse:send_events(Socket, yaws_sse:data(Data))
				of ok ->             	{noreply, State#state{handler_state=NewHSt}}
				 ; {error, closed} ->	{stop, normal, State#state{handler_state=NewHSt}}
				 ; {error, Reason} ->	{stop, Reason, State#state{handler_state=NewHSt}}
			end
		 ; {ok,NewHSt} ->
			{noreply,State#state{handler_state=NewHSt}}
	end.

terminate(_Reason, #state{sock=Socket,yaws_pid=YawsPid,arg=Arg,handler=Handler,handler_state=HSt}) ->
	Handler:terminate(HSt),
	yaws_api:stream_process_end(Socket, YawsPid),
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.
