-module(clock).
%% clock API.
-export([start/0]).
%% Yaws appmod API
-export([out/1]).
%% Flea API
-export([init/3,info/2,stream/2,terminate/1]).
-define(PERIOD, 1000).

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
		  {opaque,[{"vdir","/js/ /home/niahoo/src/flea/priv"}]}
		],
	GL =
		[ {cache_refresh_secs,1},
		  {logdir, "log"}
		],
	Docroot = PrivDir(?MODULE),
	yaws:start_embedded(Docroot,SL,GL).

%%% -----------------------------------------------------------------
%%% Yaws appmod callbacks
%%% -----------------------------------------------------------------

out(A) ->
	flea:out(A,[{handler,?MODULE}]).

%%% -----------------------------------------------------------------
%%% Flea callbacks
%%% -----------------------------------------------------------------

init(Arg,_,Active) ->
	io:format("/~s init, active=~p in ~p~n",[yaws_api:arg_appmoddata(Arg),Active,self()]),
	TRef = erlang:send_after(?PERIOD, self(), refresh),
	{ok, TRef}.

info(refresh,State) ->
	TRef = erlang:send_after(?PERIOD, self(), refresh),
	DateTime = httpd_util:rfc1123_date(erlang:localtime()),
	io:format("clock refresh timeout: ~s~n", [DateTime]),
	{reply, DateTime, TRef};

info(Message,State)  ->
	io:format("info received in handler ~p~n", [Message]),
	{ok, State}.

stream(<<"ping: ", Name/binary>>, State) ->
	io:format("ping ~p received~n", [Name]),
	{reply, <<"pong">>, State};

stream(Data, State) ->
	io:format("stream received ~p~n", [Data]),
	{ok, State}.

terminate(TRef) ->
	io:format("flea terminate~n"),
	erlang:cancel_timer(TRef),
	ok.
