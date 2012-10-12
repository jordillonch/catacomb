-module(ct_session).
-behaviour(gen_server).

-export([start_link/2,stop/0]).
-export([login/3, get_character_list/1]).
-export([init/1,handle_cast/2,handle_call/3,terminate/2,code_change/3,handle_info/2]).

-include("../include/catacomb.hrl").

%% Client API
login(Session, User, Password) ->
	io:format("api login~n"),
	gen_server:cast(Session#ct_session.my_pid, {login, User, Password}),
	{ok}.

get_character_list(_Session) ->
	[{1, player_1},
	 {2, player_2}].

start_link(_Name, _Params) ->
    gen_server:start_link(?MODULE, [], []).


%% Internal functions
init([]) ->
	State = #ct_session{},
	io:format("ct_session has started (~w)~n", [self()]),
    {ok, State}.
stop() -> gen_server:cast(?MODULE, stop).

%% User Callbacks
handle_cast({login, User, _Password}, State) ->
	io:format("Login ~p~n", [User]),
    % TODO
    NewState = State#ct_session{auth=true, user=User},
    % cast to client (ex. websocket)
    gen_server:cast(State#ct_session.client_pid, {login, ok}),
    {noreply, NewState};
handle_cast(stop, State) -> {stop, normal, State}.

handle_call(_Msg, _From, State) -> 
	{reply,State,State}.

%% System callbacks
terminate(_Reason, State) -> {ok,State}.

code_change(_OldVsn, State, _Extra) -> {ok, State}.
handle_info( _, State) -> {noreply,State}.
