-module(ct_player).
-behaviour(gen_server).

-export([start_link/2,stop/0]).
-export([get_handler/1,is_player/1,get_pid/1,get_name/1,get_max_life_points/1,get_life_points/1]).
-export([go/2, set_room/2,seen/2,unseen/2,entered/4,leave_denied/1]).
-export([init/1,handle_cast/2,handle_call/3,terminate/2,code_change/3,handle_info/2]).
-export([set_client_pid/2, ws_send_test/2]).

-record(player_state,{id,
	my_pid,
	client_pid,
	name,
	max_life_points,
	life_points,
	room,			
	room_exits,
	params=[]}).
%% Accessors
get_handler(Pid) ->
	gen_server:call(Pid,{get_handler}).
is_player(Player) when is_record(Player,player_state) ->
	true;
is_player(_) ->
	bad_arg.
get_pid(#player_state{my_pid=Pid} = _Player) ->
	Pid.
get_name(#player_state{name=Name} = _Player) ->
	Name.
get_max_life_points(#player_state{max_life_points=MaxLifePoints} = _Player) ->
	MaxLifePoints.
get_life_points(#player_state{life_points=LifePoints} = _Player) ->
	LifePoints.

start_link(Name, Params) ->
	[ClientPid|_]=Params, 
    gen_server:start_link(?MODULE,{Name,ClientPid, Params}, []).

set_client_pid(PlayerPid, ClientPid) ->
	%gen_server:call(ct_player:get_pid(Player), {set_client_pid, ClientPid}).
	gen_server:call(PlayerPid, {set_client_pid, ClientPid}).
ws_send_test(Player, Msg) ->
	gen_server:cast(ct_player:get_pid(Player), {ws_send_test, Msg}).
%% Client API
go(Player,Direction) ->
    gen_server:cast(ct_player:get_pid(Player), {go, Direction}).
%% To be called when the user is created
set_room(Player,[X,Y]) ->
    gen_server:cast(ct_player:get_pid(Player), {set_room, [X,Y]}).

%% Events
entered(Player, RoomPid, RoomExits, RoomName) ->
	gen_server:cast(ct_player:get_pid(Player), {entered, RoomPid, RoomExits, RoomName}).
seen(Player,OtherPlayer) ->
	gen_server:cast(ct_player:get_pid(Player), {seen, OtherPlayer}).
unseen(Player,OtherPlayer) ->
	gen_server:cast(ct_player:get_pid(Player), {unseen, OtherPlayer}).
leave_denied(Player) ->
	gen_server:cast(ct_player:get_pid(Player),{leave_denied}).

%% Internal functions
init({Name,ClientPid,Params}) ->
	State=#player_state{id=1,my_pid=self(),name=Name,client_pid=ClientPid, params=Params},
	io:format("ct_player has started (~w)~n", [self()]),
    {ok, State}.
stop() -> gen_server:cast(?MODULE, stop).
%% User Callbacks
handle_cast({go, Direction}, State) ->
	io:format("Going ~p~n", [Direction]),
    ct_room:request_leave(State#player_state.room,Direction,State),
    {noreply, State};
handle_cast({set_room, [X,Y]}, State) ->
	{ok,RoomPid}=ct_room_sup:get_pid([X,Y]),
	ct_room:enter(RoomPid, State, null),
	{noreply, State};
handle_cast({seen, OtherPlayer}, State) ->
	%%Decide wether to attack or not.
	io:format("~s: has been seen by ~s~n",[State#player_state.name,ct_player:get_name(OtherPlayer)]),
	{noreply,State};
handle_cast({unseen, OtherPlayer}, State) ->
	%%The player left the room
	io:format("~s: no longer see ~s~n",[State#player_state.name,ct_player:get_name(OtherPlayer)]),
	{noreply,State};
handle_cast({entered, RoomPid, RoomExits, RoomName}, State) ->
	NewState=State#player_state{room_exits=RoomExits,room=RoomPid},
	io:format("~s is entering into a ~s ~n", [State#player_state.name,RoomName]),
	io:format("Room exits ~p ~n", [[X || {X,_} <- RoomExits]]),
	{noreply, NewState};
handle_cast({leave_denied},State) ->
	io:format("~s has hit with a wall ~n", [State#player_state.name]),
	{noreply,State};
handle_cast(stop, State) -> {stop, normal, State};
handle_cast({ws_send_test, Msg}, State) -> 
	ct_yaws_catacomb_ws_endpoint:say_hi(State#player_state.client_pid, Msg), 
	{noreply, State}.

handle_call({get_handler},_From,State) -> 
	{reply,State,State};
handle_call({set_client_pid, ClientPid}, _From, State) ->
    State2 = State#player_state{client_pid=ClientPid},
	{reply,State2,State2}.


%% System callbacks
terminate(_Reason, State) -> {ok,State}.

code_change(_OldVsn, State, _Extra) -> {ok, State}.
handle_info( _, State) -> {noreply,State}.