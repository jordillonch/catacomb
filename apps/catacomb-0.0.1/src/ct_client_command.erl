-module(ct_client_command).
-export([execute/2]).

execute(Cmd,State) -> %% State contains State data relevant to this module
	try
	Command = rfc4627:get_field(Cmd,"command",<<>>),
	io:format("Command: ~p~n", [Command]),
	Result = case Command of
		<<"login">> ->
			User=rfc4627:get_field(Cmd,"user",<<>>),
			Password=rfc4627:get_field(Cmd,"password",<<>>),
			io:format("Log in: User: ~p Password: ~p ~n",[User,Password]),
			%% if ! Status#status.session_pid   %%In case a login fails we can retry
			%%  SessionId = ct_session_sup:get_new_session_pid(),
			%%  ct_session:login(Status#status.session_pid,Login,Password),
			%%  NewStatus=Status#status.session_pid=SessionPid, 
			Session = ct_session_sup:get_new_session(),
			io:format("Session ~p~n", [Session]),
			ct_session:login(Session, User, Password),
			{ok, []};
		<<"get_character_list">> ->
			%% CharacterList = ct_session:get_character_list(Status#status.session_pid),
			{ok,[]};
		<<"new_character">>->
			%% NewCharacter = ct_session:new_character(Status#status.session_pid,...),
			{ok,[]};
		<<"load_character">>->
			%% PlayerPid= ct_session:load_character(Status#status.session_pid,CharacterId)
			%% NewStatus=Status#status.player_pid=PlayerPid,  %% Wakes up a character frozen
			{ok,[]};
		<<"start_game">>->
			%% Unfreeze the character
			%% session change state
			{ok,[]};
		<<"exit_game">>->
			%% tell the session to kill everyone and get to idle state.
			%% by a call
			{ok,[]};
		<<"logout">>->
			%% tell the session to die
			%% suicide ourselves
			{ok,[]};
		<<"go">>->
			%%ct_player:go(Status#status.player_pid,Direction)
			{ok,[]};
		<<"catch">>->
			%%ct_player:catch(Status#status.player_pid,ObjectId)
			{ok,[]};
		<<"drop">>->
			%%ct_player:drop(Status#status.player_pid,ObjectId)
			{ok,[]};
		<<"get_inventory">>->
			%%ct_player:get_inventory(Status#status.player_pid)
			{ok,[]};
		<<"info">> ->
			%%ct_player:info(Status#status.player_pid)
			{ok,[]};
		<<"hit">>->
			%%ct_player:hit(Status#status.player_pid,PlayerPid)
			{ok,[]}
		%% Chat commands to be added
		end,
	{Result}
	catch
		What:Why ->
	    Trace=erlang:get_stacktrace(),
	    error_logger:error_msg("ct_client_command: ~p ~p ~p.\n", [What,Why,Trace]),
	    {stop, Why, State}
  end.