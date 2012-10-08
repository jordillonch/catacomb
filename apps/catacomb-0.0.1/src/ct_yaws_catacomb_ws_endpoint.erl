-module(ct_yaws_catacomb_ws_endpoint).
-export([handle_message/1, say_hi/2]).

handle_message({text, Message}) ->
	yaws_api:websocket_send(self(), {text, <<"hi there!">>}),
    {reply, {text, Message}};
handle_message({Type,Data}) ->
	io:format("test ~p~n", [Type]),
	ct_player:set_client_pid(player_test, self()),
	%gen_server:cast(?SERVER,{self(), Data}),
  	noreply.

say_hi(Pid, Msg) ->
    yaws_api:websocket_send(Pid, {text, <<"hi there!">>}).
