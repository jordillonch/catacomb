-module(ct_session_sup).
-behaviour(supervisor).
-export([start_link/0]).
-export([init/1,get_new_session/0]).

-include("../include/catacomb.hrl").

start_link() ->
    supervisor:start_link({global, ?MODULE}, ?MODULE, []).

init([]) ->
	%% Declare a simple_one_for_one supervisor as this king of supervisor is ideal for workers.
	%% All children must be started dynamically and are copies of the same module.
	WorkerSpecs = {{global,ct_session}, {ct_session, start_link, []}, temporary, 2000, worker,[ct_session]},
	StartSpecs = {{simple_one_for_one, 0, 1},[WorkerSpecs]},
    {ok, StartSpecs}.

%% Starts an individual player
get_new_session() ->
	io:format("starting new session...~n"),
    {ok,Pid}=supervisor:start_child({global,?MODULE}, []),
    #ct_session{my_pid=Pid, client_pid=self()}.
 