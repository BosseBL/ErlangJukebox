-module(test).
-export([bench/2]).

bench(Host, Port) ->
%global:register_name(dinnertable, self()),
Start = now(),
run(20, Host, Port, 20),
%print(),
Finish = now(),
timer:now_diff(Finish, Start).

printline(Text) ->
	global:whereis_name(dinnertable) ! Text.

quitprinter() ->
  	global:whereis_name(dinnertable) ! quit.

print() ->
	io:format("hej"),
	receive
		quit -> ok;
		Message -> io:format(Message ++ "\n"),
		print()
	end.

%% run(0, Host, Port) ->
%% ok;
%% run(N, Host, Port) ->
%% spawnRequest(Host, Port, 4),
%% %request(Host, Port),
%% run(N-1, Host, Port).

run(0, Host, Port, P) ->
	wait(P);
run(N, Host, Port, P) ->
	S = self(),
	spawn(fun() -> request(Host, Port, S) end),
	run(N-1,Host, Port, P).

wait(1) ->
  done;
wait(N) ->
	receive
		done ->
			%io:format("~5w~n", [N]),
			wait(N-1)
	end.

request(Host, Port, Pid) ->
Opt = [list, {active, false}, {reuseaddr, true}],
{ok, Server} = gen_tcp:connect(Host, Port, Opt),
gen_tcp:send(Server, http:get("foo")),
{ok, _Reply} = gen_tcp:recv(Server, 0),
gen_tcp:close(Server),
Pid ! done.

%% spawnRequest(_,_,1) ->
%% 	ok;
%% spawnRequest(Host, Port, N) ->
%% 	spawn(fun() -> request(Host, Port) end),
%% 	spawnRequest(Host, Port, N-1).
%% bench(Host, Port) ->
%% Start = now(),
%% run(100, Host, Port),
%% Finish = now(),
%% timer:now_diff(Finish, Start).
%% 
%% run(0, Host, Port) ->
%% ok;
%% run(N, Host, Port) ->
%% request(Host, Port),
%% run(N-1, Host, Port).
%% request(Host, Port) ->
%% Opt = [list, {active, false}, {reuseaddr, true}],
%% {ok, Server} = gen_tcp:connect(Host, Port, Opt),
%% gen_tcp:send(Server, http:get("foo")),
%% Recv = gen_tcp:recv(Server, 0),
%% case Recv of
%% {ok, _} ->
%% ok;
%% {error, Error} ->
%% io:format("test: error: ~w~n", [Error])
%% end.