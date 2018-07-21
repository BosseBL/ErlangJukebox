-module(rudy).
-export([start/2, stop/0]).
%-export([init/1]).

%start(Port, 0) ->
%	ok;
start(Port, N) ->
	register(rudy, spawn(fun() -> init(Port, N) end)).
	%spawn(fun() -> init(Port, N) end).
	%init(Port, N).
	%start(Port, N-1).

content() ->
	{ok, Data} = file:read_file("./index.html"),
	data.
stop() ->
	exit(whereis(rudy), "time to die").

init(Port, N) ->
	Opt = [list, {active, false}, {reuseaddr, true}],
	case gen_tcp:listen(Port, Opt) of
		{ok, Listen} ->
			spawnHandler(Listen, N),
			%handler(Listen),
			%gen_tcp:close(Listen),
			receive
				ok -> ok
			end;
		{error, Error} ->
			error
	end.

spawnHandler(Listen, 0) ->
	ok;
spawnHandler(Listen, N) ->
	spawn(fun() -> handler(Listen) end),
	spawnHandler(Listen, N-1).

handler(Listen) ->
	case gen_tcp:accept(Listen) of
		{ok, Client} ->
			request(Client),
			handler(Listen);
		{error, Error} ->
			error
	end,
	handler(Listen).


request(Client) ->
	Recv = gen_tcp:recv(Client, 0),
	case Recv of
		{ok, Str} ->
			Request = http:parse_request(Str),
			Response = reply(Request),
			gen_tcp:send(Client, Response);
			%file:sendfile("hej.html", Client);
		{error, Error} ->
			io:format("rudy: error: ~w~n", [Error])
	end,
gen_tcp:close(Client).

reply({{get, [H|T], _}, _, _}) ->
	timer:sleep(40),
http:ok(content()).