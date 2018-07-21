-module(jukebox).
%-export([start/1, init/1, segments/1]).
-compile(export_all).
-define(Opt,[binary,
{packet, 0},
{reuseaddr, true},
{active, true},
{nodelay, true}]).
-define(Port, 8080).
-define(Header, [{"icy-name", "ID1019"},
{"icy-genre", "Groove"},
{"icy-notice1", "Our own Jukebox"}]).

start(File) ->
register(jukebox, spawn(fun() -> init(File) end)).
%spawn(fun() -> init(File) end).

terminate() ->
	exit(whereis(jukebox), "Time to die").

init(File) ->
{mp3, Title, Data} = mp3:read_file(File),
Headers = icy:encode_meta([{title, Title}]),
Segments = icy:segments(Data),
{ok, Listen} = gen_tcp:listen(?Port, ?Opt),
server(Headers, Segments, Listen).

spawnSender(Socket, Header, Segments) ->
	inet:setopts(Socket, ?Opt),
	io:format("server: connect~n",[]),
	case read_request(Socket) of
	{ok, Request, _} ->
	io:format("server: received request ~p~n", [Request]),
	gen_tcp:send(Socket, icy:encode_response(?Header)),
	loop(Header, Segments, Socket);
	{error, Error} ->
	io:format("server: ~s~n", [Error])
	end.

server(Header, Segments, Listen) ->
{ok, Socket} = gen_tcp:accept(Listen),
spawn(fun() -> server(Header, Segments, Listen) end),
spawnSender(Socket, Header, Segments).
%server(Header, Segments, Listen).
%% io:format("server: connect~n",[]),
%% case read_request(Socket) of
%% {ok, Request, _} ->
%% io:format("server: received request ~p~n", [Request]),
%% gen_tcp:send(Socket, icy:encode_response(?Header)),
%% loop(Header, Segments, Socket);
%% {error, Error} ->
%% io:format("server: ~s~n", [Error])
%% end.

read_request(Socket) ->
reader(fun(More)-> icy:parse_request(More) end, Socket).

reader(Parser, Socket) ->
receive
{tcp, Socket, More} ->
case Parser(More) of
{ok, Parsed,_, _,_, Rest} ->
{ok, Parsed, Rest};
{more, Cont} ->
reader(fun(More) -> Cont(More) end, Socket);
{error, Error} ->
{error, Error}
end;
{tcp_closed, Socket} ->
{error, "server closed connection"};
stop ->
	ok
end.

%% reader(Cont, Socket) ->
%% receive
%% {tcp, Socket, More} ->
%% case ... of
%% {ok, Parsed, Rest} ->
%% ...;
%% {more, Fun} ->
%% ...;
%% {error, Error} ->
%% ...
%% end;
%% {tcp_closed, Socket} ->
%% ...
%% after ?TimeOut ->
%% ...
%% end.

loop(_, [], _) ->
ok;
loop(Header, [{seg, Segment}|Rest], Socket) ->
gen_tcp:send(Socket, Segment),
%io:format("server: sent segment ~n", []),
gen_tcp:send(Socket, Header),
%io:format("server: sent header ~n", []),
loop(Header, Rest, Socket).
