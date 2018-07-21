-module(jukebox_server).

-export([start/1]).
-define(Opt,[binary,
            {packet, 0},
            {reuseaddr, true},
            {active, true},
            {nodelay, true}]).
-define(Port, 8080).
-define(Header, [{"icy-name", "ID1019"},
                 {"icy-genre", "Groove"},
                 {"icy-notice1", "Our own Jukebox"}]).
-define(TimeOut, 10000).

                 



start(File) ->
    spawn(fun() -> init(File) end).
init(File) ->
    {mp3, Title, Data} = mp3:read_file(File),
    io:format("~p~n", [Title]),
    Header = icy:encode_meta([{"title", "title"}]),
    Segments = icy:segments(Data),
    {ok, Listen} = gen_tcp:listen(?Port, ?Opt),
    server(Header, Segments, Listen).
    
    
server(Header, Segments, Listen) ->
    {ok, Client} = gen_tcp:accept(Listen),
    io:format("server: connect~n",[]),
    case read_request(Client) of
        {ok, Request, Resource, Version, Headers, Body} ->
            io:format("server: received request ~p~n", [Request]),
            gen_tcp:send(Client, icy:encode_response(?Header)),
            spawn_link(fun() -> loop(Header, Segments, Client) end),
            server(Header, Segments, Listen);
        {error, Error} ->
            io:format("server: ~s~n", [Error])
end.


read_request(Socket) -> reader(fun(More) -> icy:parse_request(<<More/binary>>) end, Socket).
reader(Cont, Socket) -> 
	receive 
		{tcp, Socket, Segment} -> 
			case Cont(Segment) of
				{ok, Method, Resource, Version, Headers, Body} -> {ok, Method, Resource, Version, Headers, Body};
				{more, Fun} -> reader(Fun, Socket);
				{error, Error} -> {error, Error}
			end;
    	{tcp_closed, Socket} -> {error, "socket closed"};
		stop -> ok
	end.


loop(_, [], _) ->
	ok;
loop(Header, [{seg, Segment}|Rest], Socket) ->
	gen_tcp:send(Socket, Segment),
	%io:format("server: sent segment ~n", []),
	gen_tcp:send(Socket, Header),
	%io:format("server: sent header ~n", []),
	loop(Header, Rest, Socket).



    
    