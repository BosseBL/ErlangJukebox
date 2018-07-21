-module(radio_server).

-export([start/0]).
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
-define(SongList, ["imogen.mp3", "Anyday.mp3", "Blood in the Boardroom.mp3", "Used to You.mp3", "Willing to Fight.mp3"]).

                 

start() ->
    {ok, Listen} = gen_tcp:listen(?Port, ?Opt),
	PL = spawn_link(fun() -> player(1, ?SongList) end),
	Ser = spawn_link(fun() -> superServer([],[], PL) end),
	CA = spawn_link(fun() -> clientAgent(Listen, Ser) end),
    {PL, Ser, CA}.

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


clientAgent(Listen, Server) -> 
	{ok, Client} = gen_tcp:accept(Listen),
	case read_request(Client) of
        {ok, Request, Resource, Version, Headers, Body} ->
            gen_tcp:send(Client, icy:encode_response(?Header)),
            Server ! {client, Client},
            clientAgent(Listen, Server);
        {error, Error} ->
            io:format("server: ~s~n", [Error])
    end.
	

player(CurrentSong, SongList) -> 
	receive 
		{serverPID, Server} -> player(Server, CurrentSong, SongList)
	end.
player(Server, CurrentSong, SongList) -> 
	receive
		newSong -> 
			case length(SongList < CurrentSong) of
				true -> NewCurrentSong = CurrentSong + 1;
				false -> NewCurrentSong = 1
			end,
			File = lists:nth(NewCurrentSong, SongList),
    		{mp3, Title, Data} = mp3:read_file(File),
    		Header = icy:encode_meta([{"title", Title}]),
    		Segments = icy:segments(Data),
    		Server ! {newSong, Segments, Header},
    		player(Server, NewCurrentSong, SongList);
    		
    	{setSong, Filename} -> 
    		{mp3, Title, Data} = mp3:read_file(Filename),
    		Header = icy:encode_meta([{"title", Title}]),
    		Segments = icy:segments(Data),
    		Server ! {newSong, Segments, Header},
    		player(Server, CurrentSong, SongList);
    		
		stop -> Server ! stop
	end.


superServer(Clients, Header, Player) -> 
	Player ! self(),
	superServer(Clients, [], Header, Player).
superServer(Clients, [], _, Player) -> 
	Player ! newSong,
	receive
		{newSong, NewSegments, NewHeader} -> superServer(Clients, NewSegments, NewHeader, Player)
	end;
superServer(Clients, [Segment|Segments], Header, Player) ->
	sendSegment(Clients, Segment, Header), 
	receive
		{newSong, NewSegments, NewHeader} -> superServer(Clients, NewSegments, NewHeader, Player);
		{client, Client} -> superServer([Client|Clients], Segments, Header, Player);
		stop -> closeClients(Clients)
	after 100 -> superServer(Clients, Segments, Header, Player)
	end.
sendSegment([], _, _) -> ok;
sendSegment([Client|Clients], Segment, Header) -> 
	gen_tcp:send(Client, Segment),
	gen_tcp:send(Client, Header),
	sendSegment(Clients, Segment, Header).
closeClients([]) -> ok;
closeClients([Client|Clients]) -> 
	gen_tcp:close(Client),
	closeClients(Clients).



    
    