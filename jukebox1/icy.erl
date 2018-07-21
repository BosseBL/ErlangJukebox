-module(icy).
%-export([parse_request/1, encode_response/1, segments/1, encode_meta/1]).
-compile(export_all).
-define(Chunk, 4096).

    
%% kapar upp en http request i hanterbara bitar. exempel på request följer:
%%	GET / HTTP/1.0<cr><lf>
%%	Host: mp3-vr-128.smgradio.com<cr><lf>
%%	User-Agent: Casty<cr><lf>
%%	Icy-MetaData: 1<cr><lf>
%% 	<cr><lf>

parse_request(Segment) ->
	io:format("in parse_request", []),
	case request_line(Segment) of
		{ok, Method, Resource, Version, R1} ->
			case headers(R1, []) of
				{ok, Headers, Body} -> {ok, Method, Resource, Version, Headers, Body};
				more -> {more, fun(More) -> parse_request(<<Segment/binary, More/binary>>) end}
			end;
			
		{ok, Req, _} -> {error, "invalid request: " ++ Req};
			
		more -> {more, fun(More) -> parse_request(<<Segment/binary, More/binary>>) end}
	end.

request_line(Segment) when size(Segment) < 4 -> more;
request_line(<<$G, $E, $T, 32, R1/binary>>) ->
	case decode_resource(R1) of
		{ok, Resource, R2} ->
			case decode_version(R2) of
				{ok, Version, R3} -> {ok, get, Resource, Version, R3};
				more -> more;
				Other -> {error, "strange version: " ++ Other}
			end;
			
		more -> more;
		
        error -> {error, "failed to parse resource"}
    end;
request_line(Line) -> {error, "not a get request: " ++ Line}.
    

decode_resource(Segment) -> decode_resource(Segment, []).
decode_resource(Segment, _) when size(Segment) < 2 -> more;
decode_resource(<<32, Rest/binary>>, Resource) ->  {ok, lists:reverse(Resource), Rest};
decode_resource(<<Next, Rest/binary>>, Resource) ->	decode_resource(Rest, [Next|Resource]).
	    

decode_version(Segment) when size(Segment) <  10 -> more;
decode_version(<<$H, $T, $T, $P, $/, $1, $., $0, $\r, $\n,  Rest/binary>>) -> {ok, v10, Rest};
decode_version(<<$H, $T, $T, $P, $/, $1, $., $1, $\r, $\n, Rest/binary>>) -> {ok, v11, Rest}.


%% gräver fram alla headers av typen "key: value" och sparar som en tupel 
headers(<<$\r, $\n, Rest/binary>>, Headers) -> {ok, lists:reverse(Headers), Rest};
headers(Segment, Headers) -> headers([], Segment, Headers).
headers(_, Segment, _) when size(Segment) < 2 -> more;
headers(Header, <<$\r, $\n, Rest/binary>>, Headers) -> 
	headers(Rest, [headerToTuple(Header)|Headers]);
headers(Header, <<H:8, Rest/binary>>, Headers) -> 
	headers(Header ++ [H], Rest, Headers).

headerToTuple(Header) -> headerToTuple(Header, []).
headerToTuple([H, $:,32|T], Acc) -> {lists:reverse([H|Acc]), T};
headerToTuple([H|T], Acc) -> headerToTuple(T, [H|Acc]).

%% kodar en HTTP respons. 
encode_response(Header) -> 
	Status = "ICY 200 OK\r\n",
    MetaInt = "icy-metaint: " ++ integer_to_list(?Chunk) ++ "\r\n",
    Reply = Status ++ MetaInt ++ encode_header(Header),
    list_to_binary(Reply).
    
encode_header([]) ->
	"\r\n";
encode_header([{Name, Value}|Rest]) ->
  Head = Name ++ ": " ++ Value ++ "\r\n",
  [Head|encode_header(Rest)].
  
  
%% delar upp mp3 i chunks
segments(<<Chunk:?Chunk/binary, Rest/binary>>) -> [{seg, Chunk}| segments(Rest)];
segments(_) -> [].

encode_meta(Headers) -> encode_meta(Headers, []).
encode_meta([], EncodedHeaders) -> 
	Length = length(EncodedHeaders),
	case (Length rem 16) == 0 of 
		true -> Multiples = Length div 16;
		false -> Multiples = Length div 16 + 1
	end,
	Zeroes = Multiples*16 - Length,
	Code = list_to_binary(EncodedHeaders),
	<<Multiples:8, Code/binary, 0:(8*Zeroes)>>;
encode_meta([{Key, Value}|T], EncodedHeaders) -> encode_meta(T, EncodedHeaders ++ Key ++ "='" ++ Value ++ "';").