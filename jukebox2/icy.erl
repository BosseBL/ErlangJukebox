-module(icy).
%-export([headers/2, parse_request/1, encode_header/1, padding/1, encode_meta/1]).
-compile(export_all).

-define(Chunk, 4096).

headers(<<>>, _) ->
	more;
headers(<<13, 10, Body/binary>>, L) ->
  {ok, L, Body};
headers(A, B) ->
  {Head, Rest} = header(A, []),
  headers(Rest, [Head|B]).

header(<<13,10, Rest/binary>>, R) ->
	{R, Rest};
header(<<A, R/binary>>, R1) ->
  header(R, R1 ++ [A]).

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

parse_request(Segment) ->
case request_line(Segment) of
{ok, Method, Resource, Version, R1} ->
case headers(R1, []) of
{ok, Headers, Body} ->
{ok, Method, Resource, Version, Headers, Body};
more ->
{more, fun(More) -> parse_request(<<Segment/binary, More/binary>>) end}
end;
{ok, Req, _} ->
{error, "invalid request: " ++ Req};
more ->
{more, fun(More) -> parse_request(<<Segment/binary, More/binary>>) end}
end.


segments(<<Chunk:?Chunk/binary, Rest/binary>>) ->
[{seg, Chunk}| segments(Rest)];
segments(_) ->
[].

encode_meta(Headers) ->
Meta = encode_headers(Headers),
{K, Padded} = padding(Meta),
<<K/integer, Padded/binary>>.

encode_headers([]) ->
	[];
encode_headers([{title, Value}|R]) ->
	"StreamTitle='" ++ Value ++ "';" ++ encode_headers(R).

int_ceil(X) ->
    T = trunc(X),
    case (X - T) of
        Neg when Neg < 0 -> T;
        Pos when Pos > 0 -> T + 1;
        _ -> T
    end.

padding(Meta) ->
	Size = length(Meta),
	NrofSixteen = (int_ceil(Size/16)),
	TotalSize = NrofSixteen * 16,
	PaddingSize = TotalSize - Size,
	Out = paddingAddBlanks(Meta, PaddingSize),
	{NrofSixteen, Out}.

%% paddingAddBlanks(Meta, 0) ->
%% 	Meta;
%% paddingAddBlanks(Meta, N) ->
%% 	paddingAddBlanks(<<Meta, 0>>, N -1).

paddingAddBlanks(Meta, N) ->
	AddSize = 8*N,
	BinaryMeta = list_to_binary(Meta),
	Test = <<0:AddSize>>,
	<<BinaryMeta/binary, Test/binary>>.

reader(Parser, Socket) ->
receive
{tcp, Socket, More} ->
case Parser(More) of
{ok, Parsed, Rest} ->
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

decode_resource(Bin) ->
	{Resource, Rest} = decode_resourcen(Bin, []),
	{ok, Resource, Rest}.

decode_resourcen(<<32, R1/binary>>, Result) ->
	{Result, R1};
decode_resourcen(<<A, R1/binary>>, Result) ->
	decode_resourcen(R1, Result ++ [A]).




request_line(Segment) when size(Segment) < 4 ->
more;
request_line(<<$G, $E, $T, 32, R1/binary>>) ->
case decode_resource(R1) of
{ok, Resource, R2} ->
case decode_version(R2) of
{ok, Version, R3} ->
{ok, get, Resource, Version, R3};
more ->
more;
Other ->
{error, "strange version: " ++ Other}
end;
more ->
more;
error ->
{error, "failed to parse resource"}
end;
request_line(Line) ->
{error, "not a get request: " ++ Line}.

decode_version(Segment) when size(Segment) < 10 ->
more;
decode_version(<<$H, $T, $T, $P, $/, $1, $., $0, 13,10, Rest/binary>>) ->
{ok, v10, Rest};
decode_version(<<$H, $T, $T, $P, $/, $1, $., $1, 13, 10, Rest/binary>>) ->
{ok, v11, Rest}.
