-module(mp3).
-export([read_file/1]).


read_file(File) ->
    Size = filelib:file_size(File),
    {ok, S} = file:open(File, [read, binary, raw]),
    {ok, Id3, End} = id3_tag(S, Size),
    {ok, Data} = file:pread(S, 0, End),
    Title = id3_title(Id3),
    {mp3, Title, Data}.


id3_tag(S, Size) -> 
	io:format("~p~n",[Size]),
	{ok, Data} = file:pread(S, Size-128, 128),
	case Data of
		<<$T, $A, $G, Title:(8*30), Artist:(8*30), 
		Album:(8*30), Year:(8*4), 
		Comment:(8*28), O:8, Track:8, Genre:8>> -> 
			{ok, {id3, [{title, Title},
						{artist, Artist},
						{album, Album},
						{year, Year},
						{comment, Comment},
						{o, O},
						{track, Track},
						{genre, Genre}]}, Size};
		_Other -> {ok, null, Size}
	end.
	
clean(Data) -> 
	io:format("~p~n",[Data]),
	clean(Data, []).
clean(<<0:8, _>>, Ret) -> lists:reverse(Ret); 
clean(<<C:8, Rest/binary>>, Ret) -> 
	clean(Rest, [C|Ret]).

id3_title({id3, [{title, Title} | _]}) -> Title.