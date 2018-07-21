-module(mp3).
-export([removeZeros/2,read_file/1]).

read_file(File) ->
Size = filelib:file_size(File),
{ok, S} = file:open(File, [read, binary, raw]),
{ok, Id3, End} = id3_tag(S, Size),
{ok, Data} = file:pread(S, 0, End),
Title = id3_title(Id3),
{mp3, Title, Data}.

id3_title(na) ->
	"none";
id3_title({id3, [{title, TheTitle}|R]}) ->
	TheTitle.

id3_tag(S, Size) ->
	{ok, BinData} = file:pread(S, Size - 128, 128),
	getId3Tags(BinData, Size).
	
getId3Tags(BinData, Size) ->
	<<Tag:(3*8), R1/binary>> = BinData,
	<<Title:(8*30), R2/binary>> = R1,
	<<Artist:(8*30), R3/binary>> = R2,
	<<Album:(8*30), R4/binary>> = R3,
	<<Year:(8*4), R5/binary>> = R4,
	<<Comment:(8*28), R6/binary>> = R5,
	%<<0:8, R7>> = R6,
	%<<Track:(8), R8>> = R7,
	%<<Genre:(8), R9>> = R8,
	<<_/binary>> = R6,
	TheTag = binary:encode_unsigned(Tag),
	case TheTag of
		<<"TAG">> ->
			TheTitle = removeZeros(binary:encode_unsigned(Title), []),
			TheArtist = removeZeros(binary:encode_unsigned(Artist), []),
			TheAlbum = removeZeros(binary:encode_unsigned(Album), []),
			TheYear = removeZeros(binary:encode_unsigned(Year), []),
			TheComment = removeZeros(binary:encode_unsigned(Comment), []),
			%TheTrack = removeZeros(Track, []),
			%TheGenre = removeZeros(Genre, []),
			{ok, {id3, [{title, TheTitle},{artist, TheArtist}, {album, TheAlbum}, {year, TheYear}, {comment, TheComment}]}, Size};
				_ ->
				{ok, na, Size}
	end.

removeZeros(<<>>, Result) ->
	Result;
removeZeros(<<0, R1/binary>>, Result) ->
	Result;
removeZeros(<<32, R1/binary>>, Result) ->
	Result;
removeZeros(<<A, R1/binary>>, Result) ->
	%<<A, R1/binary>> = Data,
	removeZeros(R1, Result ++ [A]).
	
