-module(autoshop_csv_parser).
-export([ parse_file/2, parse_line/2 ]).

parse_file(Filename, PropsList) ->
	{ ok, HFile } = file:open(Filename, [read]),
	Lines = do_parse(HFile),
	lists:map(fun(Line) -> safe_zip(PropsList, Line) end, Lines).

parse_line(HFile, Props) ->
	case file:read_line(HFile) of 
		eof -> { error, eof };
		{ ok, Line } -> { ok, safe_zip(Props, parse_line(Line)) }
	end.

do_parse(HFile) ->
	do_parse(HFile, []).

do_parse(F, Acc) ->
	case file:read_line(F) of 
		eof -> Acc;
		{ ok, Line } -> do_parse(F, [ parse_line(Line) | Acc ])
	end.

parse_line(Line) ->
	%% cut \n from end string
	Line1 = string:substr(Line, 1, string:len(Line) - 1 ),
	parse_columns(Line1).

safe_zip([], _) ->
	[];
safe_zip(_, []) ->
	[];
safe_zip([ H1 | T1 ], [ H2 | T2 ]) ->
	[ { H1, H2 } | safe_zip(T1, T2) ].

parse_columns(Line) ->
	parse_columns(Line, []).

parse_columns([], Acc) ->
	lists:reverse(Acc);
parse_columns(Line, Acc) ->
	{ Col, Rest } = get_column(Line),
	parse_columns(Rest, [ Col | Acc ]).

get_column(Line) ->
	get_column(Line, [], read).

%% real version with escaping
get_column([$; | T], Acc, read) ->
	get_column(T, Acc, stop);
get_column([$\n , $\r | T], Acc, read) ->
	get_column(T, Acc, stop);
get_column([$\n | T], Acc, read) ->
	get_column(T, Acc, stop);
get_column([$" | T], Acc, read) ->
	get_column(T, Acc, quoted);
get_column([C | T], Acc, read) ->
	get_column(T, [C | Acc], read);
get_column([$\\, $" | T], Acc, quoted) ->
	get_column(T, [$" | Acc], quoted);
get_column([$" | T], Acc, quoted) ->
	get_column(T, Acc, read);
get_column([C | T], Acc, quoted) ->
	get_column(T, [C | Acc], quoted);
get_column(R, Acc, stop) ->
	{ lists:reverse(Acc), R};
get_column([], Acc, _State) ->
	get_column([], Acc, stop).



