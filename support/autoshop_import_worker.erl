-module (autoshop_import_worker).

-export ([start/1]).

-include ("../include/autoshop.hrl").

-define (BUFFER_SIZE, 256*1024).

start(S) ->
	S1 = open_file(S),
	worker(S1).

worker(S) ->
	handle_parse_line(parse_line(S), S).

open_file(S = #parser_state{ filename = File }) ->
	{ ok, HFile } = file:open(File, [read, {read_ahead, ?BUFFER_SIZE}]),
	S#parser_state{ file_handle = HFile }.

close_and_remove(#parser_state{ filename = F, file_handle = HF }) ->
	file:close(HF),
	file:delete(F).

parse_line( #parser_state{ file_handle = File, props = Props } ) ->
	autoshop_csv_parser:parse_line(File, Props).

%%%%%%%%%%%%
% Parsing
%%%%%%%%%%%%

handle_parse_line({ error, eof }, S) ->
	close_and_remove(S),
	print_report(S);
handle_parse_line({ ok, Line }, S) ->
	S1 = import_line(Line, S),
	worker(S1);
handle_parse_line(Any, S = #parser_state{ inserted = Ins, updated = Upd, errors = Err }) ->
	close_and_remove(S),
	io:format("Unexpected result on ~p line: ~p~n", [Ins + Upd + Err, Any]).

%%%%%%%%%%%%
% Import
%%%%%%%%%%%%
import_line(L, S = #parser_state{ provider = ProvId, context = Context, errors = Err }) ->
	try handle_import(autoshop_import:import_row(ProvId, L, Context), S)
	catch _:_ -> S#parser_state{ errors = Err + 1 } end.

handle_import({ inserted, _Id }, S = #parser_state{ inserted = Ins }) ->
	S#parser_state{ inserted = Ins + 1 };
handle_import({ updated, _Id }, S = #parser_state{ updated = Upd }) ->
	S#parser_state{ updated = Upd + 1 };
handle_import(Any, S = #parser_state{errors = Err}) ->
	io:format("Bad import result with ~p~n", [Any]),
	S#parser_state{ errors = Err + 1 }.

print_report(#parser_state{ inserted = Ins, updated = Upd, errors = Err }) ->
	io:format(
		"*****~nCSV Import: Imported ~p row. Inserted ~p rows. Updated ~p rows. Risez ~p errors.~n*****~n", 
		[Ins + Upd + Err, Ins, Upd, Err]
	).