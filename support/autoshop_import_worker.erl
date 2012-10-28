-module (autoshop_import_worker).

-export ([start/1]).

-include ("../include/autoshop.hrl").

-define (BUFFER_SIZE, 256*1024).
-define (STAT_PER_TIMES, 1000).

start(S) ->
	S1 = open_file(S),
	worker(S1#parser_state{ stat = init_stat() }).

init_stat() ->
	StartTime = now(),
	#parser_statistic{ 
		time = StartTime, 
		prev = #parser_statistic{ time = StartTime } 
	}.

worker(S) ->
	S1 = dump_statistic(S),
	handle_parse_line(parse_line(S1), S1).

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
handle_parse_line(Any, S = #parser_state{ stat = #parser_statistic{ inserted = Ins, updated = Upd, errors = Err } }) ->
	close_and_remove(S),
	io:format("Unexpected result on ~p line: ~p~n", [Ins + Upd + Err, Any]).

%%%%%%%%%%%%
% Import
%%%%%%%%%%%%
import_line(L, S = #parser_state{ provider = ProvId, context = Context }) ->
	try handle_import(autoshop_import:import_row(ProvId, L, Context), S)
	catch _:_ -> inc_st(errors, S) end.

handle_import({ inserted, _Id }, S) ->
	inc_st(inserted, S);
handle_import({ updated, _Id }, S) ->
	inc_st(updated, S);
handle_import(Any, S) ->
	io:format("Bad import result with ~p~n", [Any]),
	inc_st(errors, S).

print_report(S) ->
	io:format(
		"~n*****~nCSV Import: Imported ~p row. Inserted ~p rows. Updated ~p rows. Risez ~p errors.~n*****~n", 
		[get_st(total, S), get_st(inserted, S), get_st(updated, S), get_st(errors, S)]
	).

dump_statistic(S = #parser_state{ 
			stat = St = #parser_statistic{ 
				inserted = I, 
				updated = U, 
				errors = E
			}
		}) when ((I + U + E) rem ?STAT_PER_TIMES) =:= 0 ->
	Prev = St#parser_statistic.prev,
	NewSt = St#parser_statistic{ time = now(), prev = St },
	%% Diff = Now - St#parser_statistic.prev_time,
	io:format("~nDiff : ~p", [ stat_diff(Prev, NewSt) ]),
	S#parser_state{ stat = NewSt };
dump_statistic(S) ->
	S.

inc_st(updated, S = #parser_state{ stat = St = #parser_statistic{ updated = Upd } }) ->
	S#parser_state{ stat = St#parser_statistic{ updated = Upd + 1 } };
inc_st(inserted, S = #parser_state{ stat = St = #parser_statistic{ inserted = Ins } }) ->
	S#parser_state{ stat = St#parser_statistic{ inserted = Ins + 1 } };
inc_st(errors, S = #parser_state{ stat = St = #parser_statistic{ errors = Err } }) ->
	S#parser_state{ stat = St#parser_statistic{ errors = Err + 1 } }.

get_st(updated, #parser_state{ stat = #parser_statistic{ updated = Upd } }) ->
	Upd;
get_st(inserted, #parser_state{ stat = #parser_statistic{ inserted = Ins } }) ->
	Ins;
get_st(errors, #parser_state{ stat = #parser_statistic{ errors = Err } }) ->
	Err;
get_st(total, #parser_state{ stat = #parser_statistic{ updated = Upd, errors = Err, inserted = Ins } }) ->
	Upd + Ins + Err.

stat_diff(#parser_statistic{ time = T1, inserted = I1, updated = U1, errors = E1},
		#parser_statistic{ time = T2, inserted = I2, updated = U2, errors = E2}) ->
	[
		{ time, timer:now_diff(T2, T1)/1000000 }, 
		{ inserted, I2 - I1 },
		{ updated, U2 - U1 },
		{ errors, E2 - E1}
	].





