-module (autoshop_import).
-export ([import_row/3]).

%% =================================
%% Support functions
%% =================================

-define (ITEMS, "autoshop_items").

import_row(ProvId, Row, Context) ->
	Ps = filter_props(make_props(ProvId, Row, Context)),
	handle_exist_check(get_item_id(Ps, Context), Ps, Context).

handle_exist_check({ok, Id}, Ps, Context) ->
	io:format("Already exist: ~p~n", [Id]),
	z_db:update(?ITEMS, Id, Ps, Context);
handle_exist_check({error, not_found}, Ps, Context) ->
	z_db:insert(?ITEMS, Ps, Context);
handle_exist_check(_, _Ps, _Context) ->
	io:format("Unexpected error during import~n", []),
	undefined.

make_props(ProvId, Props, Context) ->
	Key = z_convert:to_atom(m_rsc:p(ProvId, key_prop, number, Context)), 
	lists:map(
		fun(S = {P, _V}) -> {P, parse_prop(S)} end,
		[ { provider, ProvId }, { keyprop, proplists:get_value(Key, Props) } | Props ]
	).

filter_props(Props) ->
	lists:filter(fun({P, _V}) -> is_item_prop(P) end, Props).

is_item_prop(provider) -> true;
is_item_prop(keyprop) -> true;
is_item_prop(number) -> true;
is_item_prop(price) -> true;
is_item_prop(title) -> true;
is_item_prop(delivery_time) -> true;
is_item_prop(amount) -> true;
is_item_prop(_) -> false.

parse_prop({provider, V}) -> z_convert:to_integer(V);
parse_prop({keyprop, V}) -> z_convert:to_list(V);
parse_prop({price, V}) -> z_convert:to_float(V);
parse_prop({number, V}) -> normalize_number(V);
parse_prop({_P, V}) -> V.

get_item_id(Props, Context) ->
	R = z_db:equery("select id from " ++ ?ITEMS ++ " where provider=$1 and keyprop=$2 and number=$3", [
			proplists:get_value(K, Props) || K <- [provider, keyprop, number]
		], Context),
	case R of 
		{ok, _C, [{ Id }]} -> {ok, Id};
		_ -> {error, not_found}
	end.

normalize_number(Num) when is_binary(Num) ->
    normalize_number(binary_to_list(Num));
normalize_number(Num) ->
    normalize_number(Num, []).

normalize_number([], Acc) ->
    lists:reverse(Acc);
normalize_number([ $\s | T], Acc) ->
    normalize_number(T, Acc);
normalize_number([ $- | T], Acc) ->
    normalize_number(T, Acc);
normalize_number([ $' | T], Acc) ->
    normalize_number(T, [ $\\, $' | Acc]);
normalize_number([C | T], Acc) ->
    normalize_number(T, [ C | Acc ]).
