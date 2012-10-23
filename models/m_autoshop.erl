-module (m_autoshop).

%% interface functions
-export([
	m_find_value/3,
	m_to_list/2,
	m_value/2
]).

-include_lib("zotonic.hrl").

-define (ITEMS, "autoshop_items").

m_find_value({search, Params}, M, Context) ->
	M#m{ value = {items, search_by_number(proplists:get_value(number, Params), Context) } };

m_find_value(Key, #m{value = { item, Item } }, _Context) ->
	proplists:get_value(Key, Item);

m_find_value(Key, M, Context) when is_list(Key) or is_integer(Key) ->
	M#m{value = { item, get_by_id(Key, Context) } };

m_find_value(Key, M, _Context) ->
	M#m{value = Key}.

%% @doc Transform a m_config value to a list, used for template loops
%% @spec m_to_list(Source, Context) -> List
m_to_list(#m{ value = { items, Ids } }, _Context) ->
	lists:map(fun({Id}) -> Id end, Ids);

m_to_list(Source, _Context) ->
    Source.

%% @doc Transform a model value so that it can be formatted or piped through filters
%% @spec m_value(Source, Context) -> term()
m_value(_Source, _Context) ->
    undefined.

get_by_id(Id, Context) ->
	case z_db:select(?ITEMS, Id, Context) of 
		{ok, R} -> R;
		_ -> undefined
	end.

search_by_number("", Context) ->
	z_db:q("select id from " ++ ?ITEMS ++ " limit 20", [ ], Context);

search_by_number(Num, Context) ->
	z_db:q("select id from " ++ ?ITEMS ++ " where number=$1", [ Num ], Context).