-record(parser_state, {
		filename,
		file_handle = undefined,
		provider,
		props,
		context,
		%% statistic
		stat = undefined
	}).

-record(parser_statistic, {
		inserted = 0,
		updated = 0,
		errors = 0,
		time = 0,
		prev = undefined
	}).