-record(parser_state, {
		filename,
		file_handle = undefined,
		provider,
		props,
		context,
		inserted = 0,
		updated = 0,
		errors = 0
	}).