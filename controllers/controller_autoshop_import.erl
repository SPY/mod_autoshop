-module(controller_autoshop_import).
-author("Ilya Rezvov <i.rezvov@yandex.ru>").
-export([event/2, html/1]).

-include_lib("html_controller.hrl").
-include ("../include/autoshop.hrl").

%% =================================
%% form handlers
%% =================================

event(#submit{message=autoshop_csv_import}, Context) ->
    case z_acl:is_allowed(use, mod_autoshop, Context) of
        true ->
            csv_import(Context);
        false ->
            z_render:growl("You don't have permission to import CSV files.", Context)
    end.

%% =================================
%% zotonic's resource callbacks
%% =================================

html(Context) ->
	Template = z_context:get(template, Context, "autoshop_import.tpl"),
	Variables = [],
	Html = z_template:render(Template, Variables, Context),
	z_context:output(Html, Context).

%% =================================
%% Support functions
%% =================================

csv_import(Context) ->
	ProviderId = extract_provider_id(Context),
	#upload{filename=OriginalFilename, tmpfile=TmpFile} = z_context:get_q_validated("upload_file", Context),

    %% Move temporary file to processing directory
    Target = move_tmp_file_to_processing(OriginalFilename, TmpFile, Context),

	%% Process the file
    process_file(ProviderId, get_csv_props(ProviderId, Context), Target, Context).

move_tmp_file_to_processing(OriginalFilename, TmpFile, Context) ->
	Dir = z_path:files_subdir_ensure("import", Context),
    Target = filename:join([Dir, OriginalFilename]),
    file:delete(Target),
    {ok, _} = file:copy(TmpFile, Target),
    file:delete(TmpFile),
    Target.

process_file(ProvId, Props, Target, Context) ->
	handle_spawn(ProvId, Props, Target, Context),
    z_render:growl(?__("Please hold on while the file is importing. You will get a notification when it is ready.", Context), Context).

handle_spawn(ProvId, Props, Target, Context) ->
	spawn(fun() -> 
		autoshop_import_worker:start(#parser_state{
			filename = Target,
			provider = ProvId,
			props = Props,
			context = z_acl:sudo(z_context:new(Context))
		})
	end).

extract_provider_id(Context) -> 
	z_context:get_q("provider", Context).

get_csv_props(ProvId, Context) ->
	PropsString = binary_to_list(m_rsc:p(ProvId, csv_props, Context)),
	RawProps = string:tokens(PropsString, ", "),
	lists:map(fun list_to_atom/1, RawProps).
