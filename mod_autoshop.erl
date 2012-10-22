-module(mod_autoshop).
-author("Ilya Rezvov <i.rezvov@yandex.ru>").

-mod_title("Autoshop").
-mod_description("Store of auto parts").
-mod_depends([mod_signal]).
-mod_prio(100).

-include_lib("include/zotonic.hrl").
-include_lib("modules/mod_admin/include/admin_menu.hrl").

-export([
         observe_admin_menu/3
        ]).

%% zotonic observe callbacks
observe_admin_menu(admin_menu, Acc, Context) ->
    [
     #menu_item{id=admin_autoshop,
                parent=admin_modules,
                label=?__("Autoshop", Context),
                url={admin_autoshop},
                visiblecheck={acl, use, ?MODULE}}   
     |Acc].