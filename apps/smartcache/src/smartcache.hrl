-define(MANAGER_KEY_TAB, smartcache_prefetch_manager_keys).
-type seconds() :: integer().
-type key() :: iodata().
-type value() :: iodata().


-define(TRACE(Val), lager:debug("Trace: ~p", [Val]), Val).
