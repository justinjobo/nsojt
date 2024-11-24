%%%-------------------------------------------------------------------
%%% @copyright 2006 Tail-F Systems AB
%%% @version {$Id$}
%%% @doc A blocking Erlang interface equivalent to the CDB C-API
%%%-------------------------------------------------------------------

-record(cdb_session, { socket, namespace }).

-record(compaction_info, {
          fsize_previous = 0 :: non_neg_integer(),
          fsize_current = 0 :: non_neg_integer(),
          last_time = 0 :: non_neg_integer(),
          ntrans = 0 :: non_neg_integer()
         }).

