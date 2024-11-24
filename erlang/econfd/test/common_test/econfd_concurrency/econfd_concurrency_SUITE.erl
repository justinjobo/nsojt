%%% ----------------------------------------------------------------------------
%%% @doc Erlang Common Test of econfd's concurrency functionality
%%%
%%% ----------------------------------------------------------------------------
-module(econfd_concurrency_SUITE).

-compile(export_all).

-include_lib("common_test/include/ct.hrl").
-include_lib("ct_support/include/cdb_testlib.hrl").
-include_lib("eunit/include/eunit.hrl").

-include("econfd/include/econfd_errors.hrl").
-include("econfd/include/econfd.hrl").

-include("capi/src/proto.hrl").

%% -----------------------------------------------------------------------------
%% COMMON TEST CALLBACK FUNCTIONS
%% -----------------------------------------------------------------------------

-define(b2t, binary_to_term).

all() ->
    [
     close_trans_kills_ths,
     worker_dies_ths_die,
     th_dies_worker_lives,
     daemon_dies_everything_dies,
     daemon_stop_everything_dies,
     abort
    ].

suite() ->
    [{timetrap, {minutes, 1}}].

init_per_suite(Config) ->
    ok = cdb_testlib:confd_start(),
    Config.

end_per_suite(_Config) ->
    ok.

%% -----------------------------------------------------------------------------
%% TEST CASES
%% -----------------------------------------------------------------------------

close_trans_kills_ths() ->
    cdb_testlib:desc("Test that when CONFD_PROTO_CLOSE_TRANS is called,"
                     " ths do not linger around.").
close_trans_kills_ths(_Config) ->
    {ok, _DaemonPid, Worker, TransactionHandlerPids} = setup_test(),

    [P1, P2, P3] = transaction_handler_pids_alive(TransactionHandlerPids),

    Th1 = my_th1,
    Th2 = my_th2,
    Th3 = my_th3,
    RealUsid = 1,
    Qref = x,
    Did = x,
    RequestData = x,
    HideInactive = false,

    erlang:monitor(process, P1),
    erlang:monitor(process, P2),
    erlang:monitor(process, P3),

    CloseMsg1 = {?CONFD_PROTO_CLOSE_TRANS, Qref, Did, Th1,
                RequestData, RealUsid, HideInactive},
    CloseMsg2 = {?CONFD_PROTO_CLOSE_TRANS, Qref, Did, Th2,
                RequestData, RealUsid, HideInactive},
    CloseMsg3 = {?CONFD_PROTO_CLOSE_TRANS, Qref, Did, Th3,
                RequestData, RealUsid, HideInactive},

    %% Close one transaction_handler
    Worker ! {tcp, no_socket, CloseMsg1},

    %% Syncronize, P1 is dead
    receive
        {'DOWN', _Ref1, process, P1, _Reason1} ->
            ok
    end,

    [P2, P3] = transaction_handler_pids_alive(TransactionHandlerPids),

    %% Close next transaction_handler
    Worker ! {tcp, no_socket, CloseMsg2},

    %% Syncronize, P2 is dead
    receive
        {'DOWN', _Ref2, process, P2, _Reason2} ->
            ok
    end,

    [P3] = transaction_handler_pids_alive(TransactionHandlerPids),

    %% Close final transaction_handler
    Worker ! {tcp, no_socket, CloseMsg3},

    %% Syncronize, P3 is dead
    receive
        {'DOWN', _Ref3, process, P3, _Reason3} ->
            ok
    end,

    %% No need to poll, we received exit messages on
    %% all 3 transaction_handler PIDs
    [] = transaction_handler_pids_alive(TransactionHandlerPids),

    true = is_process_alive(Worker).

worker_dies_ths_die() ->
    cdb_testlib:desc("Test that when a worker process dies, all spawned"
                     " transaction_handler processes die too. To avoid"
                     " resource leak.").
worker_dies_ths_die(_Config) ->
    {ok, DaemonPid, Worker, TransactionHandlerPids} = setup_test(),

    [P1, P2, P3] = transaction_handler_pids_alive(TransactionHandlerPids),

    process_flag(trap_exit, true),

    poll_alive(P1),
    poll_alive(P2),
    poll_alive(P3),

    %% Kill the worker process
    erlang:exit(Worker, kill),

    %% As a result of killing the worker, the econfd daemon
    %% will go down too
    receive
        {'EXIT', DaemonPid, killed} ->
            ok
    end,

    process_flag(trap_exit, false),

    %% Need to make sure that all transaction_handler pids die
    poll_no_transaction_handler_pids_alive(TransactionHandlerPids).

th_dies_worker_lives() ->
    cdb_testlib:desc("Test that when a worker's th process dies,"
                     " the worker lives").
th_dies_worker_lives(_Config) ->
    {ok, _DaemonPid, Worker, TransactionHandlerPids} = setup_test(),

    [P1, P2, P3] = transaction_handler_pids_alive(TransactionHandlerPids),

    %% Kill the worker's transaction_handler processes
    erlang:exit(P1, kill),
    erlang:exit(P2, kill),
    erlang:exit(P3, kill),

    %% After the killings above, no transaction_handler should remain
    poll_no_transaction_handler_pids_alive(TransactionHandlerPids),

    true = is_process_alive(Worker).

daemon_dies_everything_dies() ->
    cdb_testlib:desc("If the daemon goes down, then the worker and the ths"
                     " should do the same.").
daemon_dies_everything_dies(_Config) ->
    {ok, DaemonPid, Worker, TransactionHandlerPids} = setup_test(),

    [P1, P2, P3] = transaction_handler_pids_alive(TransactionHandlerPids),

    process_flag(trap_exit, true),

    %% monitor Worker so that we know when it has stopped
    erlang:monitor(process, Worker),

    poll_alive(P1),
    poll_alive(P2),
    poll_alive(P3),

    %% Kill the daemon
    erlang:exit(DaemonPid, kill),

    %% Syncronize, daemon is dead
    receive
        {'EXIT', DaemonPid, killed} ->
            ok
    end,

    %% Syncronize, worker is dead
    receive
        {'DOWN', _Ref, process, Worker, _Reason} ->
            ok
    end,

    false = is_process_alive(Worker),

    poll_no_transaction_handler_pids_alive(TransactionHandlerPids).

daemon_stop_everything_dies() ->
    cdb_testlib:desc("If the daemon is explicitly stopped, then"
                     " the worker and the ths should do the same.").
daemon_stop_everything_dies(_Config) ->
    {ok, DaemonPid, Worker, TransactionHandlerPids} = setup_test(),

    [P1, P2, P3] = transaction_handler_pids_alive(TransactionHandlerPids),

    process_flag(trap_exit, true),

    %% monitor Worker so that we know when it has stopped
    erlang:monitor(process, Worker),

    poll_alive(P1),
    poll_alive(P2),
    poll_alive(P3),

    %% Stop the daemon
    gen_server:stop(DaemonPid),

    %% Syncronize, daemon is dead
    receive
        {'EXIT', DaemonPid, normal} ->
            ok
    end,

    %% Syncronize, worker is dead
    receive
        {'DOWN', _Ref, process, Worker, _Reason} ->
            ok
    end,

    false = is_process_alive(Worker),

    poll_no_transaction_handler_pids_alive(TransactionHandlerPids).

abort() ->
    cdb_testlib:desc("Test that a transaction_handler can"
                     " be aborted without any crashes").
abort(_Config) ->
    {ok, _DaemonPid, Worker, TransactionHandlerPids} = setup_test(),

    Th1 = my_th1,
    RealUsid = 1,
    Qref = x,
    Did = x,
    RequestData = x,
    HideInactive = false,

    [P1, P2, P3] = transaction_handler_pids_alive(TransactionHandlerPids),

    poll_alive(P1),
    poll_alive(P2),
    poll_alive(P3),

    erlang:monitor(process, P1),

    AbortMsg1 = {?CONFD_PROTO_ABORT, Qref, Did, Th1,
                 RequestData, RealUsid, HideInactive},

    CloseMsg1 = {?CONFD_PROTO_CLOSE_TRANS, Qref, Did, Th1,
                 RequestData, RealUsid, HideInactive},

    %% Abort one transaction_handler
    Worker ! {tcp, no_socket, AbortMsg1},

    %% Synchronization point, wait until P1 is aborted
    poll_aborted(P1),

    %% P1 should still be alive, even though it now is aborted
    true = is_process_alive(P1),

    %% After abort, we should still be able to send
    %% ?CONFD_PROTO_CLOSE_TRANS since this is what the system does.
    Worker ! {tcp, no_socket, CloseMsg1},

    %% Syncronize, P1 is dead
    receive
        {'DOWN', _Ref1, process, P1, _Reason1} ->
            ok
    end,

    [P2, P3] = transaction_handler_pids_alive(TransactionHandlerPids).


%% -----------------------------------------------------------------------------
%% HELPER FUNCTIONS
%% -----------------------------------------------------------------------------

poll_no_transaction_handler_pids_alive(TransactionHandlerPids) ->
    case transaction_handler_pids_alive(TransactionHandlerPids) of
        [] ->
            ok;
        _ ->
            poll_no_transaction_handler_pids_alive(TransactionHandlerPids)
    end.

poll_alive(TransactionHandler) ->
    TransactionHandler ! {get_status, self()},
    receive
        {status, alive} ->
            ok
    after 100 ->
            poll_alive(TransactionHandler)
    end.

poll_aborted(TransactionHandler) ->
    TransactionHandler ! {get_status, self()},
    receive
        {status, aborted} ->
            ok;
        {status, alive} ->
            poll_aborted(TransactionHandler)
    end.

poll_capi_session_map_populated(Expect) ->
    CapiState = sys:get_state(whereis(capi_server)),
    [CapiDaemon] = element(5, CapiState),
    SessionMap = element(5, CapiDaemon),
    case lists:sort(SessionMap) of
        Expect ->
            ok;
        _ ->
            timer:sleep(25),
            poll_capi_session_map_populated(Expect)
    end.

transaction_handler_pids_alive(TransactionHandlers) ->
    Pids = [P ||
               {P, _} <- maps:to_list(TransactionHandlers),
               is_pid(P)],

    [THPid || THPid <- Pids, is_process_alive(THPid)].

setup_test() ->
    Trans = #confd_trans_cbs{init = fun(Tctx) -> {ok, Tctx} end},
    Data = #confd_data_cbs{get_elem = fun(_ , _) -> ok end},
    {ok, DaemonPid} = econfd:init_daemon(simple, ?CONFD_TRACE, user, none,
                                         {127, 0, 0, 1}, ?CONFD_PORT),

    ok = econfd:register_trans_cb(DaemonPid, Trans),
    ok = econfd:register_data_cb(DaemonPid, Data),
    ok = econfd:register_done(DaemonPid),

    Th1 = my_th1,
    Th2 = my_th2,
    Th3 = my_th3,
    Usid = 1,

    %% Get the gen_server state to see the spawned child process
    St = sys:get_state(DaemonPid),

    Worker = St#confd_daemon_ctx.worker_pid,
    Worker ! {get_state, self()},

    %% Make sure the worker pid is up and running before sending
    %% CONFD_PROTO_NEW_TRANS messages
    receive
        {state, #confd_daemon_ctx{
                   transaction_handlers = #{}}} ->
            ok
    end,

    %% Make it look like some new transactions have turned up
    Message1 = {?CONFD_PROTO_NEW_TRANS, x, x, x, x, Usid, x, Th1, x, x},
    Message2 = {?CONFD_PROTO_NEW_TRANS, x, x, x, x, Usid, x, Th2, x, x},
    Message3 = {?CONFD_PROTO_NEW_TRANS, x, x, x, x, Usid, x, Th3, x, x},

    DaemonPid ! {tcp, no_socket, Message1},

    poll_capi_session_map_populated([{my_th1,1}]),

    DaemonPid ! {tcp, no_socket, Message2},

    poll_capi_session_map_populated([{my_th1,1}, {my_th2,1}]),

    DaemonPid ! {tcp, no_socket, Message3},

    %% Wait so that we know capi_server's session map is updated,
    %% before allowing the rest of the test to run
    poll_capi_session_map_populated([{my_th1,1},{my_th2,1},{my_th3,1}]),

    %% Finally, get the transacation_handler pids we want to test
    Worker ! {get_state, self()},

    receive
        {state, #confd_daemon_ctx{
                   transaction_handlers = TransactionHandlerPids}} ->
            TransactionHandlerPids
    end,

    {ok, DaemonPid, Worker, TransactionHandlerPids}.
