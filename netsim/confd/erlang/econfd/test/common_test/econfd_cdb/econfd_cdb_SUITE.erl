%%% ----------------------------------------------------------------------------
%%% @doc Erlang Common Test of econfd_cdb
%%%
%%% ----------------------------------------------------------------------------
-module(econfd_cdb_SUITE).

-compile(export_all).

-include_lib("common_test/include/ct.hrl").
-include_lib("ct_support/include/cdb_testlib.hrl").
-include_lib("eunit/include/eunit.hrl").

-include("econfd/include/econfd_errors.hrl").
-include("econfd/include/econfd.hrl").
-include("econfd/src/econfd_cdb.hrl").

-define(NS, 'http://tail-f.com/ns/econfd_cdb').
-define(INVALID_CDB, 4).

%% -----------------------------------------------------------------------------
%% COMMON TEST CALLBACK FUNCTIONS
%% -----------------------------------------------------------------------------

all() ->
    [
     journal_compaction_all,
     journal_compaction_a_cdb,
     journal_compaction_o_cdb,
     journal_compaction_s_cdb,
     journal_compaction_invalid_cdb,
     get_compaction_info,
     get_compaction_info_not_available_cdb_file,
     get_compaction_info_invalid_cdb_file
    ].

suite() ->
    [{timetrap, {minutes, 10}}].

init_per_suite(Config) ->
    ok = cdb_testlib:confd_start(),
    Config.

end_per_suite(_Config) ->
    ok.

%% -----------------------------------------------------------------------------
%% TEST CASES
%% -----------------------------------------------------------------------------

journal_compaction_all() ->
    cdb_testlib:desc(
      "Initiate journal compaction for all CDB files. "
      "Compaction will be initiated for A.cdb, O.cdb and S.cdb "
      "but only A.cdb and O.cdb will be compacted as snapshot db "
      "is not available in ConfD").
journal_compaction_all(_Config) ->
    ensure_compaction_with_meck(
      fun() ->
              {ok, Sock} = econfd_cdb:connect(),
              ok = econfd_cdb:initiate_journal_compaction(Sock),
              ok = econfd_cdb:close(Sock)
      end, [cdb_db, cdb_op, cdb_snap]).

journal_compaction_a_cdb() ->
    cdb_testlib:desc("Initiate journal compaction for A.cdb").
journal_compaction_a_cdb(_Config) ->
    ensure_compaction_and_unlock_with_meck(
      fun() ->
              {ok, Sock} = econfd_cdb:connect(),
              ok = econfd_cdb:initiate_journal_dbfile_compaction(
                     Sock, ?EM_COMPACTION_A_CDB),
              ok = econfd_cdb:close(Sock)
      end).

journal_compaction_o_cdb() ->
    cdb_testlib:desc("Initiate journal compaction for O.cdb").
journal_compaction_o_cdb(_Config) ->
    ensure_compaction_with_meck(
      fun() ->
              {ok, Sock} = econfd_cdb:connect(),
              ok = econfd_cdb:initiate_journal_dbfile_compaction(
                     Sock, ?EM_COMPACTION_O_CDB),
              ok = econfd_cdb:close(Sock)
      end, [cdb_op]).

journal_compaction_s_cdb() ->
    cdb_testlib:desc(
      "Initiate journal compaction for S.cdb. "
      "Compaction will be initiated for S.cdb, but it will not "
      "be compacted as snapshot db is not available in ConfD").
journal_compaction_s_cdb(_Config) ->
    ensure_compaction_with_meck(
      fun() ->
              {ok, Sock} = econfd_cdb:connect(),
              ok = econfd_cdb:initiate_journal_dbfile_compaction(
                     Sock, ?EM_COMPACTION_S_CDB),
              ok = econfd_cdb:close(Sock)
      end, [cdb_snap]).

journal_compaction_invalid_cdb() ->
    cdb_testlib:desc("Initiate journal compaction for an invalid cdb file").
journal_compaction_invalid_cdb(_Config) ->
    {ok, Sock} = econfd_cdb:connect(),
    {error, {?CONFD_ERR_PROTOUSAGE, <<"Invalid DB file 4">>}} =
        econfd_cdb:initiate_journal_dbfile_compaction(Sock, ?INVALID_CDB),
    ok = econfd_cdb:close(Sock).

get_compaction_info() ->
    cdb_testlib:desc("Request to get compaction info on a valid CDB file").
get_compaction_info(_Config) ->
    BaseIKP = [[?NS|econfd_cdb]],
    cs_trans_testlib:with_trans(
      fun(Th) ->
              ok = cs_trans:create(Th, BaseIKP),
              ok = cs_trans:apply(Th)
      end),

    {ok, Sock} = econfd_cdb:connect(),
    {ok, Info1} = econfd_cdb:get_compaction_info(Sock, ?EM_COMPACTION_A_CDB),
    ?assert(Info1#compaction_info.ntrans > 0),
    ?assert(Info1#compaction_info.fsize_current >
            Info1#compaction_info.fsize_previous),
    ensure_compaction_and_unlock_with_meck(
      fun() ->
              ok = econfd_cdb:initiate_journal_dbfile_compaction(
                     Sock, ?EM_COMPACTION_A_CDB)
      end),
    {ok, Info2} = econfd_cdb:get_compaction_info(Sock, ?EM_COMPACTION_A_CDB),
    ?assertEqual(0, Info2#compaction_info.ntrans),
    ?assertEqual(Info2#compaction_info.fsize_current,
                 Info2#compaction_info.fsize_previous),
    ?assert(Info2#compaction_info.last_time > Info1#compaction_info.last_time),
    ok = econfd_cdb:close(Sock),

    cs_trans_testlib:with_trans(
      fun(Th) ->
              ok = cs_trans:delete(Th, BaseIKP),
              ok = cs_trans:apply(Th)
      end).

get_compaction_info_not_available_cdb_file() ->
    cdb_testlib:desc("Request to get compaction info on snapshot db fails "
                     "since snapshot db is not available in ConfD.").
get_compaction_info_not_available_cdb_file(_Config) ->
    {ok, Sock} = econfd_cdb:connect(),
    {error, {?CONFD_ERR_UNAVAILABLE, <<"Could not get compaction info">>}} =
        econfd_cdb:get_compaction_info(Sock, ?EM_COMPACTION_S_CDB),
    ok = econfd_cdb:close(Sock).

get_compaction_info_invalid_cdb_file() ->
    cdb_testlib:desc("Request to get compaction info on an invalid db fails").
get_compaction_info_invalid_cdb_file(_Config) ->
    {ok, Sock} = econfd_cdb:connect(),
    {error, {?CONFD_ERR_PROTOUSAGE, <<"Invalid DB file 4">>}} =
        econfd_cdb:get_compaction_info(Sock, ?INVALID_CDB),
    ok = econfd_cdb:close(Sock).

%% -----------------------------------------------------------------------------
%% SUPPORT FUNCTIONS
%% -----------------------------------------------------------------------------
ensure_compaction_with_meck(F, CdbMods) ->
    Self = self(),
    [begin
         ok = meck:new(M, [passthrough]),
         meck:expect(M, initiate_compaction,
                     fun () ->
                             Res = meck:passthrough([]),
                             Self ! initiated,
                             Res
                     end)
     end || M <- CdbMods],
    ok = meck:new(xds_ramdisk, [passthrough]),
    meck:expect(xds_ramdisk, save,
                fun (Xds, 'manual') ->
                        Res = meck:passthrough([Xds, 'manual']),
                        Self ! compacted,
                        Res
                end),
    F(),
    %% M:initiate_compaction/0 will be called for each CdbMods.
    [receive initiated -> ok end || _M <- CdbMods],
    %% xds_ramdisk:save will not be called for  cdb_snap.
    [receive compacted -> ok end || _M <- CdbMods -- [cdb_snap]],
    meck:unload().

ensure_compaction_and_unlock_with_meck(F) ->
    Self = self(),
    ok = meck:new(xds_ramdisk, [passthrough]),
    ok = meck:new(cdb_db, [passthrough]),
    meck:expect(xds_ramdisk, save,
                fun (Xds, 'manual') ->
                        Res = meck:passthrough([Xds, 'manual']),
                        Self ! save_done,
                        Res
                end),
    meck:expect(cdb_db, return_ramdisk,
                fun (Xds) ->
                        Res = meck:passthrough([Xds]),
                        Self ! unlock_done,
                        Res
                end),
    F(),
    receive save_done -> ok end,
    receive unlock_done -> ok end,
    meck:unload().
