/*

This file is generated by gen-constants.py. 2024-05-14 23:17:49.492002

Edit at your own risk!!!!

*/


/* /lab/home/jenkins/build/confd_dir/include/confd_cdb.h */

/* Generated by gcc -w -E -DCONFD_C_PRODUCT_NCS -dD - command */

/* enum cdb_db_type { */
     ADD_CONST(CDB_RUNNING, "RUNNING");
     ADD_CONST(CDB_STARTUP, "STARTUP");
     ADD_CONST(CDB_OPERATIONAL, "OPERATIONAL");
     ADD_CONST(CDB_PRE_COMMIT_RUNNING, "PRE_COMMIT_RUNNING");
/* }; */
/* enum cdb_dbfile_type { */
     ADD_CONST(CDB_A_CDB, "A_CDB");
     ADD_CONST(CDB_O_CDB, "O_CDB");
     ADD_CONST(CDB_S_CDB, "S_CDB");
/* }; */
/* enum cdb_sub_type { */
     ADD_CONST(CDB_SUB_RUNNING, "SUB_RUNNING");
     ADD_CONST(CDB_SUB_RUNNING_TWOPHASE, "SUB_RUNNING_TWOPHASE");
     ADD_CONST(CDB_SUB_OPERATIONAL, "SUB_OPERATIONAL");
/* }; */
/* enum cdb_sub_notification { */
     ADD_CONST(CDB_SUB_PREPARE, "SUB_PREPARE");
     ADD_CONST(CDB_SUB_COMMIT, "SUB_COMMIT");
     ADD_CONST(CDB_SUB_ABORT, "SUB_ABORT");
     ADD_CONST(CDB_SUB_OPER, "SUB_OPER");
/* }; */
/* enum cdb_sock_type { */
     ADD_CONST(CDB_READ_SOCKET, "READ_SOCKET");
     ADD_CONST(CDB_SUBSCRIPTION_SOCKET, "SUBSCRIPTION_SOCKET");
     ADD_CONST(CDB_DATA_SOCKET, "DATA_SOCKET");
/* }; */
/* enum cdb_phase_flags { */
     ADD_CONST(CDB_FLAG_INIT, "FLAG_INIT");
     ADD_CONST(CDB_FLAG_UPGRADE, "FLAG_UPGRADE");
/* }; */
ADD_CONST(CDB_LOCK_WAIT, "LOCK_WAIT");
ADD_CONST(CDB_LOCK_SESSION, "LOCK_SESSION");
ADD_CONST(CDB_LOCK_REQUEST, "LOCK_REQUEST");
ADD_CONST(CDB_LOCK_PARTIAL, "LOCK_PARTIAL");
ADD_CONST(CDB_READ_COMMITTED, "READ_COMMITTED");
ADD_CONST(CDB_SUB_WANT_ABORT_ON_ABORT, "SUB_WANT_ABORT_ON_ABORT");
ADD_CONST(CDB_SUB_FLAG_IS_LAST, "SUB_FLAG_IS_LAST");
ADD_CONST(CDB_SUB_FLAG_TRIGGER, "SUB_FLAG_TRIGGER");
ADD_CONST(CDB_SUB_FLAG_REVERT, "SUB_FLAG_REVERT");
ADD_CONST(CDB_SUB_FLAG_HA_SYNC, "SUB_FLAG_HA_SYNC");
ADD_CONST(CDB_SUB_FLAG_HA_IS_SECONDARY, "SUB_FLAG_HA_IS_SECONDARY");
ADD_CONST(CDB_SUB_FLAG_HA_IS_SLAVE, "SUB_FLAG_HA_IS_SLAVE");
ADD_CONST(CDB_GET_MODS_INCLUDE_LISTS, "GET_MODS_INCLUDE_LISTS");
ADD_CONST(CDB_GET_MODS_REVERSE, "GET_MODS_REVERSE");
ADD_CONST(CDB_GET_MODS_SUPPRESS_DEFAULTS, "GET_MODS_SUPPRESS_DEFAULTS");
ADD_CONST(CDB_GET_MODS_CLI_NO_BACKQUOTES, "GET_MODS_CLI_NO_BACKQUOTES");
ADD_CONST(CDB_GET_MODS_INCLUDE_MOVES, "GET_MODS_INCLUDE_MOVES");
ADD_CONST(CDB_GET_MODS_WANT_ANCESTOR_DELETE, "GET_MODS_WANT_ANCESTOR_DELETE");
/* enum cdb_subscription_sync_type { */
     ADD_CONST(CDB_DONE_PRIORITY, "DONE_PRIORITY");
     ADD_CONST(CDB_DONE_SOCKET, "DONE_SOCKET");
     ADD_CONST(CDB_DONE_TRANSACTION, "DONE_TRANSACTION");
     ADD_CONST(CDB_DONE_OPERATIONAL, "DONE_OPERATIONAL");
/* }; */