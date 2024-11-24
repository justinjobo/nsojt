/*

This file is generated by gen-constants.py. 2024-05-14 23:17:49.470043

Edit at your own risk!!!!

*/


/* /lab/home/jenkins/build/confd_dir/include/confd_maapi.h */

/* Generated by gcc -w -E -DCONFD_C_PRODUCT_NCS -dD - command */

ADD_CONST(CONFD_ECHO, "ECHO");
ADD_CONST(CONFD_NOECHO, "NOECHO");
ADD_CONST(MAAPI_FLAG_HINT_BULK, "FLAG_HINT_BULK");
ADD_CONST(MAAPI_FLAG_NO_DEFAULTS, "FLAG_NO_DEFAULTS");
ADD_CONST(MAAPI_FLAG_CONFIG_ONLY, "FLAG_CONFIG_ONLY");
ADD_CONST(MAAPI_FLAG_HIDE_INACTIVE, "FLAG_HIDE_INACTIVE");
ADD_CONST(MAAPI_FLAG_DELAYED_WHEN, "FLAG_DELAYED_WHEN");
ADD_CONST(MAAPI_FLAG_NO_CONFIG_CACHE, "FLAG_NO_CONFIG_CACHE");
ADD_CONST(MAAPI_FLAG_CONFIG_CACHE_ONLY, "FLAG_CONFIG_CACHE_ONLY");
ADD_CONST(MAAPI_COMMIT_NCS_NO_REVISION_DROP, "COMMIT_NCS_NO_REVISION_DROP");
ADD_CONST(MAAPI_COMMIT_NCS_NO_DEPLOY, "COMMIT_NCS_NO_DEPLOY");
ADD_CONST(MAAPI_COMMIT_NCS_NO_NETWORKING, "COMMIT_NCS_NO_NETWORKING");
ADD_CONST(MAAPI_COMMIT_NCS_NO_OUT_OF_SYNC_CHECK, "COMMIT_NCS_NO_OUT_OF_SYNC_CHECK");
ADD_CONST(MAAPI_COMMIT_NCS_COMMIT_QUEUE_BYPASS, "COMMIT_NCS_COMMIT_QUEUE_BYPASS");
ADD_CONST(MAAPI_COMMIT_NCS_COMMIT_QUEUE_ASYNC, "COMMIT_NCS_COMMIT_QUEUE_ASYNC");
ADD_CONST(MAAPI_COMMIT_NCS_COMMIT_QUEUE_SYNC, "COMMIT_NCS_COMMIT_QUEUE_SYNC");
ADD_CONST(MAAPI_COMMIT_NCS_NO_OVERWRITE, "COMMIT_NCS_NO_OVERWRITE");
ADD_CONST(MAAPI_COMMIT_NCS_COMMIT_QUEUE_LOCK, "COMMIT_NCS_COMMIT_QUEUE_LOCK");
ADD_CONST(MAAPI_COMMIT_NCS_COMMIT_QUEUE_BLOCK_OTHERS, "COMMIT_NCS_COMMIT_QUEUE_BLOCK_OTHERS");
ADD_CONST(MAAPI_COMMIT_NCS_COMMIT_QUEUE_ATOMIC, "COMMIT_NCS_COMMIT_QUEUE_ATOMIC");
ADD_CONST(MAAPI_COMMIT_NCS_COMMIT_QUEUE_NONATOMIC, "COMMIT_NCS_COMMIT_QUEUE_NONATOMIC");
ADD_CONST(MAAPI_COMMIT_NCS_COMMIT_QUEUE_CONTINUE_ON_ERROR, "COMMIT_NCS_COMMIT_QUEUE_CONTINUE_ON_ERROR");
ADD_CONST(MAAPI_COMMIT_NCS_COMMIT_QUEUE_ROLLBACK_ON_ERROR, "COMMIT_NCS_COMMIT_QUEUE_ROLLBACK_ON_ERROR");
ADD_CONST(MAAPI_COMMIT_NCS_COMMIT_QUEUE_STOP_ON_ERROR, "COMMIT_NCS_COMMIT_QUEUE_STOP_ON_ERROR");
ADD_CONST(MAAPI_COMMIT_NCS_USE_LSA, "COMMIT_NCS_USE_LSA");
ADD_CONST(MAAPI_COMMIT_NCS_NO_LSA, "COMMIT_NCS_NO_LSA");
ADD_CONST(MAAPI_COMMIT_NCS_RECONCILE_KEEP_NON_SERVICE_CONFIG, "COMMIT_NCS_RECONCILE_KEEP_NON_SERVICE_CONFIG");
ADD_CONST(MAAPI_COMMIT_NCS_RECONCILE_DISCARD_NON_SERVICE_CONFIG, "COMMIT_NCS_RECONCILE_DISCARD_NON_SERVICE_CONFIG");
ADD_CONST(MAAPI_COMMIT_NCS_NO_FASTMAP, "COMMIT_NCS_NO_FASTMAP");
ADD_CONST(MAAPI_COMMIT_NCS_BYPASS_COMMIT_QUEUE, "COMMIT_NCS_BYPASS_COMMIT_QUEUE");
ADD_CONST(MAAPI_COMMIT_NCS_THROUGH_COMMIT_QUEUE, "COMMIT_NCS_THROUGH_COMMIT_QUEUE");
ADD_CONST(MAAPI_COMMIT_NCS_ASYNC_COMMIT_QUEUE, "COMMIT_NCS_ASYNC_COMMIT_QUEUE");
ADD_CONST(MAAPI_COMMIT_NCS_SYNC_COMMIT_QUEUE, "COMMIT_NCS_SYNC_COMMIT_QUEUE");
/* enum maapi_delete_how { */
     ADD_CONST(MAAPI_DEL_SAFE, "DEL_SAFE");
     ADD_CONST(MAAPI_DEL_ALL, "DEL_ALL");
     ADD_CONST(MAAPI_DEL_EXPORTED, "DEL_EXPORTED");
/* }; */
/* enum maapi_move_where { */
     ADD_CONST(MAAPI_MOVE_FIRST, "MOVE_FIRST");
     ADD_CONST(MAAPI_MOVE_BEFORE, "MOVE_BEFORE");
     ADD_CONST(MAAPI_MOVE_AFTER, "MOVE_AFTER");
     ADD_CONST(MAAPI_MOVE_LAST, "MOVE_LAST");
/* }; */
/* enum ncs_commit_queue_status { */
     ADD_CONST(NCS_COMMIT_QUEUE_NONE, "NCS_COMMIT_QUEUE_NONE");
     ADD_CONST(NCS_COMMIT_QUEUE_ASYNC, "NCS_COMMIT_QUEUE_ASYNC");
     ADD_CONST(NCS_COMMIT_QUEUE_COMPLETED, "NCS_COMMIT_QUEUE_COMPLETED");
     ADD_CONST(NCS_COMMIT_QUEUE_TIMEOUT, "NCS_COMMIT_QUEUE_TIMEOUT");
     ADD_CONST(NCS_COMMIT_QUEUE_DELETED, "NCS_COMMIT_QUEUE_DELETED");
     ADD_CONST(NCS_COMMIT_QUEUE_FAILED, "NCS_COMMIT_QUEUE_FAILED");
/* }; */
/* enum maapi_snmp_var_type { */
     ADD_CONST(C_SNMP_VARIABLE, "C_SNMP_VARIABLE");
     ADD_CONST(C_SNMP_OID, "C_SNMP_OID");
     ADD_CONST(C_SNMP_COL_ROW, "C_SNMP_COL_ROW");
/* }; */
ADD_CONST(MAAPI_CONFIG_XML, "CONFIG_XML");
ADD_CONST(MAAPI_CONFIG_J, "CONFIG_J");
ADD_CONST(MAAPI_CONFIG_C, "CONFIG_C");
ADD_CONST(MAAPI_CONFIG_WITH_DEFAULTS, "CONFIG_WITH_DEFAULTS");
ADD_CONST(MAAPI_CONFIG_SHOW_DEFAULTS, "CONFIG_SHOW_DEFAULTS");
ADD_CONST(MAAPI_CONFIG_C_IOS, "CONFIG_C_IOS");
ADD_CONST(MAAPI_CONFIG_MERGE, "CONFIG_MERGE");
ADD_CONST(MAAPI_CONFIG_WITH_OPER, "CONFIG_WITH_OPER");
ADD_CONST(MAAPI_CONFIG_XPATH, "CONFIG_XPATH");
ADD_CONST(MAAPI_CONFIG_XML_PRETTY, "CONFIG_XML_PRETTY");
ADD_CONST(MAAPI_CONFIG_REPLACE, "CONFIG_REPLACE");
ADD_CONST(MAAPI_CONFIG_HIDE_ALL, "CONFIG_HIDE_ALL");
ADD_CONST(MAAPI_CONFIG_UNHIDE_ALL, "CONFIG_UNHIDE_ALL");
ADD_CONST(MAAPI_CONFIG_AUTOCOMMIT, "CONFIG_AUTOCOMMIT");
ADD_CONST(MAAPI_CONFIG_CONTINUE_ON_ERROR, "CONFIG_CONTINUE_ON_ERROR");
ADD_CONST(MAAPI_CONFIG_SUPPRESS_ERRORS, "CONFIG_SUPPRESS_ERRORS");
ADD_CONST(MAAPI_CONFIG_XML_LOAD_LAX, "CONFIG_XML_LOAD_LAX");
ADD_CONST(MAAPI_CONFIG_JSON, "CONFIG_JSON");
ADD_CONST(MAAPI_CONFIG_WITH_SERVICE_META, "CONFIG_WITH_SERVICE_META");
ADD_CONST(MAAPI_CONFIG_NO_PARENTS, "CONFIG_NO_PARENTS");
ADD_CONST(MAAPI_CONFIG_OPER_ONLY, "CONFIG_OPER_ONLY");
ADD_CONST(MAAPI_CONFIG_NO_BACKQUOTE, "CONFIG_NO_BACKQUOTE");
ADD_CONST(MAAPI_CONFIG_CDB_ONLY, "CONFIG_CDB_ONLY");
ADD_CONST(MAAPI_CONFIG_TURBO_C, "CONFIG_TURBO_C");
ADD_CONST(MAAPI_CONFIG_READ_WRITE_ACCESS_ONLY, "CONFIG_READ_WRITE_ACCESS_ONLY");
ADD_CONST(MAAPI_CMD_NO_FULLPATH, "CMD_NO_FULLPATH");
ADD_CONST(MAAPI_CMD_NO_HIDDEN, "CMD_NO_HIDDEN");
ADD_CONST(MAAPI_CMD_NO_AAA, "CMD_NO_AAA");
ADD_CONST(MAAPI_CMD_KEEP_PIPE, "CMD_KEEP_PIPE");
ADD_CONST(MAAPI_FLAG_EMIT_PARENTS, "FLAG_EMIT_PARENTS");
ADD_CONST(MAAPI_FLAG_DELETE, "FLAG_DELETE");
ADD_CONST(MAAPI_FLAG_NON_RECURSIVE, "FLAG_NON_RECURSIVE");
ADD_CONST(MAAPI_UPGRADE_KILL_ON_TIMEOUT, "UPGRADE_KILL_ON_TIMEOUT");