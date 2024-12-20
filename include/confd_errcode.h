/*
 * Copyright 2010 Tail-F Systems AB
 */

#ifndef _CONFD_ERRCODE_H
#define _CONFD_ERRCODE_H 1

#define CONFD_ERRTYPE_VALIDATION (1 << 0)
#define CONFD_ERRTYPE_BAD_VALUE (1 << 1)
#define CONFD_ERRTYPE_CLI (1 << 2)
#define CONFD_ERRTYPE_MISC (1 << 3)
#define CONFD_ERRTYPE_NCS (1 << 4)
#define CONFD_ERRTYPE_OPERATION (1 << 5)
#define CONFD_BAD_VALUE_STRING_FUN 1
#define CONFD_BAD_VALUE_WRONG_DECIMAL64_FRACTION_DIGITS 2
#define CONFD_BAD_VALUE_FRACTION_DIGITS 3
#define CONFD_BAD_VALUE_TOTAL_DIGITS 4
#define CONFD_BAD_VALUE_MAX_EXCLUSIVE 5
#define CONFD_BAD_VALUE_MAX_INCLUSIVE 6
#define CONFD_BAD_VALUE_MIN_EXCLUSIVE 7
#define CONFD_BAD_VALUE_MIN_INCLUSIVE 8
#define CONFD_BAD_VALUE_INVALID_REGEX 9
#define CONFD_BAD_VALUE_UNIQUELIST 10
#define CONFD_BAD_VALUE_ENUMERATION 11
#define CONFD_BAD_VALUE_PATTERN 12
#define CONFD_BAD_VALUE_MIN_LENGTH 13
#define CONFD_BAD_VALUE_MAX_LENGTH 14
#define CONFD_BAD_VALUE_LENGTH 15
#define CONFD_BAD_VALUE_CUSTOM_FACET_ERROR_MESSAGE 16
#define CONFD_BAD_VALUE_USER_ERROR 17
#define CONFD_BAD_VALUE_INVALID_FACET 18
#define CONFD_BAD_VALUE_BAD_LEXICAL 19
#define CONFD_BAD_VALUE_BAD_VALUE 20
#define CONFD_BAD_VALUE_BAD_TAG 21
#define CONFD_BAD_VALUE_UNKNOWN_BIT_LABEL 22
#define CONFD_BAD_VALUE_INVALID_TYPE_NAME 23
#define CONFD_BAD_VALUE_NO_DEFAULT_NAMESPACE 24
#define CONFD_BAD_VALUE_UNKNOWN_NAMESPACE_PREFIX 25
#define CONFD_BAD_VALUE_UNKNOWN_NAMESPACE 26
#define CONFD_BAD_VALUE_MISSING_NAMESPACE 27
#define CONFD_BAD_VALUE_VALUE2VALUE_FUN 28
#define CONFD_BAD_VALUE_RANGE 29
#define CONFD_BAD_VALUE_WRONG_NUMBER_IDENTIFIERS 30
#define CONFD_BAD_VALUE_POP_TOO_FAR 31
#define CONFD_BAD_VALUE_BAD_KEY_TAG 32
#define CONFD_BAD_VALUE_SYMLINK_BAD_KEY_REFERENCE 33
#define CONFD_BAD_VALUE_INVALID_XPATH 34
#define CONFD_BAD_VALUE_NOT_RESTRICTED_XPATH 35
#define CONFD_BAD_VALUE_XPATH_ERROR 36
#define CONFD_BAD_VALUE_MISSING_KEY 37
#define CONFD_CLI_COMMAND_ERROR 1
#define CONFD_CLI_COMMAND_ABORTED 2
#define CONFD_CLI_COMMAND_FAILED 3
#define CONFD_CLI_CUSTOM_ERROR 4
#define CONFD_CLI_NOT_ALLOWED 5
#define CONFD_CLI_BAD_LEAF_VALUE 6
#define CONFD_CLI_ELEM_NOT_WRITABLE 7
#define CONFD_CLI_INCOMPLETE_PATH 8
#define CONFD_CLI_INCOMPLETE_COMMAND 9
#define CONFD_CLI_DELETE_ALL_FAILED 10
#define CONFD_CLI_DELETE_FAILED 11
#define CONFD_CLI_DELETE_ERROR 12
#define CONFD_CLI_ACTION_NOT_FOUND 13
#define CONFD_CLI_ELEMENT_NOT_FOUND 14
#define CONFD_CLI_INVALID_ROLLBACK_NR 15
#define CONFD_CLI_BAD_ACTION_RESPONSE 16
#define CONFD_CLI_MISSING_ACTION_PARAM 17
#define CONFD_CLI_MISSING_ACTION_PARAM_VALUE 18
#define CONFD_CLI_READ_ERROR 19
#define CONFD_CLI_OPEN_FILE 20
#define CONFD_CLI_INVALID_PASSWORD 21
#define CONFD_CLI_UNKNOWN_HIDEGROUP 22
#define CONFD_CLI_CREATE_PATH 23
#define CONFD_CLI_REALLOCATE 24
#define CONFD_CLI_ON_LINE 25
#define CONFD_CLI_ON_LINE_DESC 26
#define CONFD_CLI_NOT_WRITABLE 27
#define CONFD_CLI_NOT_FOUND 28
#define CONFD_CLI_SET_FAILED 29
#define CONFD_CLI_MOVE_FAILED 30
#define CONFD_CLI_COPY_FAILED 31
#define CONFD_CLI_COPY_CONFIG_FAILED 32
#define CONFD_CLI_COPY_PATH_IDENTICAL 33
#define CONFD_CLI_NOT_SUPPORTED 34
#define CONFD_CLI_TARGET_EXISTS 35
#define CONFD_CLI_UNKNOWN_MODE 36
#define CONFD_CLI_FAILED_CREATE_CONTEXT 37
#define CONFD_CLI_FAILED_COPY_RUNNING 38
#define CONFD_CLI_CONFIRMED_NOT_SUPPORTED 39
#define CONFD_CLI_WRITE_CONFIG_FAILED 40
#define CONFD_CLI_FAILED_OPEN_STARTUP 41
#define CONFD_CLI_FAILED_OPEN_STARTUP_CONFIG 42
#define CONFD_CLI_MUST_BE_INTEGER 43
#define CONFD_CLI_NO_SUCH_SESSION 44
#define CONFD_CLI_NO_SUCH_ELEMENT 45
#define CONFD_CLI_WILDCARD_NOT_ALLOWED 46
#define CONFD_CLI_NO_SUCH_USER 47
#define CONFD_CLI_MESSAGE_TOO_LARGE 48
#define CONFD_CLI_FAILED_TERM_REDIRECT 49
#define CONFD_CLI_NOT_A_DIRECTORY 50
#define CONFD_CLI_NOT_A_FILE 51
#define CONFD_CLI_ILLEGAL_DIRECTORY_NAME 52
#define CONFD_CLI_ILLEGAL_FILENAME 53
#define CONFD_CLI_INVALID_PARAMETER 54
#define CONFD_CLI_MISSING_DISPLAY_GROUP 55
#define CONFD_CLI_EXPECTED_BOL 56
#define CONFD_CLI_EXPECTED_EOL 57
#define CONFD_CLI_INVALID_PATH 58
#define CONFD_CLI_INVALID_SELECT 59
#define CONFD_CLI_UNKNOWN_ELEMENT 60
#define CONFD_CLI_UNKNOWN_ARGUMENT 61
#define CONFD_CLI_UNKNOWN_COMMAND 62
#define CONFD_CLI_AMBIGUOUS_COMMAND 63
#define CONFD_CLI_INCOMPLETE_PATTERN 64
#define CONFD_CLI_MISSING_ELEMENT 65
#define CONFD_CLI_ELEMENT_DOES_NOT_EXIST 66
#define CONFD_CLI_INCOMPLETE_CMD_PATH 67
#define CONFD_CLI_MISSING_VALUE 68
#define CONFD_CLI_MISSING_ARGUMENT 69
#define CONFD_CLI_MUST_BE_AN_INTEGER 70
#define CONFD_CLI_MUST_BE_TRUE_OR_FALSE 71
#define CONFD_CLI_START_REPLAY_FAILED 72
#define CONFD_CLI_SENSITIVE_DATA 73
#define CONFD_CLI_CDM_NOT_SUPPORTED 74
#define CONFD_CLI_ELEMENT_MANDATORY 75
#define CONFD_MISC_ROLLBACK_DISABLED 1
#define CONFD_MISC_NO_SUCH_FILE 2
#define CONFD_MISC_IN_USE 3
#define CONFD_MISC_LOCKED_BY 4
#define CONFD_MISC_ACCESS_DENIED 5
#define CONFD_MISC_RESOURCE_DENIED 6
#define CONFD_MISC_INCONSISTENT_VALUE 7
#define CONFD_MISC_MISSING_INSTANCE 8
#define CONFD_MISC_WHEN_FAILED 9
#define CONFD_MISC_INTERRUPT 10
#define CONFD_MISC_TOO_MANY_SESSIONS 11
#define CONFD_MISC_TOO_MANY_TRANSACTIONS 12
#define CONFD_MISC_NODE_IS_READONLY 13
#define CONFD_MISC_NODE_WAS_READONLY 14
#define CONFD_MISC_UPGRADE_IN_PROGRESS 15
#define CONFD_MISC_BAD_PERSIST_ID 16
#define CONFD_MISC_CANDIDATE_ABORT_BAD_USID 17
#define CONFD_MISC_INDEXED_VIEW_LIST_TOO_BIG 18
#define CONFD_MISC_APPLICATION 19
#define CONFD_MISC_APPLICATION_INTERNAL 20
#define CONFD_MISC_INTERNAL 21
#define CONFD_MISC_EXTERNAL 22
#define CONFD_MISC_PROTO_USAGE 23
#define CONFD_MISC_SUBAGENT_DOWN 24
#define CONFD_MISC_SUBAGENT_ERROR 25
#define CONFD_MISC_FILE_SYNTAX_ERROR_1 26
#define CONFD_MISC_FILE_OPEN_ERROR 27
#define CONFD_MISC_FILE_SYNTAX_ERROR 28
#define CONFD_MISC_FILE_CREATE_PATH 29
#define CONFD_MISC_FILE_MOVE_PATH 30
#define CONFD_MISC_FILE_SET_PATH 31
#define CONFD_MISC_FILE_DELETE_PATH 32
#define CONFD_MISC_FILE_ACCESS_PATH 33
#define CONFD_MISC_FILE_BAD_PATH 34
#define CONFD_MISC_FILE_BAD_VALUE 35
#define CONFD_MISC_FILE_EOF 36
#define CONFD_MISC_CDB_OPER_UNAVAILABLE 37
#define CONFD_MISC_OPERATION_NOT_SUPPORTED 38
#define CONFD_MISC_SNMP_ERROR 39
#define CONFD_MISC_SNMP_TIMEOUT 40
#define CONFD_MISC_SNMP_BAD_VALUE 41
#define CONFD_MISC_SNMP_BAD_INDEX 42
#define CONFD_MISC_NOT_IMPLEMENTED 43
#define CONFD_MISC_DATA_MISSING 44
#define CONFD_MISC_EXTERNAL_TIMEOUT 45
#define CONFD_MISC_INDEXED_VIEW_LIST_HOLE 46
#define CONFD_MISC_XPATH_COMPILE 51
#define CONFD_MISC_FILE_CORRUPT 52
#define CONFD_MISC_RESOLVE_NEEDED 53
#define CONFD_MISC_TRANSACTION_CONFLICT 54
#define CONFD_MISC_HA_ABORT 55
#define CONFD_MISC_REACHED_MAX_RETRIES 56
#define CONFD_NCS_DEV_AUTH_FAILED 1
#define CONFD_NCS_NO_EXISTS 2
#define CONFD_NCS_LOCKED 3
#define CONFD_NCS_SOUTHBOUND_LOCKED 4
#define CONFD_NCS_CONNECTION_REFUSED 5
#define CONFD_NCS_NED_EXTERNAL_ERROR 6
#define CONFD_NCS_NED_INTERNAL_ERROR 7
#define CONFD_NCS_CONNECTION_TIMEOUT 8
#define CONFD_NCS_RPC_ERROR 9
#define CONFD_NCS_CONNECTION_CLOSED 10
#define CONFD_NCS_XML_PARSE 11
#define CONFD_NCS_HOST_LOOKUP 12
#define CONFD_NCS_NS_SUPPORT 13
#define CONFD_NCS_BAD_CAPAS 14
#define CONFD_NCS_NONED 15
#define CONFD_NCS_NO_YANG_MODULES 16
#define CONFD_NCS_REVDROP 17
#define CONFD_NCS_NED_OUT_OF_SYNC 18
#define CONFD_NCS_CLI_LOAD 19
#define CONFD_NCS_COMMIT_QUEUED 20
#define CONFD_NCS_CQ_BLOCK_OTHERS 21
#define CONFD_NCS_CQ_REMOTE_NOT_ENABLED 22
#define CONFD_NCS_NCS_DUPLICATE_TEMPLATE 23
#define CONFD_NCS_NCS_NO_TEMPLATE_XML 24
#define CONFD_NCS_NCS_XPATH_COMPILE_XML 25
#define CONFD_NCS_NCS_XPATH_XML 26
#define CONFD_NCS_NCS_TEMPLATE_WHEN_XML 27
#define CONFD_NCS_NCS_TEMPLATE_WHEN_KEY_XML 28
#define CONFD_NCS_NCS_TEMPLATE_FOREACH_XML 29
#define CONFD_NCS_NCS_LOAD_TEMPLATE_MISSING_ELEMENT_XML 30
#define CONFD_NCS_NCS_LOAD_TEMPLATE_INVALID_VALUE_XML 31
#define CONFD_NCS_NCS_LOAD_TEMPLATE_UNKNOWN_ATTRIBUTE_XML 32
#define CONFD_NCS_NCS_LOAD_TEMPLATE_TRAILING_XML 33
#define CONFD_NCS_NCS_LOAD_TEMPLATE_EOF_XML 34
#define CONFD_NCS_NCS_LOAD_TEMPLATE_UNKNOWN_SP_XML 35
#define CONFD_NCS_NCS_LOAD_TEMPLATE_MULTIPLE_SP_XML 36
#define CONFD_NCS_NCS_LOAD_TEMPLATE_UNKNOWN_ELEMENT_XML 37
#define CONFD_NCS_NCS_LOAD_TEMPLATE_UNKNOWN_NS_XML 38
#define CONFD_NCS_NCS_LOAD_TEMPLATE_MULTIPLE_ELEMENTS_XML 39
#define CONFD_NCS_NCS_TEMPLATE_ORDERED_LIST_XML 40
#define CONFD_NCS_NCS_TEMPLATE_INSERT_XML 41
#define CONFD_NCS_NCS_TEMPLATE_MISSING_VALUE_XML 42
#define CONFD_NCS_NCS_TEMPLATE_LONE_GUARD_XML 43
#define CONFD_NCS_NCS_TEMPLATE_VALUE_LENGTH_XML 44
#define CONFD_NCS_NCS_TEMPLATE_GUARD_LENGTH_XML 45
#define CONFD_NCS_NCS_TEMPLATE_MOVE_XML 46
#define CONFD_NCS_NCS_TEMPLATE_STR2VAL_XML 47
#define CONFD_NCS_NCS_NO_TEMPLATE 48
#define CONFD_NCS_NCS_NO_SP_TEMPLATE 49
#define CONFD_NCS_NCS_XPATH_COMPILE 50
#define CONFD_NCS_NCS_TEMPLATE_WHEN 51
#define CONFD_NCS_NCS_TEMPLATE_FOREACH 52
#define CONFD_NCS_NCS_MISSING_VARIABLES 53
#define CONFD_NCS_NCS_XPATH 54
#define CONFD_NCS_NCS_TEMPLATE_ORDERED_LIST 55
#define CONFD_NCS_NCS_TEMPLATE_INSERT 56
#define CONFD_NCS_NCS_TEMPLATE_MISSING_VALUE 57
#define CONFD_NCS_NCS_TEMPLATE_LONE_GUARD 58
#define CONFD_NCS_NCS_TEMPLATE_VALUE_LENGTH 59
#define CONFD_NCS_NCS_TEMPLATE_GUARD_LENGTH 60
#define CONFD_NCS_NCS_TEMPLATE_MOVE 61
#define CONFD_NCS_NCS_TEMPLATE_STR2VAL 62
#define CONFD_NCS_NO_TEMPLATE 63
#define CONFD_NCS_NCS_XPATH_VARBIND 64
#define CONFD_NCS_NCS_NO_CAPABILITIES 65
#define CONFD_NCS_NCS_NO_NAMESPACE 66
#define CONFD_NCS_NCS_NO_WRITE_TRANSACTION 67
#define CONFD_NCS_NCS_ACTION_NO_TRANSACTION 68
#define CONFD_NCS_NCS_ERROR 69
#define CONFD_NCS_NCS_DEV_ERROR 70
#define CONFD_NCS_NCS_ERROR_IKP 71
#define CONFD_NCS_NCS_NO_DIFF 72
#define CONFD_NCS_NCS_NO_FORWARD_DIFF 73
#define CONFD_NCS_NCS_MISSING_CLUSTER_AUTH 74
#define CONFD_NCS_NCS_CLUSTER_AUTH_FAILED 75
#define CONFD_NCS_COMMIT_QUEUE_HAS_SENTINEL 76
#define CONFD_NCS_NCS_NED_MULTI_ERROR 77
#define CONFD_NCS_NCS_LOAD_TEMPLATE_MULTIPLE_KEY_LEAFS_XML 78
#define CONFD_NCS_NCS_SERVICE_CONFLICT 79
#define CONFD_NCS_NCS_TEMPLATE_NOT_CREATED_XML 80
#define CONFD_NCS_NCS_TEMPLATE_NOT_CREATED 81
#define CONFD_NCS_DEV_IN_USE 82
#define CONFD_NCS_NCS_ALREADY_EXISTS 83
#define CONFD_NCS_NCS_OPERATION_LOCKED 84
#define CONFD_NCS_CONFIG_LOCKED 85
#define CONFD_NCS_NCS_LOAD_TEMPLATE_UNKNOWN_PI 86
#define CONFD_NCS_NCS_LOAD_TEMPLATE_INVALID_PI_SYNTAX 87
#define CONFD_NCS_NCS_LOAD_TEMPLATE_UNMATCHED_PI 88
#define CONFD_NCS_NCS_LOAD_TEMPLATE_UNCLOSED_PI 89
#define CONFD_NCS_NCS_LOAD_TEMPLATE_UNEXPECTED_PI 90
#define CONFD_NCS_NCS_TEMPLATE_CONTEXT_NODE_NOEXISTS 91
#define CONFD_NCS_NCS_TEMPLATE_SAVED_CONTEXT_NOEXISTS 92
#define CONFD_NCS_NCS_TEMPLATE_MULTIPLE_CONTEXT_NODES 93
#define CONFD_NCS_NCS_TEMPLATE_ROOT_LEAF_LIST 94
#define CONFD_NCS_NCS_TEMPLATE_COPY_TREE_BAD_OP 95
#define CONFD_NCS_NCS_TEMPLATE_LOOP_PREVENTION 96
#define CONFD_NCS_NCS_LOAD_TEMPLATE_COPY_TREE_CROSS_NS 97
#define CONFD_NCS_NCS_LOAD_TEMPLATE_UNKNOWN_ELEMENT2_XML 98
#define CONFD_NCS_NCS_LOAD_TEMPLATE_MISSING_ELEMENT2_XML 99
#define CONFD_NCS_NCS_LOAD_TEMPLATE_UNKNOWN_NED_ID_XML 100
#define CONFD_NCS_NCS_LOAD_TEMPLATE_UNSUPPORTED_NED_ID_XML 101
#define CONFD_NCS_NCS_LOAD_TEMPLATE_UNSUPPORTED_NED_ID_AT_TAG_XML 102
#define CONFD_NCS_NCS_LOAD_TEMPLATE_TAG_AMBIGUOUS_XML 103
#define CONFD_NCS_COMMIT_QUEUE_HAS_OVERLAPPING 104
#define CONFD_NCS_BAD_AUTHGROUP_CALLBACK_RESPONSE 105
#define CONFD_NCS_UNKNOWN_NED_ID 106
#define CONFD_NCS_UNKNOWN_NED_ID_DEVICE_TEMPLATE 107
#define CONFD_NCS_CALL_HOME 108
#define CONFD_NCS_NED_OFFLINE_UNAVAILABLE 109
#define CONFD_NCS_NCS_LOAD_TEMPLATE_SHADOWED_NED_ID_XML 110
#define CONFD_NCS_NCS_LOAD_TEMPLATE_MISPLACED_IF_NED_ID_XML 111
#define CONFD_NCS_COMMIT_QUEUE_DISABLED 112
#define CONFD_NCS_NCS_TEMPLATE_UNSUPPORTED_NED_ID 113
#define CONFD_NCS_COMMIT_QUEUED_AND_DELETED 114
#define CONFD_NCS_YANGLIB_NO_SCHEMA_FOR_RUNNING 115
#define CONFD_NCS_SESSION_LIMIT_EXCEEDED 116
#define CONFD_NCS_NCS_LOAD_TEMPLATE_UNKNOWN_MACRO 117
#define CONFD_NCS_NCS_LOAD_TEMPLATE_MISSING_MACRO_VARS 118
#define CONFD_NCS_NCS_LOAD_TEMPLATE_EXTRA_MACRO_VARS 119
#define CONFD_NCS_NCS_LOAD_TEMPLATE_DUPLICATE_MACRO 120
#define CONFD_NCS_PLAN_LOCATION 121
#define CONFD_NCS_NCS_LOAD_TEMPLATE_MISPLACED_IF_NED_ID_MATCH_XML 122
#define CONFD_NCS_NCS_LOAD_TEMPLATE_INVALID_PI_REGEX 123
#define CONFD_NCS_NCS_PACKAGE_SYNC_MISMATCHED_LOAD_PATH 124
#define CONFD_NCS_UNKNOWN_NED_IDS_COMPLIANCE_TEMPLATE 125
#define CONFD_NCS_CONFLICTING_INTENT 126
#define CONFD_NCS_NCS_LOAD_TEMPLATE_UNSUPPORTED_NETCONF_YANG_ATTRIBUTES 127
#define CONFD_NCS_OVERLAPPING_PRESENCE_AND_ABSENCE_ASSERTION_COMPLIANCE_TEMPLATE 128
#define CONFD_OPERATION_CASE_EXISTS 13

#endif
