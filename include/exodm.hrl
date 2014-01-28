%%% @author Malotte W Lönne <malotte@malotte.net>
%%% @copyright (C) 2013, Tony Rogvall
%%% @doc
%%%     Exosense db definitions
%%% @end
%%% Created :  2013 Malotte W Lönne <malotte@malotte.net>

%% exodm admin
-define(EXODM, <<"exodm">>). 
-define(EXODM_ADMIN, <<"exodm-admin">>).

%% Predefined roles
-define(ROOT, <<"root">>).
-define(INIT_ADMIN, <<"initial-admin">>).
-define(ADMIN, <<"admin">>).
-define(EXEC, <<"execute">>).
-define(CONFIG, <<"config">>).
-define(VIEW, <<"view">>).

%% Result codes
-define(PERMISSION_DENIED, 'permission-denied').
-define(VALIDATION_FAILED, 'validation-failed').
-define(OBJECT_EXISTS, 'object-exists').
-define(DEVICE_NOT_FOUND, 'device-not-found').
-define(DEVICE_ERROR, 'device-error').
-define(OBJECT_NOT_FOUND, 'object-not-found').
-define(OBJECT_NOT_EMPTY, 'object-not-empty').
-define(ACCOUNT_NOT_SPECIFIED, 'account-not-specified').

%%--------------------------------------------------------------------
%% Predefined rpc:s - must correspond to yang-files!!!
%% !!! When adding an rpc make sure you put the correct
%% !!! permissions in the sections below
%%--------------------------------------------------------------------
-define(RPC_CREATE_ACCOUNT, <<"create-account">>).
-define(RPC_UPDATE_ACCOUNT,  <<"update-account">>).
-define(RPC_DELETE_ACCOUNT,  <<"delete-account">>).
-define(RPC_LOOKUP_ACCOUNT,  <<"lookup-account">>).
-define(RPC_LIST_ACCOUNTS,  <<"list-accounts">>).
-define(RPC_ADD_ACCOUNT_USERS,  <<"add-account-users">>).
-define(RPC_REMOVE_ACCOUNT_USERS,  <<"remove-account-users">>).
-define(RPC_REMOVE_ACCOUNT_USER,  <<"remove-account-user">>).
-define(RPC_LIST_ACCOUNT_USERS,  <<"list-account-users">>).
-define(RPC_LIST_ACCOUNT_ROLES,  <<"list-account-roles">>).

-define(RPC_CREATE_USER,  <<"create-user">>).
-define(RPC_DELETE_USER,  <<"delete-user">>). 
-define(RPC_LOOKUP_USER,  <<"lookup-user">>).
-define(RPC_LIST_USERS,  <<"list-users">>).
-define(RPC_LIST_USER_ACCOUNTS, <<"list-user-accounts">>).

-define(RPC_CREATE_CONFIG_SET, <<"create-config-set">>).
-define(RPC_UPDATE_CONFIG_SET, <<"update-config-set">>). 
-define(RPC_READ_CONFIG_SET_DATA, <<"read-config-set-data">>). 
-define(RPC_DELETE_CONFIG_SET, <<"delete-config-set">>).
-define(RPC_LIST_CONFIG_SETS, <<"list-config-sets">>).
-define(RPC_LIST_CONFIG_SET_MEMBERS, <<"list-config-set-members">>).
-define(RPC_ADD_CONFIG_SET_MEMBERS, <<"add-config-set-members">>).
-define(RPC_REMOVE_CONFIG_SET_MEMBERS, <<"remove-config-set-members">>).
-define(RPC_PUSH_CONFIG_SET, <<"push-config-set">>).
-define(RPC_ADD_NOTIFICATION_URLS, <<"add-notification-urls">>).
-define(RPC_REMOVE_NOTIFICATION_URLS, <<"remove-notification-urls">>).

-define(RPC_CREATE_YANG_MODULE, <<"create-yang-module">>).
-define(RPC_DELETE_YANG_MODULE, <<"delete-yang-module">>).
-define(RPC_LOOKUP_YANG_MODULE, <<"lookup-yang-module">>).
-define(RPC_LIST_YANG_MODULES, <<"list-yang-modules">>).
-define(RPC_LIST_EXEC_PERMISSION, <<"list-execution-permission">>).

-define(RPC_CREATE_DEVICE_TYPE, <<"create-device-type">>). 
-define(RPC_UPDATE_DEVICE_TYPE, <<"update-device-type">>).
-define(RPC_DELETE_DEVICE_TYPE, <<"delete-device-type">>).
-define(RPC_LIST_DEVICE_TYPES, <<"list-device-types">>).
-define(RPC_LIST_DEVICE_TYPE_MEMBERS, <<"list-device-type-members">>).

-define(RPC_CREATE_DEVICE_GROUP, <<"create-device-group">>).
-define(RPC_UPDATE_DEVICE_GROUP, <<"update-device-group">>).
-define(RPC_DELETE_DEVICE_GROUP, <<"delete-device-group">>).
-define(RPC_ADD_DEVICE_GROUP_MEMBERS, <<"add-device-group-members">>).
-define(RPC_REMOVE_DEVICE_GROUP_MEMBERS, <<"remove-device-group-members">>).
-define(RPC_LIST_DEVICE_GROUPS, <<"list-device-groups">>).
-define(RPC_LIST_DEVICE_GROUP_MEMBERS, <<"list-device-group-members">>).

-define(RPC_PROVISION_DEVICE, <<"create-device">>).
-define(RPC_LOOKUP_DEVICE, <<"lookup-device">>).
-define(RPC_LOOKUP_DEVICE_ATTRIBUTES, <<"lookup-device-attributes">>).
-define(RPC_UPDATE_DEVICE, <<"update-device">>).
-define(RPC_DEPROVISION_DEVICES, <<"delete-devices">>).
-define(RPC_LIST_DEVICES, <<"list-devices">>).

-define(RPC_CREATE_PACKAGE, <<"create-package">>).
-define(RPC_ADD_PACKAGE_MEMBERS, <<"add-package-members">>).
-define(RPC_PUSH_PACKAGE, <<"push-package">>).
-define(RPC_LIST_PACKAGES, <<"list-packages">>).
-define(RPC_GET_PACKAGE_STATUS, <<"get-package-status">>).

-define(RPC_SET_MBLOX_PARAMETERS, <<"set-mblox-parameters">>).
-define(RPC_GET_MBLOX_PARAMETERS, <<"get-mblox-parameters">>).
-define(RPC_DELETE_MBLOX_PARAMETERS, <<"delete-mblox-parameters">>).

%%--------------------------------------------------------------------
%% Defines wich rpc:s a role can execute
%%--------------------------------------------------------------------
-define(ROOT_ACCESS_RPCS,
    [?RPC_CREATE_ACCOUNT, 
     ?RPC_UPDATE_ACCOUNT,
     ?RPC_DELETE_ACCOUNT, 
     ?RPC_LIST_ACCOUNTS]).

-define(RPC_ROLE_LIST,
        [{?RPC_CREATE_ACCOUNT, [?ROOT]},
         {?RPC_UPDATE_ACCOUNT, [?ROOT]},
         {?RPC_DELETE_ACCOUNT, [?ROOT]},
         {?RPC_LIST_ACCOUNTS, [?ROOT]},
         {?RPC_LOOKUP_ACCOUNT, [?ROOT, ?INIT_ADMIN, ?ADMIN]},
         {?RPC_ADD_ACCOUNT_USERS, [?ROOT, ?INIT_ADMIN, ?ADMIN]},
         {?RPC_REMOVE_ACCOUNT_USERS, [?ROOT, ?INIT_ADMIN, ?ADMIN]},
         {?RPC_REMOVE_ACCOUNT_USER, [?ROOT, ?INIT_ADMIN, ?ADMIN]},
         {?RPC_LIST_ACCOUNT_USERS, [?ROOT, ?INIT_ADMIN, ?ADMIN]},
         {?RPC_LIST_ACCOUNT_ROLES, [?ROOT, ?INIT_ADMIN, ?ADMIN]},

         {?RPC_CREATE_USER, [?ROOT, ?INIT_ADMIN, ?ADMIN]},
         {?RPC_DELETE_USER, [?ROOT, ?INIT_ADMIN, ?ADMIN]},
         {?RPC_LOOKUP_USER, [?ROOT, ?INIT_ADMIN, ?ADMIN]},
         {?RPC_LIST_USERS, [?ROOT, ?INIT_ADMIN, ?ADMIN]},
	 {?RPC_LIST_USER_ACCOUNTS, [?ROOT, ?INIT_ADMIN, ?ADMIN]},
	 
         {?RPC_CREATE_CONFIG_SET, 
          [?ROOT, ?INIT_ADMIN, ?ADMIN, ?CONFIG]},
         {?RPC_UPDATE_CONFIG_SET, 
          [?ROOT, ?INIT_ADMIN, ?ADMIN, ?CONFIG]},
         {?RPC_READ_CONFIG_SET_DATA,
          [?ROOT, ?INIT_ADMIN, ?ADMIN, ?CONFIG]},
         {?RPC_DELETE_CONFIG_SET, 
          [?ROOT, ?INIT_ADMIN, ?ADMIN, ?CONFIG]},
         {?RPC_LIST_CONFIG_SETS, 
          [?ROOT, ?INIT_ADMIN, ?ADMIN, ?CONFIG, ?VIEW]},
         {?RPC_LIST_CONFIG_SET_MEMBERS, 
          [?ROOT, ?INIT_ADMIN, ?ADMIN, ?CONFIG , ?VIEW]},
         {?RPC_ADD_CONFIG_SET_MEMBERS, 
          [?ROOT, ?INIT_ADMIN, ?ADMIN, ?CONFIG]},
         {?RPC_REMOVE_CONFIG_SET_MEMBERS, 
          [?ROOT, ?INIT_ADMIN, ?ADMIN, ?CONFIG]},
         {?RPC_PUSH_CONFIG_SET, 
          [?ROOT, ?INIT_ADMIN, ?ADMIN, ?CONFIG]},
	 {?RPC_ADD_NOTIFICATION_URLS,
	  [?ROOT, ?INIT_ADMIN, ?ADMIN, ?CONFIG]},
	 {?RPC_REMOVE_NOTIFICATION_URLS,
	  [?ROOT, ?INIT_ADMIN, ?ADMIN, ?CONFIG]},

         {?RPC_CREATE_YANG_MODULE, [?ROOT, ?INIT_ADMIN, ?ADMIN]},
         {?RPC_DELETE_YANG_MODULE, [?ROOT, ?INIT_ADMIN, ?ADMIN]},
         {?RPC_LIST_YANG_MODULES, [?ROOT, ?INIT_ADMIN, ?ADMIN, ?VIEW]},
         {?RPC_LOOKUP_YANG_MODULE, [?ROOT, ?INIT_ADMIN, ?ADMIN, ?VIEW]},
         {?RPC_LIST_EXEC_PERMISSION, [?ROOT, ?INIT_ADMIN, ?ADMIN, ?VIEW]},

         {?RPC_CREATE_DEVICE_TYPE, 
          [?ROOT, ?INIT_ADMIN, ?ADMIN]},
         {?RPC_UPDATE_DEVICE_TYPE, 
          [?ROOT, ?INIT_ADMIN, ?ADMIN]},
         {?RPC_DELETE_DEVICE_TYPE, 
          [?ROOT, ?INIT_ADMIN, ?ADMIN]},
         {?RPC_LIST_DEVICE_TYPES, 
          [?ROOT, ?INIT_ADMIN, ?ADMIN, ?VIEW]},
         {?RPC_LIST_DEVICE_TYPE_MEMBERS, 
          [?ROOT, ?INIT_ADMIN, ?ADMIN, ?VIEW]},

         {?RPC_CREATE_DEVICE_GROUP, 
          [?ROOT, ?INIT_ADMIN, ?ADMIN]},
         {?RPC_UPDATE_DEVICE_GROUP, 
          [?ROOT, ?INIT_ADMIN, ?ADMIN]},
         {?RPC_DELETE_DEVICE_GROUP, 
          [?ROOT, ?INIT_ADMIN, ?ADMIN]},
         {?RPC_ADD_DEVICE_GROUP_MEMBERS, 
          [?ROOT, ?INIT_ADMIN, ?ADMIN]},
         {?RPC_REMOVE_DEVICE_GROUP_MEMBERS, 
          [?ROOT, ?INIT_ADMIN, ?ADMIN]},
         {?RPC_LIST_DEVICE_GROUPS, 
          [?ROOT, ?INIT_ADMIN, ?ADMIN, ?VIEW]},
         {?RPC_LIST_DEVICE_GROUP_MEMBERS, 
          [?ROOT, ?INIT_ADMIN, ?ADMIN, ?VIEW]},

         {?RPC_PROVISION_DEVICE, 
          [?ROOT, ?INIT_ADMIN, ?ADMIN, ?CONFIG]},
         {?RPC_LOOKUP_DEVICE, 
          [?ROOT, ?INIT_ADMIN, ?ADMIN, ?CONFIG, ?VIEW]},
	 {?RPC_LOOKUP_DEVICE_ATTRIBUTES,
	  [?ROOT, ?INIT_ADMIN, ?ADMIN, ?CONFIG]},
         {?RPC_UPDATE_DEVICE, 
          [?ROOT, ?INIT_ADMIN, ?ADMIN, ?CONFIG]},
         {?RPC_DEPROVISION_DEVICES, 
          [?ROOT, ?INIT_ADMIN, ?ADMIN, ?CONFIG]},
         {?RPC_LIST_DEVICES, 
          [?ROOT, ?INIT_ADMIN, ?ADMIN, ?CONFIG, ?VIEW]},

         {?RPC_CREATE_PACKAGE, 
          [?ROOT, ?INIT_ADMIN, ?ADMIN]},
         {?RPC_ADD_PACKAGE_MEMBERS, 
          [?ROOT, ?INIT_ADMIN, ?ADMIN, ?CONFIG]},
         {?RPC_PUSH_PACKAGE, 
          [?ROOT, ?INIT_ADMIN, ?ADMIN, ?CONFIG]},
         {?RPC_LIST_PACKAGES, 
          [?ROOT, ?INIT_ADMIN, ?ADMIN, ?CONFIG, ?VIEW]},
         {?RPC_GET_PACKAGE_STATUS, 
          [?ROOT, ?INIT_ADMIN, ?ADMIN, ?CONFIG, ?VIEW]},

	 {?RPC_SET_MBLOX_PARAMETERS,
	  [?ROOT, ?INIT_ADMIN, ?ADMIN, ?CONFIG]},
	 {?RPC_GET_MBLOX_PARAMETERS,
	  [?ROOT, ?INIT_ADMIN, ?ADMIN, ?CONFIG]},
	 {?RPC_DELETE_MBLOX_PARAMETERS,
	  [?ROOT, ?INIT_ADMIN, ?ADMIN, ?CONFIG]}
	]).
