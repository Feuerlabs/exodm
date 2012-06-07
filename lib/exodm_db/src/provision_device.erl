%% -*- erlang -*-

-module(provision_device).

-export([main/1]).

main([NodeName, AccountName, GroupName, DeviceID, MSISDN, ClientKey, ServerKey]) ->
    AID = rpc:call(NodeName, exodm_db_account, lookup, [list_to_binary(AccountName)]),
    GID = rpc:call(NodeName, exodm_db_group, lookup, [list_to_binary(GroupName)]),
                       
    rpc:call(NodeName, exodm_db_device, new, [AID, 
                                              list_to_binary(DeviceID), 
                                              [
                                               {'__ck', list_to_binary(ClientKey)},
                                               {'__sk', list_to_binary(ServerKey)},
                                               {msisdn, list_to_binary(MSISDN)},
                                               {group, {1,GID}},
                                               {yang, <<"rfzone.yang">>}
                                              ]]),
   halt(0);

main(_) ->
    io:format("usage: ~s node-name account-name group-name device-id msisdn client-key server-key\n", 
              [ escript:script_name()] ),
    halt(255).

