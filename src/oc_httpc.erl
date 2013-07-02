-module(oc_httpc).

-export([
    request/4,
    request/5,
    request/6,
    add_pool/2,
    delete_pool/1
  ]).


-define(DEFAULT_SINGLE_REQUEST_TIMEOUT, 30000).
-define(DEFAULT_MULTI_REQUEST_TIMEOUT, 30000).

request(PoolName, Endpoint, Headers, Method) ->
  request(PoolName, Endpoint, Headers, Method, []).

request(PoolName, Endpoint, Headers, Method, Body) ->
    request(PoolName, Endpoint, Headers, Method, Body, ?DEFAULT_SINGLE_REQUEST_TIMEOUT).

-spec request(string(), oc_httpc_types:headerList(), oc_httpc_types:method(), oc_https_types:body(),[oc_https_types:ibrowse_option()], non_neg_integer()) -> oc_httpc_types:response().
request(PoolName, Endpoint, Headers, Method, Body, Timeout) ->
    Pid = pooler:take_member(PoolName),
    Result = oc_httpc_worker:request(Pid, Endpoint, Headers, Method, Body, Timeout),
    pooler:return_member(PoolName, Pid),
    Result.

-spec add_pool(oc_httpc_types:pool_name(), oc_httpc_types:pool_config()) -> any().
add_pool(PoolName, Config)  ->
    RootUrl =proplists:get_value(root_url, Config),
    Options = proplists:get_value(ibrowse_options, Config, []),
    PoolConfig = [
      {name, PoolName},
      {start_mfa,
        {oc_httpc_worker,
          start_link,
          [RootUrl, Options]
        }
      } | Config],
		{ok, _} = pooler:new_pool(PoolConfig).

delete_pool(PoolName) ->
  pooler:rm_pool(PoolName).
