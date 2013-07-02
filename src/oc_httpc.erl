-module(oc_httpc).

-export([
    request/4,
    request/5,
    request/6,
    request/7,
    add_pool/2,
    delete_pool/1
  ]).

-include_lib("ibrowse/include/ibrowse.hrl").

-define(DEFAULT_SINGLE_REQUEST_TIMEOUT, 30000).
-define(DEFAULT_MULTI_REQUEST_TIMEOUT, 30000).

request(PoolName, Url, Headers, Method) ->
  request(PoolName, Url, Headers, Method, [], []).

request(PoolName, Url, Headers, Method, Body) ->
    request(PoolName, Url, Headers, Method, Body, []).

request(PoolName, Url, Headers, Method, Body, Options) ->
    request(PoolName, Url, Headers, Method, Body, Options, ?DEFAULT_SINGLE_REQUEST_TIMEOUT).

-spec request(string(), oc_httpc_types:headerList(), oc_httpc_types:method(), oc_https_types:body(),[oc_https_types:ibrowse_option()], non_neg_integer()) -> oc_httpc_types:response().
request(PoolName, UrlString, Headers, Method, Body, Options, Timeout) ->
    Pid = pooler:take_member(PoolName),
    Result = ibrowse:send_req_direct(Pid, UrlString, Headers, Method, Body, Options, Timeout),
    pooler:return_member(PoolName, Pid),
    Result.

-spec add_pool(oc_httpc_types:pool_name(), oc_httpc_types:pool_config()) -> any().
add_pool(PoolName, Config)  ->
    Url = #url{protocol = Protocol} = ibrowse_lib:parse_url(proplists:get_value(root_url, Config)),
    Options = proplists:get_value(ibrowse_options, Config, []),
    {SSLOptions, IsSSL} =
        case (Protocol == https) orelse
            ibrowse_lib:get_value(is_ssl, Options, false) of
            false -> {[], false};
            true -> {ibrowse_lib:get_value(ssl_options, Options, []), true}
       end,
    PoolConfig = [
      {name, PoolName},
      {start_mfa,
        {ibrowse_http_client,
          start_link,
          [{undefined, Url, {SSLOptions, IsSSL}}]
        }
      } | Config],
		{ok, _} = pooler:new_pool(PoolConfig).

delete_pool(PoolName) ->
  pooler:rm_pool(PoolName).
