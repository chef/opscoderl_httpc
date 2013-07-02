-module(oc_httpc).

-export([
    request/3,
    request/4,
    request/5,
    request/6,
    add_pool/2
  ]).

-include_lib("ibrowse/include/ibrowse.hrl").

-define(DEFAULT_SINGLE_REQUEST_TIMEOUT, 30000).
-define(DEFAULT_MULTI_REQUEST_TIMEOUT, 30000).

request(Url, Headers, Method) ->
  request(Url, Headers, Method, [], []).

request(Url, Headers, Method, Body) ->
    request(Url, Headers, Method, Body, []).

request(Url, Headers, Method, Body, Options) ->
    request(Url, Headers, Method, Body, Options, ?DEFAULT_SINGLE_REQUEST_TIMEOUT).

-spec request(string(), oc_httpc_types:headerList(), oc_httpc_types:method(), oc_httpc_types:body(),[oc_httpc_types:ibrowse_option()], non_neg_integer()) -> oc_httpc_types:response().
request(UrlString, Headers, Method, Body, Options, Timeout) ->
    Url = #url{host = Host, port = Port, protocol = Protocol} = ibrowse_lib:parse_url(UrlString),
    {SSLOptions, IsSSL} =
        case (Protocol == https) orelse
            ibrowse_lib:get_value(is_ssl, Options, false) of
            false -> {[], false};
            true -> {ibrowse_lib:get_value(ssl_options, Options, []), true}
       end,
    PoolName = list_to_atom(Host ++ integer_to_list(Port)),
    PoolConfig = [{name, PoolName}, {init_count, 50}, {max_count, 250},
      {start_mfa, {ibrowse_http_client, start_link, [{undefined, Url, {SSLOptions, IsSSL}}]}}],
    pooler:new_pool(PoolConfig),
    Pid = pooler:take_member(PoolName),
    Result = ibrowse:send_req_direct(Pid, UrlString, Headers, Method, Body, Options, Timeout),
    pooler:return_member(PoolName, Pid),
    Result.

-spec add_pool(oc_httpc_types:pool_name(), oc_httpc_types:pool_config()) -> any().
add_pool(_Name, _Config)  ->
  ok.
