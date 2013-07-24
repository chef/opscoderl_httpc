-module(oc_httpc).
-include("oc_httpc_types.hrl").
-export([
         request/4,
         request/5,
         request/6,
         multi_request/3,
         add_pool/2,
         delete_pool/1
        ]).


-define(DEFAULT_SINGLE_REQUEST_TIMEOUT, 30000).
-define(DEFAULT_MULTI_REQUEST_TIMEOUT, 30000).

request(PoolName, Endpoint, Headers, Method) ->
    request(PoolName, Endpoint, Headers, Method, []).

request(PoolName, Endpoint, Headers, Method, Body) ->
    request(PoolName, Endpoint, Headers, Method, Body, ?DEFAULT_SINGLE_REQUEST_TIMEOUT).

multi_request(PoolName, Fun, Timeout) ->
    Pid = pooler:take_member(PoolName),
    Result = oc_httpc_worker:multi_request(Pid, Fun, Timeout),
    pooler:return_member(PoolName, Pid),
    Result.

-spec request(pool_name(), string(), headerList(), method(), body(), non_neg_integer()) -> response().
request(PoolName, Endpoint, Headers, Method, Body, Timeout) ->
    Pid = pooler:take_member(PoolName),
    Result = oc_httpc_worker:request(Pid, Endpoint, Headers, Method, Body, Timeout),
    pooler:return_member(PoolName, Pid),
    Result.

-spec add_pool(pool_name(), pool_config()) -> any().
add_pool(PoolName, Config)  ->
    RootUrl =proplists:get_value(root_url, Config),
    Options = proplists:get_value(ibrowse_options, Config, []),
    UpdatedOptions = update_response_format(Options),
    PoolConfig = [{name, PoolName},
                  {start_mfa, {oc_httpc_worker, start_link, [RootUrl, UpdatedOptions]}}
                  | Config],
    {ok, _} = pooler:new_pool(PoolConfig).

delete_pool(PoolName) ->
    pooler:rm_pool(PoolName).

update_response_format(Options) ->
    case proplists:is_defined(response_format, Options) of
        true ->
            Options;
        false ->
            [{response_format,binary} | Options]
    end.
