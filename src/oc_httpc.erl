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

%Type exports
-export_type([
         response/0
        ]).


-define(DEFAULT_SINGLE_REQUEST_TIMEOUT, 30000).
-define(DEFAULT_MULTI_REQUEST_TIMEOUT, 30000).
%% @doc Issue request to available pid in named pool.  Request specifies; the endpoint from
%% the root url, the headers and the method.  The body and timeout will be defaulted.
-spec request(atom(), string(), headerList(), method()) ->
                     response().
request(PoolName, Endpoint, Headers, Method) ->
    request(PoolName, Endpoint, Headers, Method, []).

%% @doc Issue request to available pid in named pool.  Request specifies; the endpoint from
%% the root url, the headers, the method and the body.  The timeout will be defaulted.
-spec request(atom(), string(), headerList(), method(), body()) ->
                     response().
request(PoolName, Endpoint, Headers, Method, Body) ->
    request(PoolName, Endpoint, Headers, Method, Body, ?DEFAULT_SINGLE_REQUEST_TIMEOUT).

%% @doc Issue request to available pid in named pool.  Request specifies; the endpoint from
%% the root url, the headers, the method, the body and the timeout.
-spec request(atom(), string(), headerList(), method(), body(), non_neg_integer()) ->
                     response().
request(PoolName, Endpoint, Headers, Method, Body, Timeout) ->
    take_and_execute(PoolName, fun(Pid) ->
                   oc_httpc_worker:request(Pid, Endpoint, Headers, Method, Body, Timeout)
               end).

%% @doc Sets up a multi_request in named pool.  The fun passed in a single arity fun that
%% will be passed an arity four fun.  The arity four fun is
%% RequestFun(Path, Headers, Method, Body).  This RequestFun can be used to issue multiple
%% requests on the same connection.  The result of the RequestFun will be returned on each
%% invocation.
-spec multi_request(atom(), fun(), non_neg_integer()) ->
                     any().
multi_request(PoolName, Fun, Timeout) ->
    take_and_execute(PoolName, fun(Pid) ->
                   oc_httpc_worker:multi_request(Pid, Fun, Timeout)
               end).

%% @doc Creates a pool with pool_name, which is an atom.  The config must have the following
%% values in it as a proplist; {root_url, string()}, {init_count, non_neg_integer()},
%% {max_count, non_neg_integer}.  Ibrowse specific configuration can be passed as
%% {ibrowse_options, [options]}.  Additional pooler configuration can be passed as
%% documented in the pooler application https://github.com/seth/pooler
-spec add_pool(atom(), pool_config()) -> any().
add_pool(PoolName, Config)  ->
    RootUrl =proplists:get_value(root_url, Config),
    Options = proplists:get_value(ibrowse_options, Config, []),
    UpdatedOptions = update_ibrowse_options(Options, Config),
    PoolConfig = [{name, PoolName},
                  {start_mfa, {oc_httpc_worker, start_link, [RootUrl, UpdatedOptions, Config]}}
                  | Config],
    {ok, _} = pooler:new_pool(PoolConfig).

%% @doc Delete the named pool
-spec delete_pool(atom()) -> ok | {error, term()}.
delete_pool(PoolName) ->
    pooler:rm_pool(PoolName).

update_ibrowse_options(Options, Config) ->
    UpdatedOptions = case proplists:is_defined(response_format, Options) of
        true ->
            Options;
        false ->
            [{response_format,binary} | Options]
    end,
    {Val, Unit} = proplists:get_value(max_connection_duration, Config, {1, min}),
    [{inactivity_timeout, oc_time:convert_units({Val * 2, Unit}, ms)} |
     UpdatedOptions].

take_and_execute(PoolName, Fun) ->
    case pooler:take_member(PoolName) of
        error_no_members ->
            {error, no_members};
        Pid when is_pid(Pid) ->
            Result = Fun(Pid),
            pooler:return_member(PoolName, Pid),
            Result
    end.
