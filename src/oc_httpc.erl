%% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 92 -*-
%% ex: ts=4 sw=4 et
%% @author Oliver Ferrigni <oliver@opscode.com>
%% Copyright 2013 Opscode, Inc. All Rights Reserved.
%%
%% This file is provided to you under the Apache License,
%% Version 2.0 (the "License"); you may not use this file
%% except in compliance with the License.  You may obtain
%% a copy of the License at
%%
%%   http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing,
%% software distributed under the License is distributed on an
%% "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
%% KIND, either express or implied.  See the License for the
%% specific language governing permissions and limitations
%% under the License.
%%

-module(oc_httpc).
-include("oc_httpc_types.hrl").
-export([
         request/4,
         request/5,
         request/6,
         multi_request/3,
         add_pool/2,
         delete_pool/1,
         get_authz_pool_info/0
        ]).

%Type exports
-export_type([
         response/0
        ]).

-ifdef(TEST).
-compile([export_all]).
-endif.

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
-spec request(atom(), string(), headerList(), method(), body(), timeout()) ->
                     response().
request(PoolName, Endpoint, Headers, Method, Body, _Timeout) ->
    take_and_execute(PoolName, fun(Pid) ->
                   oc_httpc_worker:request(Pid, Endpoint, Headers, Method, Body, ?DEFAULT_SINGLE_REQUEST_TIMEOUT)
               end).

%% @doc Sets up a multi_request in named pool.  The fun passed in a single arity fun that
%% will be passed an arity four fun.  The arity four fun is
%% RequestFun(Path, Headers, Method, Body).  This RequestFun can be used to issue multiple
%% requests on the same connection.  The result of the RequestFun will be returned on each
%% invocation.
-spec multi_request(atom(), fun(), non_neg_integer()) ->
                     any().
multi_request(PoolName, Fun, _Timeout) ->
    take_and_execute(PoolName, fun(Pid) ->
                   oc_httpc_worker:multi_request(Pid, Fun, ?DEFAULT_MULTI_REQUEST_TIMEOUT)
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
    enforce_inactivity_timeout(UpdatedOptions),
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
    case proplists:is_defined(inactivity_timeout, UpdatedOptions) of
        true ->
            UpdatedOptions;
        false ->
            {Val, Unit} = proplists:get_value(max_connection_duration, Config, {1, min}),
            [{inactivity_timeout, oc_time:convert_units({Val * 2, Unit}, ms)} |
            UpdatedOptions]
    end.
take_and_execute(PoolName, Fun) ->
    case pooler:take_member(PoolName, get_pooler_timeout()) of
        error_no_members ->
            {error, no_members};
        Pid when is_pid(Pid) ->
            Result = Fun(Pid),
            pooler:return_member(PoolName, Pid),
            Result
    end.

enforce_inactivity_timeout(Options) ->
    InactivityTimeout = proplists:get_value(inactivity_timeout, Options, 0),
    SetInactivityTimeout = ibrowse:get_config_value(inactivity_timeout, 0),
    case InactivityTimeout > SetInactivityTimeout of
        true ->
            ok = gen_server:call(ibrowse, {set_config_value, inactivity_timeout, InactivityTimeout});
        false  ->
            no_op
    end.

-spec get_pooler_timeout() -> pooler:time_spec().
get_pooler_timeout() ->
    case application:get_env(opscoderl_httpc, pooler_timeout) of
        undefined ->
            0;
        {ok, Val} when is_integer(Val) andalso Val >= 0 ->
            Val;
        {ok, {Val, _} = TimeTuple} when is_integer(Val) andalso Val >= 0 ->
            TimeTuple;
        {ok, Other} ->
            exit({invalid_pooler_timeout, Other})
    end.

get_authz_pool_info() ->
    [erlang:process_info(X) || 
        {_,X,_,_} <- supervisor:which_children(erlang:whereis(pooler_oc_chef_authz_http_member_sup))].
