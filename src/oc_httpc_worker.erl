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

-module(oc_httpc_worker).

-behaviour(gen_server).

%% API
-export([
         start_link/3,
         request/6,
         verify_ca/3,
         multi_request/3
        ]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).


-record(state, {ibrowse_options = [],
                root_url,
                ibrowse_pid,
                current_connection_requests = 0,
                max_connection_requests,
                max_connection_duration, %% value in ms
                retry_on_conn_closed,
                born_on_time}).

-include_lib("ibrowse/include/ibrowse.hrl").

%%%===================================================================
%%% API
%%%===================================================================

start_link(RootUrl, IbrowseOptions, Config) ->
    gen_server:start_link(?MODULE, [RootUrl, IbrowseOptions, Config], []).

request(Pid, Path, Headers, Method, Body, Timeout) when is_atom(Method) ->
    try gen_server:call(Pid, {request, Path, Headers, Method, Body, Timeout}, Timeout) of
        Result ->
            Result
    catch
        exit:{timeout, _} ->
            {error, req_timedout};
        Other ->
            throw(Other)
    end.

multi_request(Pid, Fun, Timeout) ->
    RequestFun = fun(Path, Headers, Method, Body) when is_atom(Method) ->
                         oc_httpc_worker:request(Pid, Path, Headers, Method, Body, Timeout)
                 end,
    Fun(RequestFun).

%%%===================================================================
%%% Gen Server Callbacks
%%%===================================================================
init([RootUrl, IbrowseOptions, Config]) ->
    process_flag(trap_exit, true),
    RetryOnConnClosed = proplists:get_value(retry_on_conn_closed, Config, false),
    MaxRequests = proplists:get_value(max_connection_request_limit, Config, 100),
    MaxConnectionDuration = oc_time:convert_units(proplists:get_value(max_connection_duration, Config, {1, min}), ms),
    #url{host = Host, port = Port} = create_ibrowse_url(RootUrl),
    IbrowseOptions1 = handle_custom_ssl_options(IbrowseOptions),
    ibrowse:add_config([{ignored, ignored}, {dest, Host, Port, 1, 1, IbrowseOptions1}]),
    {ok, #state{root_url = RootUrl, ibrowse_options = IbrowseOptions1, ibrowse_pid = undefined,
                retry_on_conn_closed = RetryOnConnClosed,
                max_connection_requests = MaxRequests,
                max_connection_duration = MaxConnectionDuration}}.

%%% handle_custom_ssl_options currently implements a custom verify mode, `verify_ca` which
%%% ignores hostname mismatches, restoring the behavior of earlier Erlang versions. This is
%%% useful for OTP 19-21 where verify_peer now verifies the hostname but does so naively
%%% without additional configuration. This additional configuration is hard to provide
%%% via a configuration file before OTP 22.
%%%
%%% This is not a stable API and will be removed when we upgrade our ecosystem to Erlang 22.
handle_custom_ssl_options(Options) ->
    case proplists:get_value(ssl_options, Options) of
        undefined ->
            Options;
        SslOpts ->
            case proplists:get_value(verify, SslOpts) of
                verify_ca ->
                    SslOptsNoVerify = proplists:delete(verify, SslOpts),
                    SslNewOpts = [{verify, verify_peer}, {verify_fun, {fun oc_httpc_worker:verify_ca/3, []}} | SslOptsNoVerify],
                    lists:keyreplace(ssl_options, 1, Options, {ssl_options, SslNewOpts});
                _ ->
                    Options
            end
    end.

verify_ca(_Cert, Event, UserState) ->
    case Event of
        {bad_cert, hostname_check_failed} ->
            {valid, UserState};
        {bad_cert, _} ->
            {fail, Event};
        {extension, _} ->
            {unknown, UserState};
        valid ->
            {valid, UserState};
        valid_peer ->
            {valid, UserState}
    end.



handle_call(Request, From, State = #state{ibrowse_pid = undefined}) ->
    handle_call(Request, From, make_http_client_pid(State));
handle_call({request, Path, Headers, Method, Body, Timeout}, _From, State = #state{root_url = RootUrl,
                                                                                   ibrowse_options = IbrowseOptions,
                                                                                   retry_on_conn_closed = RetryOnConnClosed}) ->
    NewState = refresh_connection_process(State),
    ReqUrl = combine(RootUrl, Path),
    lager:info("Devlog - oc_httpc_worker: before request attempt - request path ~p ", [ReqUrl]),
    Result = ibrowse:send_req_direct(NewState#state.ibrowse_pid, ReqUrl, Headers, Method, Body, IbrowseOptions, Timeout),
    lager:info("Devlog - oc_httpc_worker: attempted request - request path ~p - result - ~p", [ReqUrl, Result]),
    case {Result, RetryOnConnClosed} of
        {{error, sel_conn_closed}, true} ->
            lager:info("oc_httpc_worker: attempted request on closed connection (pid = ~p); opening new connection and retrying", [NewState#state.ibrowse_pid]),
            NewState2 = reset_http_client_pid(State),
            RetryResult = ibrowse:send_req_direct(NewState2#state.ibrowse_pid, ReqUrl, Headers, Method, Body, IbrowseOptions, Timeout),
            {reply, RetryResult, NewState2};
        _ ->
            {reply, Result, NewState}
    end;

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({'EXIT', Pid, normal}, State = #state{ibrowse_pid = Pid}) ->
    {noreply, make_http_client_pid(State)};
handle_info({'EXIT', Pid, _Reason}, State = #state{ibrowse_pid=Cur}) when Pid /= Cur ->
    {noreply, State};
handle_info(Message, State) ->
    lager:info("Devlog - oc_httpc_worker: handle info - caught message ~p", [Message]),
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

create_ibrowse_url(RootUrl) ->
    ibrowse_lib:parse_url(RootUrl).

create_ssl_options(#url{protocol = Protocol}, Options) ->
    case (Protocol == https) orelse
        ibrowse_lib:get_value(is_ssl, Options, false) of
        false -> {[], false};
        true -> {ibrowse_lib:get_value(ssl_options, Options, []), true}
    end.

enforce_trailing_slash(S) ->
    Rev = lists:reverse(S),
    case Rev of
        [$/ | _Rest] ->
            S;
        RevNoSlash ->
            lists:reverse([$/ | RevNoSlash])
    end.

enforce_no_leading_slash(S) ->
    case S of
        [$/ | Rest] ->
            enforce_no_leading_slash(Rest);
        S ->
            S
     end.

combine(Root, Path) ->
    enforce_trailing_slash(Root) ++ enforce_no_leading_slash(Path).

refresh_connection_process(State = #state{current_connection_requests = CurrentConnectionRequests,
                                          max_connection_requests = MaxConnectionRequests})
  when CurrentConnectionRequests >= MaxConnectionRequests  ->
    reset_http_client_pid(State);
refresh_connection_process(State = #state{born_on_time = BornOnTime,
                                          max_connection_duration = MaxConnectionDuration,
                                          current_connection_requests = CurrentConnectionRequests}) ->
    Duration = (timer:now_diff(os:timestamp(), BornOnTime)/1000),
    case Duration >= MaxConnectionDuration of
        true ->
            reset_http_client_pid(State);
        false ->
            State#state{current_connection_requests = CurrentConnectionRequests + 1}
     end.

reset_http_client_pid(State) ->
    clear_previous_connection(State),
    make_http_client_pid(State).

clear_previous_connection(State = #state{ibrowse_pid = undefined}) ->
    State;
clear_previous_connection(State = #state{ibrowse_pid = Pid}) ->
    ibrowse_http_client:stop(Pid),
    State.

make_http_client_pid(State = #state{root_url = RootUrl, ibrowse_options = IbrowseOptions}) ->
    Url = create_ibrowse_url(RootUrl),
    {ok, Pid} = ibrowse_http_client:start_link({undefined, Url, create_ssl_options(Url, IbrowseOptions)}),
    State#state{ibrowse_pid = Pid, born_on_time = os:timestamp(), current_connection_requests = 0}.
