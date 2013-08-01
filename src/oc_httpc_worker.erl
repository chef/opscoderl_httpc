%%%-------------------------------------------------------------------
%%% @author Oliver Ferrigni <oliverferrigni@seaof001.local>
%%% @copyright (C) 2013, Oliver Ferrigni
%%% @doc
%%%
%%% @end
%%% Created :  2 Jul 2013 by Oliver Ferrigni <oliverferrigni@seaof001.local>
%%%-------------------------------------------------------------------
-module(oc_httpc_worker).

-behaviour(gen_server).

%% API
-export([
         start_link/3,
         request/6,
         multi_request/3
        ]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

%% max_connection_duration is in ms
-record(state, {ibrowse_options = [], root_url, ibrowse_pid, current_connection_requests = 0,
                max_connection_requests, max_connection_duration, born_on_time}).

-include_lib("ibrowse/include/ibrowse.hrl").

%%%===================================================================
%%% API
%%%===================================================================

start_link(RootUrl, IbrowseOptions, Config) ->
    gen_server:start_link(?MODULE, [RootUrl, IbrowseOptions, Config], []).

request(Pid, Path, Headers, Method, Body, Timeout) when is_atom(Method) ->
    gen_server:call(Pid, {request, Path, Headers, Method, Body, Timeout}, Timeout).

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
    MaxRequests = proplists:get_value(max_connection_request_limit, Config, 100),
    MaxConnectionDuration = oc_time:convert_units(proplists:get_value(max_connection_lifetime, Config, {1, min}), ms),
    #url{host = Host, port = Port} = create_ibrowse_url(RootUrl),
    ibrowse:add_config([{ignored, ignored}, {dest, Host, Port, 1, 1, IbrowseOptions}]),
    {ok, #state{root_url = RootUrl, ibrowse_options = IbrowseOptions, ibrowse_pid = undefined,
                max_connection_requests = MaxRequests,
                max_connection_duration = MaxConnectionDuration}}.

handle_call(Request, From, State = #state{ibrowse_pid = undefined}) ->
    handle_call(Request, From, make_http_client_pid(State));
handle_call({request, Path, Headers, Method, Body, Timeout}, _From, State = #state{root_url = RootUrl, ibrowse_options = IbrowseOptions}) ->
    NewState = refresh_connection_process(State),
    Result = ibrowse:send_req_direct(NewState#state.ibrowse_pid, combine(RootUrl, Path), Headers, Method, Body, IbrowseOptions, Timeout),
    {reply, Result, NewState};

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

%% Ibrowse prompted exit
handle_info({'EXIT', Pid, normal}, State = #state{ibrowse_pid = Pid}) ->
    error_logger:info_report(ibrowse_prompted_refresh),
    {noreply, make_http_client_pid(State)};
%% Connection management exit
handle_info({'EXIT', Pid, normal}, State) ->
    {noreply, refresh_connection_process(State)}.

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

make_http_client_pid(State = #state{root_url = RootUrl, ibrowse_options = IbrowseOptions}) ->
    Url = create_ibrowse_url(RootUrl),
    {ok, Pid} = ibrowse_http_client:start_link({undefined, Url, create_ssl_options(Url, IbrowseOptions)}),
    State#state{ibrowse_pid = Pid, born_on_time = os:timestamp(), current_connection_requests = 0}.

refresh_connection_process(State = #state{current_connection_requests = CurrentConnectionRequests,
                                          max_connection_requests = MaxConnectionRequests})
  when CurrentConnectionRequests >= MaxConnectionRequests  ->
    error_logger:info_report(request_limit_hit),
    make_http_client_pid(State);
refresh_connection_process(State = #state{born_on_time = BornOnTime,
                                          max_connection_duration = MaxConnectionDuration,
                                          current_connection_requests = CurrentConnectionRequests}) ->
    Duration = (timer:now_diff(os:timestamp(), BornOnTime)/1000),
    error_logger:info_report({Duration, MaxConnectionDuration, Duration >= MaxConnectionDuration}),
    case Duration >= MaxConnectionDuration of
        true ->
            error_logger:info_report(duration_based),
            make_http_client_pid(State);
        false ->
            State#state{current_connection_requests = CurrentConnectionRequests + 1}
     end.
