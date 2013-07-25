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
         start_link/2,
         request/6,
         multi_request/3
        ]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {ibrowse_options = [], root_url, ibrowse_pid}).

-include_lib("ibrowse/include/ibrowse.hrl").

%%%===================================================================
%%% API
%%%===================================================================

start_link(RootUrl, IbrowseOptions) ->
    gen_server:start_link(?MODULE, [RootUrl, IbrowseOptions], []).

request(Pid, Path, Headers, Method, Body, Timeout) ->
    gen_server:call(Pid, {request, Path, Headers, Method, Body, Timeout}, Timeout).

multi_request(Pid, Fun, Timeout) ->
    gen_server:call(Pid, {multi_request, Fun, Timeout}, Timeout).

%%%===================================================================
%%% Gen Server Callbacks
%%%===================================================================
init([RootUrl, IbrowseOptions]) ->
    Url = create_ibrowse_url(RootUrl),
    SslOptions = create_ssl_options(Url, IbrowseOptions),
    {ok, Pid} = ibrowse_http_client:start_link({undefined, Url, SslOptions}),
    {ok, #state{root_url = RootUrl, ibrowse_options = IbrowseOptions, ibrowse_pid = Pid}}.

handle_call({request, Path, Headers, Method, Body, Timeout},
            _From,
            State = #state{root_url = RootUrl, ibrowse_options = IbrowseOptions, ibrowse_pid = Pid}) ->
    Result = ibrowse:send_req_direct(Pid, combine(RootUrl, Path), Headers, Method, Body, IbrowseOptions, Timeout),
    {reply, Result, State};

handle_call({multi_request, CallbackFun, Timeout}, _From, State = #state{root_url = RootUrl, ibrowse_options = IbrowseOptions, ibrowse_pid = Pid} ) ->
    RequestFun = fun(Path, Headers, Method, Body) ->
                         ibrowse:send_req_direct(Pid, combine(RootUrl, Path), Headers, Method, Body, IbrowseOptions, Timeout)
                 end,
    Result = CallbackFun(RequestFun),
    {reply, Result, State};
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
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
