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
         request/6
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

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link(RootUrl, IbrowseOptions) ->
    gen_server:start_link(?MODULE, [RootUrl, IbrowseOptions], []).

request(Pid, Path, Headers, Method, Body, Timeout) ->
  gen_server:call(Pid, {request, Path, Headers, Method, Body, Timeout}).
%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([RootUrl, IbrowseOptions]) ->
    Url = create_ibrowse_url(RootUrl),
    SslOptions = create_ssl_options(Url, IbrowseOptions),
    {ok, Pid} = ibrowse_http_client:start_link({undefined, Url, SslOptions}),
    {ok, #state{root_url = RootUrl, ibrowse_options = IbrowseOptions, ibrowse_pid = Pid}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call({request, Path, Headers, Method, Body, Timeout},
    _From,
    State = #state{root_url = RootUrl, ibrowse_options = IbrowseOptions, ibrowse_pid = Pid}) ->
    Result = ibrowse:send_req_direct(Pid, RootUrl ++ Path, Headers, Method, Body, IbrowseOptions, Timeout),
  {reply, Result, State};
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
create_ibrowse_url(RootUrl) ->
  ibrowse_lib:parse_url(RootUrl).

create_ssl_options(#url{protocol = Protocol}, Options) ->
    case (Protocol == https) orelse
        ibrowse_lib:get_value(is_ssl, Options, false) of
        false -> {[], false};
        true -> {ibrowse_lib:get_value(ssl_options, Options, []), true}
    end.
