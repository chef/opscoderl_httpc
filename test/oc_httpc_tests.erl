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

-module(oc_httpc_tests).

-include_lib("eunit/include/eunit.hrl").

opscoderl_ibrowse_test_() ->
    {setup,
     fun() ->
             application:ensure_all_started(ssl),
             application:ensure_all_started(pooler),
             ibrowse:start(),
             ok
     end,
     fun(_) ->
             ok
     end,
     [
      assert_200_req("http://www.google.co.uk", "", get),
      assert_200_req("http://www.google.com", "", get),
      assert_200_req("http://www.google.com", "",options),
      assert_200_req("https://mail.google.com", "",get),
      assert_200_req("http://www.sun.com", "",get),
      assert_200_req("http://www.oracle.com", "",get),
      assert_200_req("http://www.bbc.co.uk", "",get),
      assert_200_req("http://www.bbc.co.uk", "",trace),
      assert_200_req("http://www.bbc.co.uk", "",options),
      assert_200_req("http://yaws.hyber.org", "",get),
      assert_200_req("http://jigsaw.w3.org/", "HTTP/ChunkedScript", get),
      assert_200_req("http://jigsaw.w3.org/", "HTTP/TE/foo.txt", get),
      assert_200_req("http://jigsaw.w3.org/", "HTTP/TE/bar.txt", get),
      assert_200_req("http://jigsaw.w3.org/", "HTTP/connection.html", get),
      assert_200_req("http://jigsaw.w3.org/", "HTTP/cc.html", get),
      assert_200_req("http://jigsaw.w3.org/", "HTTP/cc-private.html", get),
      assert_200_req("http://jigsaw.w3.org/", "HTTP/cc-proxy-revalidate.html", get),
      assert_200_req("http://jigsaw.w3.org/", "HTTP/cc-nocache.html", get),
      assert_200_req("http://jigsaw.w3.org/", "HTTP/h-content-md5.html", get),
      assert_200_req("http://jigsaw.w3.org/", "HTTP/h-retry-after.html", get),
      assert_200_req("http://jigsaw.w3.org/", "HTTP/h-retry-after-date.html", get),
      assert_200_req("http://jigsaw.w3.org/", "HTTP/neg", get),
      assert_200_req("http://jigsaw.w3.org/", "HTTP/negbad", get),
      assert_200_req("http://jigsaw.w3.org/", "HTTP/400/toolong/", get),
      assert_200_req("http://jigsaw.w3.org/", "HTTP/300/", get),
      assert_200_req("http://jigsaw.w3.org/", "HTTP/Basic/", get,[{basic_auth, {"guest", "guest"}}]),
      assert_200_req("https://github.com", "",get, [{ssl_options, [{depth, 2}]}]),
      assert_200_req("http://jigsaw.w3.org/", "HTTP/CL/", get),
      assert_200_req("http://www.httpwatch.com/", "httpgallery/chunked/", get)
     ]}.

assert_200_req(RootUrl, Endpoint, Method) ->
    assert_200_req(RootUrl, Endpoint, Method, []).
assert_200_req(RootUrl, Endpoint, Method, OptionsInput) ->
    {atom_to_list(Method) ++ " " ++ RootUrl ++ "/" ++ Endpoint,
     fun() ->
             Options = [{connect_timeout, 5000}] ++ OptionsInput,
             PoolConfig = [{root_url, RootUrl}, {init_count, 50}, {max_count, 250},
                           {ibrowse_options, Options}],
             oc_httpc:add_pool(list_to_atom(RootUrl), PoolConfig),
             Result = (catch oc_httpc:request(list_to_atom(RootUrl), Endpoint, [], Method, [], 60000)),
             oc_httpc:delete_pool(list_to_atom(RootUrl)),
             ?assertMatch({ok, _, _, _}, Result)
     end}.

multi_request_test() ->
    RootUrl = "http://jigsaw.w3.org/",
    CallbackFun = fun(RequestFun) ->
                          Paths = ["HTTP/ChunkedScript",
                                   "HTTP/TE/foo.txt",
                                   "HTTP/TE/bar.txt",
                                   "HTTP/connection.html",
                                   "HTTP/cc.html",
                                   "HTTP/cc-private.html",
                                   "HTTP/cc-proxy-revalidate.html",
                                   "HTTP/cc-nocache.html",
                                   "HTTP/h-content-md5.html",
                                   "HTTP/h-retry-after.html",
                                   "HTTP/h-retry-after-date.html",
                                   "HTTP/neg",
                                   "HTTP/negbad",
                                   "HTTP/400/toolong/",
                                   "HTTP/300/"],
                          [ RequestFun(P, [], get, []) || P <- Paths ]
                  end,
    Options = [{connect_timeout, 5000}],
    PoolConfig = [{root_url, RootUrl}, {init_count, 50}, {max_count, 250},
                  {ibrowse_options, Options}],
    oc_httpc:add_pool(list_to_atom(RootUrl), PoolConfig),
    Results = (catch oc_httpc:multi_request(list_to_atom(RootUrl), CallbackFun , 60000)),
    oc_httpc:delete_pool(list_to_atom(RootUrl)),
    ?assertEqual(15, length(Results)),
    [?assertMatch({ok, _,_,_}, Result) || Result <- Results].

process_leak_test() ->
    Options = [{connect_timeout, 5000}],
    PoolConfig = [{root_url, "http://google.com"}, {init_count, 3}, {max_count, 3},
                  {ibrowse_options, Options}, {max_connection_requests, 1}],
    oc_httpc:add_pool(proc_mon_test, PoolConfig),
    Me = self(),
    [ spawn(fun() ->
                    oc_httpc:request(proc_mon_test, "/", [], get, [], 60000),
                    Me ! done
            end) || _X <- lists:seq(1, 20)
    ],
    collect_all(20),
    %% Each worker should only have two links, one link to the member_sup
    %% and one link to the ibrowse pid
    ?assert(lists:all(fun(X) -> length(X) =< 2 end, worker_links(proc_mon_test))),
    oc_httpc:delete_pool(proc_mon_test).

collect_all(Count) ->
    collect_all(Count, 0).

collect_all(Count, Count) ->
    ok;
collect_all(Count, Curr) ->
    receive
        done ->
            collect_all(Count, Curr + 1)
    end.

%% Takes an atom representing a pool name and returns a list of lists.  The inner lists
%% contain the links for each pool worker process.
worker_links(PoolName) ->
    MemberSup = list_to_atom("pooler_" ++ atom_to_list(PoolName) ++ "_member_sup"),
    PoolSup = list_to_atom("pooler_" ++ atom_to_list(PoolName) ++ "_pool_sup"),
    {links, Workers} = erlang:process_info(whereis(MemberSup), links),
    Info = [ erlang:process_info(W) || W <- Workers ],
    [proplists:get_value(links, I) || I <- Info, proplists:get_value(registered_name, I) /= PoolSup].

request_error_test_() ->
    Modules = [ibrowse, ibrowse_http_client],
    Timeout = 500,
    {foreach,
     fun() ->
             error_logger:tty(false),
             application:start(ibrowse),
             RootUrl = "http://jigsaw.w3.org/",
             Options = [{connect_timeout, 5000}],
             PoolConfig = [{root_url, RootUrl}, {init_count, 50}, {max_count, 250},
                           {ibrowse_options, Options}],
             oc_httpc:add_pool(foo, PoolConfig),
             Pid = spawn(fun() -> ok end),


             [meck:new(Mod) || Mod <- Modules],
             meck:expect(ibrowse_http_client, start_link, fun(_) ->
                                                                  {ok, Pid}
                                                          end),

             ok
     end,
     fun(_) ->
             [meck:unload(Mod) || Mod <- Modules],
             oc_httpc:delete_pool(foo),
             application:stop(ibrowse),
             ok
     end,

     [{"Timeout exception should be adapted to error, req_timedout",
       fun() ->
               meck:expect(ibrowse, send_req_direct, fun(_, _, _, _, _, _, _) ->
                                                             timer:sleep(Timeout + 500)
                                                     end),

               Result = oc_httpc:request(foo, "/", [], get, [], Timeout),

               ?assertEqual({error, req_timedout}, Result)
       end},
     {"Any other exception should be thrown",
     fun() ->
             meck:expect(ibrowse, send_req_direct, fun(_,_,_,_,_,_,_) ->
                                                           erlang:error(other_error)
                                                               end),
             Result = (catch(oc_httpc:request(foo, "/", [], get, [], Timeout))),

             ?assertMatch({'EXIT', {{other_error,_}, _}}, Result)
                            end}]
    }.

get_pooler_timeout_test() ->
    application:set_env(opscoderl_httpc, pooler_timeout, foo),
    ?assertExit({invalid_pooler_timeout, foo}, oc_httpc:get_pooler_timeout()),
    application:set_env(opscoderl_httpc, pooler_timeout, {-1, min}),
    ?assertExit({invalid_pooler_timeout, {-1, min}}, oc_httpc:get_pooler_timeout()),
    application:set_env(opscoderl_httpc, pooler_timeout, {0, min}),
    ?assertEqual({0, min}, oc_httpc:get_pooler_timeout()),
    application:set_env(opscoderl_httpc, pooler_timeout, 100),
    ?assertEqual(100, oc_httpc:get_pooler_timeout()),
    application:set_env(opscoderl_httpc, pooler_timeout, undefined).
