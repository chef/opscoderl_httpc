%% Copyright 2012 Opscode, Inc. All Rights Reserved.
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

-module(opscoderl_ibrowse_tests).

-include_lib("eunit/include/eunit.hrl").

opscoderl_ibrowse_test_() ->
    {setup,
     fun() ->
            application:start(crypto),
            application:start(public_key),
            application:start(ssl),
            application:start(pooler),
            (catch ibrowse_test_server:start_server(8181, tcp)),
            ibrowse:start(),
             ok
     end,
     fun(_) ->
            catch ibrowse_test_server:stop_server(8181),
             ok
     end,
     [
		    fun() -> assert_200_req("http://www.google.co.uk", get) end,
		    fun() -> assert_200_req("http://www.google.com", get) end,
		    fun() -> assert_200_req("http://www.google.com", options) end,
        fun() -> assert_200_req("https://mail.google.com", get) end,
		    fun() -> assert_200_req("http://www.sun.com", get) end,
		    fun() -> assert_200_req("http://www.oracle.com", get) end,
		    fun() -> assert_200_req("http://www.bbc.co.uk", get) end,
		    fun() -> assert_200_req("http://www.bbc.co.uk", trace) end,
		    fun() -> assert_200_req("http://www.bbc.co.uk", options) end,
		    fun() -> assert_200_req("http://yaws.hyber.org", get) end,
		    fun() -> assert_200_req("http://jigsaw.w3.org/HTTP/ChunkedScript", get) end,
		    fun() -> assert_200_req("http://jigsaw.w3.org/HTTP/TE/foo.txt", get) end,
		    fun() -> assert_200_req("http://jigsaw.w3.org/HTTP/TE/bar.txt", get) end,
		    fun() -> assert_200_req("http://jigsaw.w3.org/HTTP/connection.html", get) end,
		    fun() -> assert_200_req("http://jigsaw.w3.org/HTTP/cc.html", get) end,
		    fun() -> assert_200_req("http://jigsaw.w3.org/HTTP/cc-private.html", get) end,
		    fun() -> assert_200_req("http://jigsaw.w3.org/HTTP/cc-proxy-revalidate.html", get) end,
		    fun() -> assert_200_req("http://jigsaw.w3.org/HTTP/cc-nocache.html", get) end,
		    fun() -> assert_200_req("http://jigsaw.w3.org/HTTP/h-content-md5.html", get) end,
		    fun() -> assert_200_req("http://jigsaw.w3.org/HTTP/h-retry-after.html", get) end,
		    fun() -> assert_200_req("http://jigsaw.w3.org/HTTP/h-retry-after-date.html", get) end,
		    fun() -> assert_200_req("http://jigsaw.w3.org/HTTP/neg", get) end,
		    fun() -> assert_200_req("http://jigsaw.w3.org/HTTP/negbad", get) end,
		    fun() -> assert_200_req("http://jigsaw.w3.org/HTTP/400/toolong/", get) end,
		    fun() -> assert_200_req("http://jigsaw.w3.org/HTTP/300/", get) end,
		    fun() -> assert_200_req("http://jigsaw.w3.org/HTTP/Basic/", get,[{basic_auth, {"guest", "guest"}}]) end,
        fun() -> assert_200_req("https://github.com", get, [{ssl_options, [{depth, 2}]}]) end,
		    fun() -> assert_200_req("http://jigsaw.w3.org/HTTP/CL/", get) end,
		    fun() -> assert_200_req("http://www.httpwatch.com/httpgallery/chunked/", get) end
      ]}.

assert_200_req(Url, Method) ->
    assert_200_req(Url, Method, []).
assert_200_req(Url, Method, OptionsInput) ->
    Options = [{connect_timeout, 5000}] ++ OptionsInput,
    Result = (catch opscoderl_ibrowse:send_req(Url, [], Method, [], Options, 60000)),
    ?assertMatch({ok, _, _, _}, Result).

