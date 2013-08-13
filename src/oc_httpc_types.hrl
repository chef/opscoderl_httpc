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

-type ibrowse_option() :: {max_sessions, integer()}        |
          {response_format,response_format()}|
          {stream_chunk_size, non_neg_integer()}     |
          {max_pipeline_size, non_neg_integer()}     |
          {trace, boolean()}                 | 
          {is_ssl, boolean()}                |
          {ssl_options, [sSLOpt()]}            |
          {pool_name, atom()}                |
          {proxy_host, string()}             |
          {proxy_port, integer()}            |
          {proxy_user, string()}             |
          {proxy_password, string()}         |
          {use_absolute_uri, boolean()}      |
          {basic_auth, {username(), password()}} |
          {cookie, string()}                 |
          {content_length, integer()}        |
          {content_type, string()}           |
          {save_response_to_file, srtf()}    |
          {stream_to, stream_to()}           |
          {http_vsn, {majorVsn(), minorVsn()}}   |
          {host_header, string()}            |
          {inactivity_timeout, integer()}    |
          {connect_timeout, integer()}       |
          {socket_options, sock_opts()}        |
          {transfer_encoding, {chunked, chunkSize()}} | 
          {headers_as_is, boolean()}         |
          {give_raw_headers, boolean()}      |
          {preserve_chunked_encoding,boolean()}     |
          {workaround, head_response_with_body}     |
          {worker_process_options, list()}.

-type stream_to() :: process() | {process(), once}.
-type process() :: pid() | atom().
-type username() :: string().
-type password() :: string().
-type sSLOpt() :: term().
-type sock_opts() :: [sock_opt()].
-type sock_opt() :: term().
-type chunkSize() :: integer().
-type majorVsn() :: any().
-type minorVsn() :: any().
-type srtf() :: boolean() | filename() | {append, filename()}.
-type filename() :: string().
-type response_format() :: list | binary.
-type pool_option() :: {root_url, string()} |
                       {max_conns, non_neg_integer()} | 
                       {init_cons, non_neg_integer()} |
                       {default_timeout, non_neg_integer()} |
                       {when_full, start_new} |
                       {max_age, non_neg_integer()} |
                       {health_check_path, string()} |
                       {health_check_interval, non_neg_integer()} |
                       {ibrowse_opts, [ibrowse_option()]}.
-type pool_config() :: [pool_option()].
-type headerList() :: [{header(), value()}].
-type header() :: atom() | string().
-type value() :: term().
-type method() :: get | post | head | options | put | delete | trace | mkcol | propfind | proppatch | lock | unlock | move | copy.
-type status() :: string().
-type responseHeaders() :: [respHeader()].
-type respHeader() :: {headerName(), headerValue()}.
-type headerName() :: string().
-type headerValue() :: string().
-type response() :: {ok, status(), responseHeaders(), responseBody()} | {ibrowse_req_id, req_id() } | {error, reason()}.
-type req_id() :: term().
-type responseBody() :: string() | {file, filename()}.
-type reason() :: term().
-type initial_state() :: term().
-type body() :: [] | string() | binary() | fun() | {fun(() -> any()), initial_state()}.
