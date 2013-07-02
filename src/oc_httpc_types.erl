-module(oc_httpc_types).

-export_type(
[
  pool_name/0,
  pool_config/0,
  method/0,
  body/0,
  headerList/0,
  response/0
]).

-type ibrowse_option() :: {max_sessions, integer()}        |
          {response_format,response_format()}|
          {stream_chunk_size, integer()}     |
          {max_pipeline_size, integer()}     |
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
-type pool_name() :: atom().
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
