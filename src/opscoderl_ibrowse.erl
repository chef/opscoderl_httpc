-module(opscoderl_ibrowse).

-export([
    send_req/6
  ]).

-include_lib("ibrowse/include/ibrowse.hrl").

send_req(UrlString, Headers, Method, Body, Options, Timeout) ->
    Url = #url{host = Host, port = Port, protocol = Protocol} = ibrowse_lib:parse_url(UrlString),
    {SSLOptions, IsSSL} =
        case (Protocol == https) orelse
            ibrowse_lib:get_value(is_ssl, Options, false) of
            false -> {[], false};
            true -> {ibrowse_lib:get_value(ssl_options, Options, []), true}
       end,
    PoolName = list_to_atom(Host ++ integer_to_list(Port)),
    PoolConfig = [{name, PoolName}, {init_count, 50}, {max_count, 250},
      {start_mfa, {ibrowse_http_client, start_link, [{undefined, Url, {SSLOptions, IsSSL}}]}}],
    pooler:new_pool(PoolConfig),
    Pid = pooler:take_member(PoolName),
    Result = ibrowse:send_req_direct(Pid, UrlString, Headers, Method, Body, Options, Timeout),
    pooler:return_member(PoolName, Pid),
    Result.
