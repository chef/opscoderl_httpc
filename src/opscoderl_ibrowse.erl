-module(opscoderl_ibrowse).

-export([
    send_req/3,
    send_req/4,
    send_req/5,
    send_req/6
  ]).


send_req(Url, Headers, Method) ->
  send_req(Url, Headers, Method, [], []).

send_req(Url, Headers, Method, Body) ->
    send_req(Url, Headers, Method, Body, []).

send_req(Url, Headers, Method, Body, Options) ->
    send_req(Url, Headers, Method, Body, Options, 30000).

send_req(Url, Headers, Method, Body, Options, Timeout) ->
  ibrowse:send_req(Url, Headers, Method, Body, Options, Timeout).
