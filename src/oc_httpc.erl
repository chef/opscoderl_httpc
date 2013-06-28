-module(oc_httpc).

-export([
    send_req/3,
    send_req/4,
    send_req/5,
    send_req/6
  ]).

-include_lib("ibrowse/include/ibrowse.hrl").

send_req(Url, Headers, Method) ->
  send_req(Url, Headers, Method, [], []).

send_req(Url, Headers, Method, Body) ->
    send_req(Url, Headers, Method, Body, []).

send_req(Url, Headers, Method, Body, Options) ->
    send_req(Url, Headers, Method, Body, Options, 30000).

send_req(UrlString, Headers, Method, Body, Options, Timeout) ->
    oc_ibrowse:send_req(UrlString, Headers, Method, Body, Options, Timeout).
