-module(cookies_h).

-export([init/2]).

init(Req, State) ->
    io:fwrite("State = ~p~n", [State]),
    case cowboy_req:method(Req) of
        <<"GET">> ->
            Cookies = cowboy_req:parse_cookies(Req),
            cowboy_req:reply(200, #{}, jsone:encode(Cookies), Req),
            { ok, Req, State };
        <<"POST">> ->
            { ok, Body, _Other } = cowboy_req:read_urlencoded_body(Req),
            { _, Value } = lists:keyfind(<<"SET_TEST_COOKIE">>, 1, Body),
            Req1 = cowboy_req:set_resp_cookie(<<"TEST_COOKIE">>, Value, Req),
            cowboy_req:reply(200, #{}, jsone:encode(Body), Req1),
            { ok, Req1, State }
    end.
