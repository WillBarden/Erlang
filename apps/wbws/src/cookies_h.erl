-module(cookies_h).

-export([init/2]).

init(Req, State) ->
    io:fwrite("State = ~p~n", [State]),
    case cowboy_req:method(Req) of
        <<"GET">> ->
            Cookies = cowboy_req:parse_cookies(Req),
            req:ok_json(#{}, Cookies, Req);
        <<"POST">> ->
            { ok, Body, Req1 } = cowboy_req:read_urlencoded_body(Req),
            Req2 = lists:foldl(
                fun({ K, V }, FReq) -> req:set_cookie(K, V, FReq) end,
                Req1,
                Body    
            ),
            Cookies = cowboy_req:parse_cookies(Req),
            req:ok_json(#{}, Cookies, Req)
    end.
