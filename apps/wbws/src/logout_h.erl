-module(logout_h).

-export([init/2]).

init(Req, State) ->
    Req1 = cowboy_req:set_resp_cookie(<<"AUTH_TOKEN">>, <<>>, Req, #{ max_age => 0, http_only => true }),
    cowboy_req:reply(302, #{ "Location" => "/login" }, Req1),
    { ok, Req1, State }.