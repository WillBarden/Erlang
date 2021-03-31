-module(index_h).

-export([init/2]).

init(Req, State) -> 
    cowboy_req:reply(200, #{}, <<"<h1>Hello!</h1>">>, Req),
    { ok, Req, State }.
