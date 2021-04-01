-module(index_h).

-export([init/2]).

init(Req, State) -> cowboy_req:reply(404, Req), { ok, Req, State }.
