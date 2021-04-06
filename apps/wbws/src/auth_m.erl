-module(auth_m).

-export([execute/2]).

execute(Req, Env) ->
    #{ handler_opts := { AuthOpts, HandlerOpts } } = Env,
    case AuthOpts of
        unprotected -> { ok, Req, Env#{ handler_opts => HandlerOpts } };
        _ ->
            Req1 = cowboy_req:reply(401, Req),
            { stop, Req1 }
    end.
