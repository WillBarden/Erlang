-module(login_h).

-export([init/2]).

handle_get(Req) ->
    ResPath = filename:join(code:priv_dir(wbws), "login.html"),
    case file:read_file(ResPath) of
        { ok, Data } -> cowboy_req:reply(200, #{}, Data, Req);
        { error, _ } -> cowboy_req:reply(404, Req)
    end.

handle_post(Req) ->
    { ok, Body, _ } = cowboy_req:read_urlencoded_body(Req),
    { _, Username } = lists:keyfind(<<"username">>, 1, Body),
    { _, Password } = lists:keyfind(<<"password">>, 1, Body),

    case users:authenticate({ Username, Password }) of
        { error, _Error } -> cowboy_req:reply(401, Req);
        AuthToken ->
            Req1 = cowboy_req:set_resp_cookie(<<"AUTH_TOKEN">>, AuthToken, Req),
            cowboy_req:reply(302, #{ "Location" => "/home" }, Req1)
    end.


    % case users:create(Username, Password) of
    %     ok -> cowboy_req:reply(302, #{ "Location" => "/login" }, Req);
    %     { error, Error } ->
    %         ErrorMsg = unicode:characters_to_binary(jsone:encode(Error)),
    %         cowboy_req:reply(400, #{}, #{ "error" => ErrorMsg }, Req)
    % end.

handle_unsupp_method(Req) -> cowboy_req:reply(405, Req).

init(Req, State) ->
    case cowboy_req:method(Req) of
        <<"GET">> -> handle_get(Req);
        <<"POST">> -> handle_post(Req);
        _ -> handle_unsupp_method(Req)
    end,
    { ok, Req, State }.