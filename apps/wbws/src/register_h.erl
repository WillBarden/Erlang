-module(register_h).

-export([init/2]).

handle_get(Req) ->
    ResPath = filename:join(code:priv_dir(wbws), "register.html"),
    case file:read_file(ResPath) of
        { ok, Data } -> cowboy_req:reply(200, #{}, Data, Req);
        { error, _ } -> cowboy_req:reply(404, Req)
    end.

handle_post(Req) ->
    { ok, Body, _ } = cowboy_req:read_urlencoded_body(Req),
    { _, Username } = lists:keyfind(<<"username">>, 1, Body),
    { _, Password } = lists:keyfind(<<"password">>, 1, Body),
    case users:create({ Username, Password }) of
        ok -> cowboy_req:reply(302, #{ "Location" => "/login" }, Req);
        { error, Error } ->
            cowboy_req:reply(400, #{}, jsone:encode(#{ <<"error">> => unicode:characters_to_binary(Error) }), Req)
    end.

handle_unsupp_method(Req) ->
    cowboy_req:reply(405, Req).


clear_cookie(Req, Cookie) when is_binary(Cookie) ->
    cowboy_req:set_resp_cookie(Cookie, <<>>, Req, #{ max_age => 0 }).

init(Req, State) ->
    % Cookies = cowboy_req:parse_cookies(Req),
    io:fwrite("Incoming Cookie Example Value: ~p~n", [cowboy_req:parse_cookies(Req)]),
    % Req1 = cowboy_req:set_resp_cookie(<<"asdf">>, <<"New Example Value">>, Req),
    Req1 = clear_cookie(Req, <<"asdf">>),
    Req2 = cowboy_req:reply(200, #{}, jsone:encode(#{ <<"message">> => "Success!" }), Req1),
    { ok, Req2, State }.
    % Req1 = cowboy_req:set_resp_cookie(<<"AUTH_TOKEN">>, <<>>, Req, #{ max_age => 0, http_only => true }),
    % case cowboy_req:method(Req1) of
    %     <<"GET">> -> handle_get(Req1);
    %     <<"POST">> -> handle_post(Req1);
    %     _ -> handle_unsupp_method(Req1)
    % end,
    % { ok, Req1, State }.
