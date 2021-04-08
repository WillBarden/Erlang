-module(req).

-compile(export_all).

read_urlencoded_body(Req) -> cowboy_req:read_urlencoded_body(Req).

method(Req) -> cowboy_req:method(Req).

get_cookie(Key, Req) when is_list(Key) ->
    get_cookie(unicode:characters_to_binary(Key), Req);
get_cookie(Key, Req) ->
    Cookies = cowboy_req:parse_cookies(Req),
    case lists:keyfind(Key, 1, Cookies) of
        false -> null;
        { _, Value } -> Value    
    end.

set_cookie(Key, Value, Req) when is_list(Key) ->
    set_cookie(unicode:characters_to_binary(Key), Value, Req);
set_cookie(Key, Value, Req) when is_list(Value) ->
    set_cookie(Key, unicode:characters_to_binary(Value), Req);
set_cookie(Key, Value, Req) ->
    SecondsInDay = 60 * 60 * 24,
    cowboy_req:set_resp_cookie(Key, Value, Req, #{ max_age => SecondsInDay, http_only => true }).

clear_cookie(Key, Req) when is_list(Key) ->
    clear_cookie(unicode:characters_to_binary(Key), Req);
clear_cookie(Key, Req) ->
    cowboy_req:set_resp_cookie(Key, <<>>,  Req, #{ max_age => 0, http_only => true }).

authorize(Req) -> authorize(Req, []).

authorize(Req, Permissions) ->
    case get_cookie("AUTH_TOKEN", Req) of
        null -> { unauthorized, Req };
        AuthToken -> case auth_token:verify(AuthToken) of
            no_auth_info ->
                Req1 = clear_cookie("AUTH_TOKEN", Req),
                { unauthorized, Req1 };
            expired ->
                Req1 = clear_cookie("AUTH_TOKEN", Req),
                { unauthorized, Req1 };
            invalid_signature ->
                Req1 = clear_cookie("AUTH_TOKEN", Req),
                { unauthorized, Req1 };
            { valid, { UserID, Username, UserPermissions, ExpTime } = AuthInfo } ->
                case length(Permissions) of
                    0 -> { authorized, Req };
                    _ -> 
                        NormalizedPermissions = lists:map(fun(P) -> etc:normalize_permission(P) end, Permissions),
                        NormalizedUserPermissions = lists:map(
                            fun(P) -> etc:normalize_permission(P) end, UserPermissions
                        ),
                        case lists:search(
                            fun(P) -> lists:member(P, NormalizedPermissions) end, NormalizedUserPermissions
                        ) of
                            { value, _Permission } -> { authorized, Req };
                            false ->
                                Req1 = clear_cookie("AUTH_TOKEN", Req),
                                { unauthorized, Req1 }
                        end
                end
        end
    end.

create_error_response(Error) -> #{ <<"error">> => unicode:characters_to_binary(Error) }.

reply(Status, Req) -> cowboy_req:reply(Status, Req).
reply(Status, Headers, Req) -> cowboy_req:reply(Status, Headers, Req).
reply(Status, Headers, Body, Req) -> cowboy_req:reply(Status, Headers, Body, Req).
reply_json(Status, Headers, Body, Req) ->
    Headers1 = Headers#{ <<"content-type">> => <<"application/json">> },
    reply(Status, Headers1, jsone:encode(Body), Req).
reply_error(Status, Headers, Error, Req) ->
    reply_json(Status, Headers, create_error_response(Error), Req).

ok(Req) -> reply(200, Req).
ok(Headers, Req) -> reply(200, Headers, Req).
ok(Headers, Body, Req) -> reply(200, Headers, Body, Req).
ok_json(Headers, Body, Req) -> reply_json(200, Headers, Body, Req).

created(Req) -> reply(201, Req).
created(Headers, Req) -> reply(201, Headers, Req).
created(Headers, Body, Req) -> reply(201, Headers, Body, Req).
created_json(Headers, Body, Req) -> reply_json(201, Headers, Body, Req).

accepted(Req) -> reply(202, Req).
accepted(Headers, Req) -> reply(202, Headers, Req).
accepted(Headers, Body, Req) -> reply(202, Headers, Body, Req).

no_content(Req) -> reply(204, Req).
no_content(Headers, Req) -> reply(204, Headers, Req).
no_content(Headers, Body, Req) -> reply(204, Headers, Body, Req).

bad_req(Req) -> reply(400, Req).
bad_req(Headers, Req) -> reply(400, Headers, Req).
bad_req(Headers, Body, Req) -> reply(400, Headers, Body, Req).
bad_req_json(Headers, Body, Req) -> reply_json(400, Headers, Body, Req).
bad_req_error(Headers, Error, Req) -> reply_error(400, Headers, Error, Req).

unauthorized(Req) -> reply_error(401, #{}, "Anauthorized", Req).
forbidden(Req) -> reply_error(403, #{}, "Forbidden", Req).
not_found(Req) -> reply_error(404, #{}, "Not Found", Req).
method_not_allowed(Req) -> reply_error(405, #{}, "Method Not Allowed", Req).
