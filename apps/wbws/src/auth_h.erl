-module(auth_h).

-export([init/2]).

-compile(export_all).

init(Req, State) -> { ok, handle(Req), State }.

handle(Req) -> handle(req:method(Req), Req).

handle(<<"GET">>, Req) ->
    case req:authorize(Req) of
        { authorized, Req1 } ->
            { UserID, Username, Permissions, ExpTime } = auth_token:decode(req:get_cookie("AUTH_TOKEN", Req1)),
            req:ok_json(
                #{},
                #{ user_id => UserID, username => Username, permissions => Permissions, exp => ExpTime },
                Req1
            );
        { unauthorized, Req1 } -> req:unauthorized(Req1)
    end;
handle(<<"POST">>, Req) ->
    { ok, RequestFields, Req1 } = req:read_urlencoded_body(Req),
    RequestFieldNames = lists:map(fun({ Name, _Value }) -> Name end, RequestFields),
    case lists:search(
        fun(FieldName) -> not lists:member(FieldName, RequestFieldNames) end,
        [<<"username">>, <<"password">>]
    ) of
        { value, MissingField } -> req:bad_req_error(#{}, io_lib:format("Field ~s required", [MissingField]), Req1);
        false ->
            { _, Username } = lists:keyfind(<<"username">>, 1, RequestFields),
            { _, Password } = lists:keyfind(<<"password">>, 1, RequestFields),
            case users:authenticate({ Username, Password }) of
                no_user ->
                    Req2 = req:clear_cookie("AUTH_TOKEN", Req1),
                    req:bad_req_error(#{}, io_lib:format("User ~s does not exist", [Username]), Req2);
                auth_limit_met ->
                    Req2 = req:clear_cookie("AUTH_TOKEN", Req1),
                    req:bad_req_error(#{}, "Authentication frequency limit met", Req2);
                invalid_credentials ->
                    Req2 = req:clear_cookie("AUTH_TOKEN", Req1),
                    req:bad_req_error(#{}, "Invalid credentials", Req2);
                { authenticated, AuthToken } ->
                    Req2 = req:set_cookie(<<"AUTH_TOKEN">>, AuthToken, Req1),
                    { UserID, Username, Permissions, ExpTime } = auth_token:decode(AuthToken),
                    req:ok_json(
                        #{},
                        #{ user_id => UserID, username => Username, permissions => Permissions, exp => ExpTime },
                        Req2
                    )
            end
    end;
handle(<<"PUT">>, Req) ->
    case req:authorize(Req) of
        { authorized, Req1 } ->
            { UserID, Username, Permissions, _ExpTime } = auth_token:decode(req:get_cookie("AUTH_TOKEN", Req1)),
            FreshToken = auth_token:sign({ UserID, Username, Permissions }),
            { FreshUserID, FreshUsername, FreshPermissions, FreshExpTime } = auth_token:decode(FreshToken),
            Req2 = req:set_cookie("AUTH_TOKEN", FreshToken, Req1),
            req:ok_json(
                #{},
                #{ 
                    user_id => FreshUserID, username => FreshUsername, permissions => FreshPermissions, 
                    exp => FreshExpTime
                },
                Req2
            );
        { unauthorized, Req1 } -> req:unauthorized(Req1)
    end;
handle(<<"DELETE">>, Req) ->
    Req1 = req:clear_cookie("AUTH_TOKEN", Req),
    req:ok(Req1);
handle(_, Req) -> req:method_not_allowed(Req).
