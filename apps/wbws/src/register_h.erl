-module(register_h).

-export([init/2]).

handle(Req) -> handle(req:method(Req), Req).

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
            case users:create({ Username, Password }) of
                { invalid_username, Reason } -> req:bad_req_error(#{}, Reason, Req1);
                { invalid_password, Reason } -> req:bad_req_error(#{}, Reason, Req1);
                ok -> req:created(Req1)
            end
    end;
handle(<<"PUT">>, Req) ->
    case req:authorize(Req) of
        { authorized, Req1 } ->
            { ok, RequestFields, Req2 } = req:read_urlencoded_body(Req1),
            RequestFieldNames = lists:map(fun({ Name, _Value }) -> Name end, RequestFields),
            case lists:search(
                fun(FieldName) -> not lists:member(FieldName, RequestFieldNames) end,
                [<<"username">>, <<"password">>, <<"new_password">>]
            ) of
                { value, MissingField } -> req:bad_req_error(#{}, io_lib:format("Field ~s required", [MissingField]), Req2);
                false ->
                    { _, Username } = lists:keyfind(<<"username">>, 1, RequestFields),
                    { _, Password } = lists:keyfind(<<"password">>, 1, RequestFields),
                    { _, NewPassword } = lists:keyfind(<<"new_password">>, 1, RequestFields),
                    { _, AuthenticatedUsername, _, _ } = auth_token:decode(req:get_cookie("AUTH_TOKEN", Req2)),
                    if
                        Username =/= AuthenticatedUsername ->
                            req:bad_req_error(#{}, "Credentials do match those of the currently authenticated user", Req2);
                        Username =:= AuthenticatedUsername ->
                            case users:update_credentials({ Username, Password, NewPassword }) of
                                no_user -> req:bad_req_error(#{}, io_lib:format("User ~s does not exist", [Username]), Req2);
                                change_limit_met -> req:bad_req_error(#{}, "Password change frequency limit met", Req2);
                                invalid_credentials -> req:bad_req_error(#{}, "Invalid credentials", Req2);
                                { invalid_password, Reason } -> req:bad_req_error(#{}, Reason, Req2);
                                ok ->
                                    Req3 = req:clear_cookie("AUTH_TOKEN", Req2),
                                    req:ok(Req3)
                            end
                    end
            end;
        { unauthorized, Req1 } -> req:unauthorized(Req1)
    end;
handle(_, Req) -> req:method_not_allowed(Req).

init(Req, State) -> { ok, handle(Req), State }.