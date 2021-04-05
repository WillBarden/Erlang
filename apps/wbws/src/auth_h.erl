-module(auth_h).

-export([init/2]).

-compile(export_all).

all_missing(Required, Values) ->
    lists:filtermap(
        fun(RequiredValue) -> not lists:member(RequiredValue, Values) end,
        Required
    ).

first_missing(Required, Values) ->
    case lists:search(
        fun(RequiredValue) -> not lists:member(RequiredValue, Values) end,
        Required    
    ) of
        { value, Value } -> Value;
        false -> null    
    end.

handle(<<"GET">>, Req) ->
    case req:get_cookie("AUTH_TOKEN", Req) of
        null -> req:no_content(Req);
        AuthToken -> case users:verify({ AuthToken }) of
            { error, _Error } -> req:no_content(Req);
            { TokenInfo, FreshToken } ->
                { ResponseTokenInfo, Req2 } = if 
                    FreshToken == AuthToken ->
                        #{ user_id := UserID, username := Username, exp := ExpTime } = TokenInfo,
                        { #{ user_id => UserID, username => Username, exp => calendar:gregorian_seconds_to_datetime(ExpTime) }, Req };
                    true ->
                        Req1 = req:set_cookie("AUTH_TOKEN", FreshToken, Req),
                        FreshTokenInfo = element(1, users:verify({ FreshToken })),
                        #{ user_id := UserID, username := Username, exp := ExpTime } = FreshTokenInfo,
                        { #{ user_id => UserID, username => Username, exp => calendar:gregorian_seconds_to_datetime(ExpTime) }, Req1 }
                end,
                req:ok_json(ResponseTokenInfo, Req2)
        end
    end;
handle(<<"POST">>, Req) ->
    { ok, RequestFields, Req1 } = cowboy_req:read_urlencoded_body(Req),
    RequiredFieldNames = [<<"username">>, <<"password">>],
    MissingField = first_missing(RequiredFieldNames, lists:map(fun(Field) -> element(1, Field) end, RequestFields)),
    case MissingField of
        null ->
            { _, Username } = lists:keyfind(<<"username">>, 1, RequestFields),
            { _, Password } = lists:keyfind(<<"password">>, 1, RequestFields),
            case users:authenticate({ Username, Password }) of
                { error, Error } -> 
                    Req2 = req:clear_cookie("AUTH_TOKEN", Req1),
                    req:bad_req(Error, Req2);
                AuthToken ->
                    Req2 = req:set_cookie("AUTH_TOKEN", AuthToken, Req1),
                    req:ok(Req2)
            end;
        MissingFieldName -> 
            Req2 = req:clear_cookie("AUTH_TOKEN", Req1),
            ErrorMsg = io_lib:format("Missing required field \"~s\"", [MissingFieldName]),
            req:bad_req_error(ErrorMsg, Req2)
    end;
handle(_, Req) -> req:method_not_allowed(Req).

init(Req, State) -> { ok, req:reply(handle(cowboy_req:method(Req), Req)), State }.
