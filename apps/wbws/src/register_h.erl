-module(register_h).

-export([init/2]).

first_missing(Required, Values) ->
    case lists:search(
        fun(RequiredValue) -> not lists:member(RequiredValue, Values) end,
        Required    
    ) of
        { value, Value } -> Value;
        false -> null    
    end.

handle(<<"POST">>, Req) ->
    { ok, RequestFields, Req1 } = cowboy_req:read_urlencoded_body(Req),
    RequiredFieldNames = [<<"username">>, <<"password">>],
    MissingField = first_missing(RequiredFieldNames, lists:map(fun(Field) -> element(1, Field) end, RequestFields)),
    case MissingField of
        null ->
            { _, Username } = lists:keyfind(<<"username">>, 1, RequestFields),
            { _, Password } = lists:keyfind(<<"password">>, 1, RequestFields),
            case users:create({ Username, Password }) of
                { error, Error } -> req:bad_req(Error, Req1);
                ok -> req:created(Req1)
            end;
        MissingFieldName -> 
            ErrorMsg = io_lib:format("Missing required field \"~s\"", [MissingFieldName]),
            req:bad_req_error(ErrorMsg, Req1)
    end;
handle(_, Req) -> req:method_not_allowed(Req).

init(Req, State) -> { ok, req:reply(handle(cowboy_req:method(Req), Req)), State }.