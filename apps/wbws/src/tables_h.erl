-module(tables_h).

-export([init/2, websocket_init/1, websocket_handle/2, websocket_info/2, terminate/3]).

init(Req, State) ->
    Ws = req:binding(ws, Req, "") =:= <<"ws">>,
    case Ws of 
        true -> case req:authorize(Req) of
            { authorized, Req1 } -> { cowboy_websocket, Req1, State };
            { unauthorized, Req1 } -> { ok, req:unauthorized(Req1), State}
        end;
        false -> { ok, handle(Req), State }
    end.

websocket_init(State) ->
    io:fwrite("Websocket Init ~p~n", [State]),
    { ok, State }.

websocket_handle({ text, Msg } = InFrame, State) ->
    io:fwrite("Websocket frame received ~p~nState ~p~n", [InFrame, State]),
    { reply, { text, Msg }, State }.

websocket_info(Info, State) ->
    io:fwrite("~p: ~p~n", [self(), Info]),
    { ok, State }.

terminate(Reason, PartialReq, State) ->
    io:fwrite("Process terminating ~p~n", [self()]),
    normal.

handle(Req) -> handle(req:method(Req), Req).

handle(<<"POST">>, Req) ->
    case req:authorize(Req) of
        { authorized, Req1 } ->
            TableID = binary_to_list(req:binding(id, Req1, <<"">>)),
            if
                TableID =:= "" ->
                    { ok, RequestFields, Req2 } = req:read_urlencoded_body(Req1),
                    RequestFieldNames = lists:map(fun({ Name, _Value }) -> Name end, RequestFields),
                    case lists:search(
                        fun(FieldName) -> not lists:member(FieldName, RequestFieldNames) end,
                        [<<"seats">>, <<"name">>]
                    ) of
                        { value, MissingField } ->
                            req:bad_req_error(#{}, io_lib:format("Field ~s required", [MissingField]), Req2);
                        false ->
                            { _, TableSeats } = lists:keyfind(<<"seats">>, 1, RequestFields),
                            { _, TableName } = lists:keyfind(<<"name">>, 1, RequestFields),
                            try binary_to_integer(TableSeats) of
                                TableSeatsCount ->
                                    { UserID, _, _, _ } = auth_token:decode(req:get_cookie("AUTH_TOKEN", Req2)),
                                    case tables:create({ UserID, TableSeatsCount, TableName }) of
                                        no_user -> req:bad_req_error(#{}, "Tables requested owner does not exist", Req2);
                                        { invalid_seat_count, Error } -> req:bad_req_error(#{}, Error, Req2);
                                        { invalid_name, Error } -> req:bad_req_error(#{}, Error, Req2);
                                        { error, Error } -> req:bad_req(Req2);
                                        { ok, TableID } -> req:created_json(#{}, TableID, Req2)
                                    end,
                                    req:created(Req2)
                            catch
                                _:_ -> req:bad_req_error(#{}, "Table seat count must be a valid integer", Req2)
                            end
                    end;
                TableID =/= "" -> req:not_found(Req1)
            end;
        { unauthorized, Req1 } -> req:unauthorized(Req1)
    end.