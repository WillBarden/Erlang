-module(req).

-compile(export_all).

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
    cowboy_req:set_resp_cookie(
        unicode:characters_to_binary(Key), 
        unicode:characters_to_binary(Value), 
        Req,
        #{ max_age => (60 * 60 * 24), http_only => true }
    ).

clear_cookie(Key, Req) when is_list(Key) ->
    clear_cookie(unicode:characters_to_binary(Key), Req);
clear_cookie(Key, Req) ->
    cowboy_req:set_resp_cookie(
        unicode:characters_to_binary(Key),
        <<>>, 
        Req,
        #{ max_age => 0, http_only => true }
    ).

reply(ReplyTuple) ->
    case ReplyTuple of
        { Status, Req } -> cowboy_req:reply(Status, Req);
        { Status, Body, Req } -> cowboy_req:reply(Status, #{}, Body, Req);
        { Status, Body, Headers, Req } -> cowboy_req:reply(Status, Headers, Body, Req)
    end.

create_error_response(Error) -> #{ <<"error">> => unicode:characters_to_binary(Error) }.

ok(Req) -> { 200, Req }.
ok(Body, Req) -> { 200, Body, Req }.
ok(Body, Headers, Req) -> { 200, Body, Headers, Req }.
ok_json(Body, Req) -> { 200, jsone:encode(Body), Req }.
ok_json(Body, Headers, Req) -> { 200, jsone:encode(Body), Headers, Req }.

created(Req) -> { 201, Req }.
created(Body, Req) -> { 201, Body, Req }.
created(Body, Headers, Req) -> { 201, Body, Headers, Req }.
created_json(Body, Req) -> { 201, jsone:encode(Body), Req }.
created_json(Body, Headers, Req) -> { 201, jsone:encode(Body), Headers, Req }.

accepted(Req) -> { 202, Req }.
accepted(Body, Req) -> { 202, Body, Req }.
accepted(Body, Headers, Req) -> { 202, Body, Headers, Req }.
accepted_json(Body, Req) -> { 202, jsone:encode(Body), Req }.
accepted_json(Body, Headers, Req) -> { 202, jsone:encode(Body), Headers, Req }.

no_content(Req) -> { 204, Req }.
no_content(Body, Req) -> { 204, Body, Req }.
no_content(Body, Headers, Req) -> { 204, Body, Headers, Req }.
no_content_json(Body, Req) -> { 204, jsone:encode(Body), Req }.
no_content_json(Body, Headers, Req) -> { 204, jsone:encode(Body), Headers, Req }.

redirect(Location, Req) -> { 302, <<>>, #{ "Location" => Location }, Req }.

bad_req(Req) -> { 400, Req }.
bad_req(Body, Req) -> { 400, Body, Req }.
bad_req(Body, Headers, Req) -> { 400, Body, Headers, Req }.
bad_req_json(Body, Req) -> { 400, jsone:encode(Body), Req }.
bad_req_json(Body, Headers, Req) -> { 400, jsone:encode(Body), Headers, Req }.
bad_req_error(Error, Req) -> { 400, jsone:encode(create_error_response(Error)), Req }.
bad_req_error(Error, Headers, Req) -> { 400, jsone:encode(create_error_response(Error)), Headers, Req }.

unauthorized(Req) -> { 401, jsone:encode(create_error_response("Unauthorized")), Req }.
forbidden(Req) -> { 403, jsone:encode(create_error_response("Forbidden")), Req }.
not_found(Req) -> { 404, jsone:encode(create_error_response("Not Found")), Req }.
method_not_allowed(Req) -> { 405, jsone:encode(create_error_response("Method Not Allowed")), Req }.
