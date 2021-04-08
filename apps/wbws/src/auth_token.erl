-module(auth_token).
-behaviour(gen_server).

-export([init/1, terminate/2, start_link/1, handle_call/3, handle_cast/2]).
-export([sign/1, verify/1, decode/1]).

init(#{ hmac_key := HMACKey } = State) when is_binary(HMACKey) -> { ok, State }.

terminate(_Reason, _State) -> normal.

start_link(Args) -> gen_server:start_link(?MODULE, Args, []).

handle_call({ sign, UserID, Username, Permissions }, _From, #{ hmac_key := HMACKey} = State) ->
    Now = calendar:datetime_to_gregorian_seconds(calendar:universal_time()),
    SecondsInDay = 60 * 60 * 24,
    ExpTime = Now + SecondsInDay,
    Body = jsone:encode(
        #{ <<"user_id">> => UserID, <<"username">> => Username, <<"exp">> => ExpTime, <<"permissions">> => Permissions }
    ),
    Signature = crypto:mac(hmac, sha3_512, HMACKey, Body),
    AuthToken = <<(base64:encode(Body))/binary, <<".">>/binary, (base64:encode(Signature))/binary>>,
    { reply, AuthToken, State };
handle_call({ verify, null }, _From, State) ->
    { reply, no_auth_info, State };
handle_call({ verify, AuthToken }, _From, #{ hmac_key := HMACKey } = State) ->
    [BodyStr, TokenSignatureStr] = string:split(unicode:characters_to_list(AuthToken), "."),
    BodyJSON = base64:decode(BodyStr),
    TokenSignatureBin = base64:decode(TokenSignatureStr),
    ComputedSignature = crypto:mac(hmac, sha3_512, HMACKey, BodyJSON),
    Reply = if
        ComputedSignature =:= TokenSignatureBin ->
            Body = jsone:decode(BodyJSON),
            #{ <<"exp">> := ExpTime } = Body,
            Now = calendar:datetime_to_gregorian_seconds(calendar:universal_time()),
            if
                Now >= ExpTime -> expired;
                true ->
                    #{ <<"user_id">> := UserID, <<"username">> := Username, <<"permissions">> := Permissions } = Body,
                    { valid, { UserID, Username, Permissions, calendar:gregorian_seconds_to_datetime(ExpTime) } }
            end;
        true -> invalid_signature
    end,
    { reply, Reply, State };
handle_call({ decode, AuthToken }, _From, State) ->
    Body = jsone:decode(base64:decode(hd(string:split(unicode:characters_to_list(AuthToken), ".")))),
    #{
        <<"user_id">> := UserID, <<"username">> := Username, <<"permissions">> := Permissions, <<"exp">> := ExpTime
    } = Body,
    Reply = { UserID, Username, Permissions, calendar:gregorian_seconds_to_datetime(ExpTime) },
    { reply, Reply, State };
handle_call(_Request, _From, State) -> { noreply, State }.

handle_cast(_Request, State) -> { noreply, State }.

call(Request) -> poolboy:transaction(auth_token_pool, fun(Worker) -> gen_server:call(Worker, Request) end).

cast(Request) -> poolboy:transaction(auth_token_pool, fun(Worker) -> gen_server:cast(Worker, Request) end).

sign({ UserID, Username, Permissions }) when is_binary(UserID), is_binary(Username), is_list(Permissions)
    -> call({ sign, UserID, Username, Permissions }).

verify(AuthToken) when is_binary(AuthToken) -> call({ verify, AuthToken });
verify(null) -> call({ verify, null }).

decode(AuthToken) when is_binary(AuthToken) -> call({ decode, AuthToken }).
