-module(users).
-behaviour(gen_server).

-export([init/1, terminate/2, start_link/1, handle_call/3, handle_cast/2]).
-export([id/1, exists/1, create/1, authenticate/1, update_credentials/1]).


init(Args) -> { ok, Args }.

terminate(_Reason, _State) -> normal.

start_link(Args) -> gen_server:start_link(?MODULE, Args, []).

handle_call({ id, Username }, _From, State) ->
    UserID = case db:equery("select id from users where username = $1", [Username]) of
        { ok, _Columns, [{ ID }]} -> ID;
        _ -> null
    end,
    { reply, UserID, State };
handle_call({ exists, UserID }, _From, State) ->
    UserExists = case db:equery("select count(id) from users where id = $1", [UserID]) of
        { ok, _Columns, [{ Count }]} -> Count =:= 1;
        _ -> false
    end,
    { reply, UserExists, State };
handle_call({ create, Username, Password }, _From, #{ sec_salt := SecSalt } = State) ->
    UsernameStr = unicode:characters_to_list(Username),
    PasswordStr = unicode:characters_to_list(Password),
    Reply = if
        length(UsernameStr) < 3 orelse length(UsernameStr) > 64 ->
            { invalid_username, "Username must be 3 to 64 characters long" };
        length(PasswordStr) < 8 orelse length(UsernameStr) > 64 ->
            { invalid_password, "Password must be 8 to 64 characters long" };
        true -> case id(Username) of
            ExistingID when is_binary(ExistingID) ->
                { invalid_username, io_lib:format("Username ~s already exists", [Username]) };
            null ->
                db:transact(fun(Conn) ->
                    { ok, _Count, _Columns, [{ UserID }] } = db:equery(
                        "insert into users (username, created) values ($1, $2) returning (id)",
                        [Username, calendar:universal_time()]
                    ),
                    Salt = crypto:strong_rand_bytes(64),
                    Hash = crypto:hash(sha3_512, <<Salt/binary, SecSalt/binary, Password/binary>>),
                    { ok, _Count } = db:equery(
                        "insert into passwords (user_id, salt, hash) values ($1, $2, $3)",
                        [UserID, base64:encode(Salt), base64:encode(Hash)]
                    ),
                    ok
                end)
        end
    end,
    { reply, Reply, State };
handle_call({ authenticate, Username, Password }, _From, #{ sec_salt := SecSalt } = State) ->
    Reply = case id(Username) of
        null -> no_user;
        UserID ->
            { ok, _Columns, [{ LastAuthAttempt }] } = db:equery(
                "select max(at) from auth_attempts where user_id = $1 and successful = false", [UserID]
            ),
            AuthLimitMet = case LastAuthAttempt of
                null -> false;
                AttemptTime -> % Now < (calendar:datetime_to_gregorian_seconds({ { Yr, Mn, Dy }, { Hr, Min, round(Sec) } }) + 5)
                    Now = calendar:datetime_to_gregorian_seconds(calendar:universal_time()),
                    Now < (calendar:datetime_to_gregorian_seconds(round_datetime(AttemptTime)) + 5)
            end,
            if
                AuthLimitMet -> auth_limit_met;
                not AuthLimitMet ->
                    { ok, _, [{ Salt, Hash }] } = db:equery(
                        "select salt, hash from passwords where user_id = $1", [UserID]
                    ),
                    ComputedHash = base64:encode(
                        crypto:hash(sha3_512, <<(base64:decode(Salt))/binary, SecSalt/binary, Password/binary>>)
                    ),
                    db:equery(
                        "insert into auth_attempts (user_id, successful, at) values ($1, $2, $3)",
                        [UserID, ComputedHash =:= Hash, calendar:universal_time()]
                    ),
                    if
                        ComputedHash =:= Hash -> { authenticated, auth_token:sign({ UserID, Username, [] }) };
                        ComputedHash =/= Hash -> invalid_credentials
                    end
            end
    end,
    { reply, Reply, State };
handle_call({ update_credentials, Username, Password, NewPassword }, _From, #{ sec_salt := SecSalt } = State) ->
    Reply = case id(Username) of
        null -> no_user;
        UserID ->
            { ok, _Columns, [{ LastChange }] } = db:equery(
                "select max(at) from password_changes where user_id = $1", [UserID]
            ),
            ChangeLimitMet = case LastChange of
                null -> false;
                ChangeTime -> % Now < (calendar:datetime_to_gregorian_seconds({ { Yr, Mn, Dy }, { Hr, Min, round(Sec) } }) + 5)
                    Now = calendar:datetime_to_gregorian_seconds(calendar:universal_time()),
                    Now < (calendar:datetime_to_gregorian_seconds(round_datetime(ChangeTime)) + 60)
            end,
            if
                ChangeLimitMet -> change_limit_met;
                not ChangeLimitMet ->
                    { ok, _, [{ Salt, Hash }] } = db:equery(
                        "select salt, hash from passwords where user_id = $1", [UserID]
                    ),
                    ComputedHash = base64:encode(
                        crypto:hash(sha3_512, <<(base64:decode(Salt))/binary, SecSalt/binary, Password/binary>>)
                    ),
                    if
                        ComputedHash =:= Hash ->
                            NewPasswordStr = unicode:characters_to_list(NewPassword),
                            if
                                length(NewPasswordStr) < 8 orelse length(NewPasswordStr) > 64 ->
                                    { invalid_password, "Password must be 8 to 64 characters long" };
                                true ->
                                    NewSalt = crypto:strong_rand_bytes(64),
                                    NewHash = crypto:hash(
                                        sha3_512, <<NewSalt/binary, SecSalt/binary, NewPassword/binary>>
                                    ),
                                    db:transact(fun(Conn) ->
                                        db:equery(
                                            Conn,
                                            "update passwords set salt = $1, hash = $2 where user_id = $3",
                                            [base64:encode(NewSalt), base64:encode(NewHash), UserID]
                                        ),
                                        db:equery(
                                            Conn,
                                            "insert into password_changes (user_id, at) values ($1, $2)",
                                            [UserID, calendar:universal_time()]
                                        ),
                                        ok
                                    end)
                            end;
                        ComputedHash =/= Hash -> invalid_credentials
                    end
            end
    end,
    { reply, Reply, State };
handle_call(_Request, _From, State) -> { noreply, State }.

handle_cast(_Request, State) -> { noreply, State }.

call(Request) -> poolboy:transaction(users_worker_pool, fun(Worker) -> gen_server:call(Worker, Request) end).

cast(Request) -> poolboy:transaction(users_worker_pool, fun(Worker) -> gen_server:cast(Worker, Request) end).

id(Username) when is_binary(Username) -> call({ id, Username }).

exists(UserID) when is_binary(UserID) -> call({ exists, UserID }).

create({ Username, Password }) -> call({ create, Username, Password }).

authenticate({ Username, Password }) -> call({ authenticate, Username, Password }).

update_credentials({ Username, Password, NewPassword }) ->
    call({ update_credentials, Username, Password, NewPassword }).

round_datetime({ { Yr, Mon, Day }, { Hr, Min, Sec } }) ->
    { { Yr, Mon, Day }, { Hr, Min, round(Sec)} }.
