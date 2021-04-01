-module(users).
-behaviour(gen_server).

-export([init/1, handle_call/3, handle_cast/2, terminate/2]).
-export([start_link/0, create/1, authenticate/1, verify/1]).
-export([valid_username/1, valid_password/1, get_last_auth_attempt/2, verify_token/2]).

init(_Args) ->
    Conn = db:connect(),
    { ok, HMACKey}  = list_to_binary(os:getenv("HMAC_KEY")),
    { ok, SecSalt } = list_to_binary(os:getenv("SEC_SALT")),
    { ok, #{ conn => Conn, hmac_key => HMACKey, sec_salt => SecSalt }}.

handle_call({ users_register, Username, InitPassword } = _Request, _From, State) -> 
    { reply, create({ Username, InitPassword }, State), State };
handle_call({ users_auth, Username, Password } = _Request, _From, State) -> 
    { reply, authenticate({ Username, Password }, State), State };
handle_call({ users_verify_token, Token } = _Request, _From, State) -> 
    { reply, verify({ Token }, State), State }.

handle_cast(_Request, _State) -> { noreply, _State }.

terminate(_Reason, #{ conn := Conn }) ->
    epgsql:close(Conn),
    normal.

start_link() -> gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

create({ Username, InitPassword }) when is_binary(Username), is_binary(InitPassword) ->
    gen_server:call(?MODULE, { users_register, Username, InitPassword }).

create({ Username, InitPassword }, #{ conn := Conn, sec_salt := SecSalt }) ->
    UserID = get_user_id(Conn, Username),
    case UserID of
        null -> case valid_username(Username) of
            true -> case valid_password(InitPassword) of
                true ->
                    epgsql:with_transaction(
                        Conn,
                        fun(TConn) ->
                            NewUserID = insert_user(TConn, Username),
                            insert_password(TConn, NewUserID, InitPassword, SecSalt),
                            insert_password_change(TConn, NewUserID),
                            ok
                        end,
                        []
                    );
                { error, Error } -> { error, Error }
            end;
            { error, Error } -> { error, Error }
        end;
        _ -> { error, io_lib:format("Username ~s already exists", [Username]) }
    end.

authenticate({ Username, Password }) when is_binary(Username), is_binary(Password) ->
    gen_server:call(?MODULE, { users_auth, Username, Password }).

authenticate({ Username, Password }, #{ conn := Conn, sec_salt := SecSalt, hmac_key := HMACKey }) ->
    case get_user_id(Conn, Username) of
        UserID ->
            % Make sure a user can't login more than once every 5 seconds
            Now = calendar:datetime_to_gregorian_seconds(calendar:universal_time()),
            AuthLimitMet = case get_last_auth_attempt(Conn, UserID) of
                null -> false;
                LastAuthAttempt -> Now < (calendar:datetime_to_gregorian_seconds(LastAuthAttempt) + 5)
            end,
            if
                not AuthLimitMet ->
                    case verify_password(Conn, UserID, Password, SecSalt) of 
                        true ->
                            insert_auth_attempt(Conn, UserID, true),
                            create_token(Conn, UserID, HMACKey);
                        false ->
                            insert_auth_attempt(Conn, UserID, false),
                            { error, io_lib:format("Invalid password for user \"~s\"~n", [Username]) }
                    end;
                true -> { error, "Too many authentication attempts, try again later" }
            end;
        null -> { error, io_lib:format("No user exists with username \"~s\"~n", [Username]) }
    end.

verify({ Token }) when is_binary(Token) ->
    gen_server:call(?MODULE, { users_verify_token, Token }).

verify({ Token }, #{ conn := Conn, hmac_key := HMACKey }) ->
    case verify_token(HMACKey, Token) of
        TokenInfo when is_map(TokenInfo) ->
            % If the token expires within the next hour, refresh it
            #{ user_id := UserID, exp := ExpTime } = TokenInfo,
            Now = calendar:datetime_to_gregorian_seconds(calendar:universal_time()),
            if
                Now > (ExpTime - (60 * 60)) andalso Now < ExpTime ->
                    FreshToken = create_token(Conn, UserID, HMACKey),
                    { verify_token(HMACKey, FreshToken), FreshToken };
                true -> { TokenInfo, Token }
            end;
        { error, Error } -> { error, Error }
    end.

get_user_id(Conn, Username) ->
    case epgsql:equery(Conn, "select id from users where username = $1", [Username]) of
        { ok, _Columns, Rows } -> case length(Rows) of
            0 -> null;
            1 -> element(1, hd(Rows))
        end;
        _ -> null
    end.

insert_user(Conn, Username) ->
    Result = epgsql:equery(
        Conn,
        "insert into users (username, created) values ($1, $2) returning (id)",
        [Username, calendar:universal_time()]
    ),
    case Result of
        { ok, _Count, _Columns, Rows } ->  element(1, hd(Rows));
        { error, Error } -> { error, Error }
    end.

insert_password(Conn, UserID, Password, SecSalt) ->
    Salt = crypto:strong_rand_bytes(64),
    Hash = crypto:hash(sha3_512, <<Salt/binary, SecSalt/binary, Password/binary>>),
    Result = epgsql:equery(
        Conn, 
        "insert into passwords (user_id, salt, hash) values ($1, $2, $3)",
        [UserID, base64:encode(Salt), base64:encode(Hash)]
    ),
    case Result of 
        { ok, _Count } -> ok;
        { error, Error } -> { error, Error }
    end.

insert_auth_attempt(Conn, UserID, Successful) when is_boolean(Successful) ->
    Result = epgsql:equery(
        Conn,
        "insert into auth_attempts (user_id, successful, at) values ($1, $2, $3)",
        [UserID, Successful, calendar:universal_time()]
    ),
    case Result of
        { ok, _Count } -> ok;
        { error, Error } -> { error, Error }
    end.

get_last_auth_attempt(Conn, UserID) ->
    case epgsql:equery(Conn, "select max(at) from auth_attempts where user_id = $1", [UserID]) of
        { ok, _Columns, Rows } -> case length(Rows) of
            1 -> element(1, hd(Rows));
            _ -> null
        end;
        _ -> null
    end.

insert_password_change(Conn, UserID) ->
    case epgsql:equery(
        Conn,
        "insert into password_changes (user_id, at) values ($1, $2)",
        [UserID, calendar:universal_time()]
    ) of
        { ok, _Count } -> ok;
        { error, Error } -> { error, Error }
    end.

verify_password(Conn, UserID, Password, SecSalt) ->
    case epgsql:equery(Conn, "select salt, hash from passwords where user_id = $1", [UserID]) of
        { ok, _Columns, Rows } -> if
            length(Rows) == 1 ->
                { Salt, TargetHash } = hd(Rows),
                Hash = base64:encode(
                    crypto:hash(sha3_512, <<(base64:decode(Salt))/binary, SecSalt/binary, Password/binary>>)
                ),
                Hash == TargetHash;
            true -> false
        end;
        _ -> false
    end.

create_token(Conn, UserID, HMACKey) ->
    case epgsql:equery(Conn, "select username from users where id = $1", [UserID]) of
        { ok, _Columns, Rows } -> if
            length(Rows) == 1 ->
                { Username } = hd(Rows),
                Now = calendar:datetime_to_gregorian_seconds(calendar:universal_time()),
                ExpTime = Now + (60 * 60 * 24), % Expire in one day
                Body = jsone:encode(#{
                    <<"user_id">> => UserID,
                    <<"username">> => Username,
                    <<"exp">> => ExpTime
                }),
                Signature = crypto:mac(hmac, sha3_512, HMACKey, Body),
                <<(base64:encode(Body))/binary, <<".">>/binary, (base64:encode(Signature))/binary>>;
            true -> { error, "Could not create auth token" }
        end;
        _ -> { error, "Failed to retrieve user information" }
    end.

verify_token(HMACKey, Token) ->
    [BodyStr, TokenSignatureStr] = string:split(unicode:characters_to_list(Token), "."),
    BodyJSON = base64:decode(BodyStr),
    TokenSignatureBin = base64:decode(TokenSignatureStr),
    ComputedSignature = crypto:mac(hmac, sha3_512, HMACKey, BodyJSON),
    if
        ComputedSignature == TokenSignatureBin ->
            Body = jsone:decode(BodyJSON),
            #{ <<"exp">> := ExpTime } = Body,
            Now = calendar:datetime_to_gregorian_seconds(calendar:universal_time()),
            if
                Now >= ExpTime -> { error, "Token expired" };
                true -> #{ <<"user_id">> := UserID, <<"username">> := Username } = Body,
                    #{ user_id => UserID, username => Username, exp => ExpTime }
            end;
        true -> { error, "Invalid signature" }
    end.

valid_username(Username) ->
    case unicode:characters_to_list(Username) of
        UnicodeUsername when is_list(UnicodeUsername) -> if
            length(UnicodeUsername) < 3 -> { error, "Username must be at least 3 characters long" };
            length(UnicodeUsername) > 64 -> { error, "Username must be no longer than 64 characters" };
            true -> true
        end;
        _ -> { error, "Username must be a valid unicode string" }
    end.

valid_password(Password) ->
    case unicode:characters_to_list(Password) of
        UnicodePassword when is_list(UnicodePassword) -> if
            length(UnicodePassword) < 8 -> { error, "Password must be at least 8 characters long" };
            length(UnicodePassword) > 64 -> { error, "Password must be no longer than 64 characters" };
            true -> true
        end;
        _ -> { error, "Password must be a valid unicode string" }
    end.
