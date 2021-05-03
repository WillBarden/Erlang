-module(tables).

-export([init/1, terminate/2, start_link/1, handle_call/3, handle_cast/2, handle_info/2]).

-compile([export_all]).

init(Args) -> { ok, Args }.

terminate(_Reason, _State) -> normal.

start_link(Args) -> gen_server:start_link(?MODULE, Args, []).

handle_call({ create, Owner, Seats, Name }, _From, State) ->
    NameStr = unicode:characters_to_list(Name),
    UserExists = users:exists(Owner),
    Reply = if
        UserExists =:= false -> no_user;
        Seats < 2 -> { invalid_seat_count, "Table must have at least 2 seats" };
        Seats > 10 -> { invalid_seat_count, "Table cannot have more than 10 seats" };
        length(NameStr) > 256 -> { invalid_name, "Table name must be 256 characters or less" };
        true ->
            { ok, _Columns, [{ OwnedTablesCount }] } = db:equery(
                "select count(*) from tables where owner = $1", [Owner]
            ),
            if
                OwnedTablesCount =:= 8 -> { table_limit_met, "Users may not own more than 8 tables" };
                true ->
                    case db:equery(
                        "insert into tables (owner, name, seats)"
                        "values ($1, $2, $3)"
                        "returning (id)",
                        [Owner, Name, Seats]
                    ) of
                        { ok, _Count, _Columns, [{ TableID }] } -> { ok, TableID };
                        { error, Error } -> { error, Error }
                    end
            end
    end,
    { reply, Reply, State };
handle_call({ message, TableID, Sender, Message }, _From, State) ->
    TableExists = tables:exists(TableID),
    SenderExists = users:exists(Sender),
    MessageStr = unicode:characters_to_list(Message),
    Reply = if
        TableExists =:= false -> no_table;
        SenderExists =:= false -> no_sender;
        length(MessageStr) > 2048 -> { invalid_message, "Messages must be no longer than 2048 characters" };
        true ->
            { ok, 1 } = db:equery(
                "insert into chat_messages (table_id, sender, msg) values ($1, $2, $3)",
                [TableID, Sender, Message]
            ),
            sent
    end,
    { reply, Reply, State };
handle_call({ exists, TableID }, _From, State) ->
    TableExists = case db:equery("select count(id) from tables where id = $1", [TableID]) of
        { ok, _Columns, [{ Count }]} -> Count =:= 1;
        _ -> false
    end,
    { reply, TableExists, State };
handle_call(_Request, _From, State) -> { noreply, State }.

handle_cast(_Request, State) -> { noreply, State }.

handle_info(Info, State) -> { noreply, State }.

call(Request) -> poolboy:transaction(tables_pool, fun(Worker) -> gen_server:call(Worker, Request) end).

cast(Request) -> poolboy:transaction(tables_pool, fun(Worker) -> gen_server:cast(Worker, Request) end).

create({ Owner, Seats, Name }) when is_binary(Owner), is_integer(Seats), is_binary(Name) ->
    call({ create, Owner, Seats, Name }).

exists(TableID) when is_integer(TableID) -> call({ exists, TableID }).

message({ TableID, Sender, Message }) when is_integer(TableID), is_binary(Sender), is_binary(Message)
    -> call({ message, TableID, Sender, Message}).
