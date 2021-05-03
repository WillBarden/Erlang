-module(chat_listener).

-export([init/1, terminate/2, start_link/0, handle_call/3, handle_cast/2, handle_info/2]).

-compile([export_all]).

init(_Args) ->
    State = #{
        listeners => gb_trees:empty()    
    },
    { ok, State }.

terminate(_Reason, _State) -> normal.

start_link() -> gen_server:start_link({ local, ?MODULE }, ?MODULE, [], []).

handle_call({ add, TableID, ListenerPID }, _From, #{ listeners := Listeners } = State) ->
    TableListenersSet = case gb_trees:lookup(TableID, Listeners) of
        { value, ListenerSet } -> ListenerSet;
        none -> sets:new()    
    end,
    UpdatedListeners = gb_trees:enter(TableID, sets:add_element(ListenerPID, TableListenersSet), Listeners),
    { reply, ok, State#{ listeners => UpdatedListeners } };
handle_call({ get, TableID }, _From, #{ listeners := Listeners } = State) ->
    TableListenersSet = case gb_trees:lookup(TableID, Listeners) of
        { value, ListenerSet } -> ListenerSet;
        none -> sets:new()    
    end,
    { reply, TableListenersSet, State };
handle_call({ clear, TableID }, _From, #{ listeners := Listeners } = State) ->
    { reply, ok, State#{ listeners => gb_trees:delete_any(TableID, Listeners) } };
handle_call({ remove, TableID, ListenerPID }, _From, #{ listeners := Listeners } = State) ->
    TableListeners = case gb_trees:lookup(TableID, Listeners) of
        { value, TableListenerSet } -> TableListenerSet;
        none -> sets:new()
    end,
    UpdatedTableListeners = sets:del_element(ListenerPID, TableListeners),
    UpdatedListeners = case sets:is_empty(UpdatedTableListeners) of
        true -> gb_trees:delete_any(TableID, Listeners);
        false -> gb_trees:enter(TableID, UpdatedTableListeners, Listeners)
    end,
    { reply, ok, State#{ listeners => UpdatedListeners } };
handle_call({ dispatch, TableID, Message }, _From, #{ listeners := Listeners } = State) ->
    TableListeners = case gb_trees:lookup(TableID, Listeners) of
        { value, TableListenerSet } -> TableListenerSet;
        none -> sets:new()
    end,
    list:foreach(fun(ListeningPID) -> ListeningPID ! Message end, sets:to_list(TableListeners)),
    { reply, ok, State }.

handle_cast(_Request, State) -> { noreply, State }.

handle_info(Info, State) -> { noreply, State }.

call(Request) -> gen_server:call(?MODULE, Request).

add({ TableID, ListenerPID }) when is_integer(TableID), is_pid(ListenerPID) -> call({ add, TableID, ListenerPID }).

get(TableID) when is_integer(TableID) -> call({ get, TableID }).

clear(TableID) when is_integer(TableID) -> call({ clear, TableID }).

remove(TableID, ListenerPID) when is_integer(TableID), is_pid(ListenerPID) -> call({ remove, TableID, ListenerPID }).

dispatch(TableID, Message) when is_integer(TableID) -> call({ dispatch, TableID, Message }).
