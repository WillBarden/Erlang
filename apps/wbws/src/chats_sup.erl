-module(chats_sup).
-behaviour(supervisor).

-export([init/1, start_link/0]).

init([]) ->
    SupFlags = #{
        strategy => one_for_one,
        intensity => 5,
        period => 10
    },
    ChildSpecs = [
        #{ id => chat_listener, start => { chat_listener, start_link, [] } }
    ],
    { ok, { SupFlags, ChildSpecs } }.

start_link() -> supervisor:start_link(?MODULE, []).
