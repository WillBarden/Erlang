-module(wbws_sup).

-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    SupFlags = #{ strategy => one_for_one, intensity => 5, period => 10 },
    ChildSpecs = [
        #{ id => users_sup, start => { users_sup, start_link, [] } },
        #{ id => db_sup, start => { db_sup, start_link, []} },
        #{ id => auth_token_sup, start => { auth_token_sup, start_link, [] } }
    ],
    { ok, { SupFlags, ChildSpecs } }.
