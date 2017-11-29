%%%-------------------------------------------------------------------
%% @doc xm_up_config top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(xm_up_config_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).
-define(MXMUPCONFIG,xm_up_config).

%%====================================================================
%% API functions
%%====================================================================

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
init([]) ->
    XmUpConfig = {?MXMUPCONFIG,
        {?MXMUPCONFIG, start_link, []},
        permanent, 2000, worker, [?MXMUPCONFIG]},

    Children = [XmUpConfig],
    RestartStrategy = {one_for_one, 4, 60},
    {ok, {RestartStrategy, Children}}.

%%====================================================================
%% Internal functions
%%====================================================================
