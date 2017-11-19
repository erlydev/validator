%%% ==================================================================
%%% @author Oleksandr Boiko <erlydev@gmail.com>
%%% @copyright 2017, Oleksandr Boiko <erlydev@gmail.com>
%%% @doc
%%% Supervisor tree.
%%% TODO: we can cache compiled regular expressions.
%%% @end
%%% ==================================================================
%%% Copyright (c) 2017, Oleksandr Boiko <erlydev@gmail.com>
%%%
%%% Permission is hereby granted, free of charge, to any person obtaining a copy
%%% of this software and associated documentation files (the "Software"), to deal
%%% in the Software without restriction, including without limitation the rights
%%% to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
%%% copies of the Software, and to permit persons to whom the Software is
%%% furnished to do so, subject to the following conditions:
%%%
%%% The above copyright notice and this permission notice shall be included in all
%%% copies or substantial portions of the Software.
%%%
%%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
%%% IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
%%% FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
%%% AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
%%% LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
%%% OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
%%% SOFTWARE.
%%% ==================================================================

-module(validator_sup).
-behaviour(supervisor).

%% Callbacks
-export([
  start_link/0,
  init/1
]).

-author("Oleksandr Boiko, erlydev@gmail.com").

%%% ==================================================================
%%% Macro
%%% ==================================================================

-define(CHILD(Mod), {Mod, {Mod, start_link, []}, permanent, brutal_kill, worker, [Mod]}).
-define(CHILD(Mod, Opts), {Mod, {Mod, start_link, Opts}, permanent, brutal_kill, worker, [Mod]}).

%%% ==================================================================
%%% Callback functions
%%% ==================================================================

%% -------------------------------------------------------------------
%% @private
%% -------------------------------------------------------------------
-spec start_link() -> supervisor:startlink_ret().

start_link() ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% -------------------------------------------------------------------
%% @private
%% -------------------------------------------------------------------
-spec init(term()) -> {'ok', tuple()}.

init([]) ->
  RestartStrategy = one_for_one,
  MaxR = 10,
  MaxT = 1,
  Childs = [],
  {ok, {{RestartStrategy, MaxR, MaxT}, Childs}}.
