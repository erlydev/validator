%%% ==================================================================
%%% @author Oleksandr Boiko <erlydev@gmail.com>
%%% @copyright 2017, Oleksandr Boiko <erlydev@gmail.com>
%%% @doc
%%% ...
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

-ifndef(VALIDATOR_HRL).
-define(VALIDATOR_HRL, 1).

%%% ==================================================================
%%% Helpers Macro
%%% ==================================================================

%% http://www.regular-expressions.info/examples.html

-define(RE_NAME, <<"^[a-zA-Z0-9_-]+$">>).
-define(RE_EMAIL, <<"^[a-zA-Z0-9.!#$%&'*+/=?^_`{|}~-]+@[a-zA-Z0-9-]+(?:\\.[a-zA-Z0-9-]+)*$">>).
-define(RE_PHONE_NUM, <<"^\\+?[0-9\s()-]{3,}$">>).

-define(RE_IP4, <<"^(\\d{1,3}\\.){3}\\d{1,3}$">>).
-define(RE_IP4_AND_PORT, <<"^(\\d{1,3}\\.){3}\\d{1,3}:\\d{1,5}$">>).
-define(RE_IP4_AND_OPTIONAL_PORT, <<"^(\\d{1,3}\\.){3}\\d{1,3}(:\\d{1,5})?$">>).

%% TODO: more regular expressions!

%%% ==================================================================
%%% Records
%%% ==================================================================

-record(vrule, {
  field           = undefined :: binary(),
  type            = undefined :: atom(),
  size            = {infinity, infinity} :: min_max(),
  allowed_values  = undefined :: list(),
  regex           = undefined :: binary(),
  optional        = false :: boolean(),
  validate_with   = undefined :: vrule() | function() | mfa()
}).

%%% ==================================================================
%%% Data Types
%%% ==================================================================

-type vrule() :: #vrule{}.
-type data_type() :: integer | float | number | bin_integer | bin_float
                  | binary | list | tuple | map | atom | function
                  | pid | port | ref | boolean.
-type min_max() :: {Min :: number() | infinity, Max :: number() | infinity}.

-export_type([
  vrule/0,
  data_type/0,
  min_max/0
]).

-endif.