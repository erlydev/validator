%%% ==================================================================
%%% @author Oleksandr Boiko <erlydev@gmail.com>
%%% @copyright 2017, Oleksandr Boiko <erlydev@gmail.com>
%%% @doc
%%% Validation tool.
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

-module(validator).

%% API
-export([
  validate/2,
  validate_bulk/2,
  validate_bulk/3
]).

-author("Oleksandr Boiko, erlydev@gmail.com").


%%% ==================================================================
%%% Includes
%%% ==================================================================

-include("validator.hrl").


%%% ==================================================================
%%% API functions
%%% ==================================================================

%% -------------------------------------------------------------------
%% @doc
%% Size for binary is bytes.
%% For function 'size' means arity.
%%
%% Priority: validate_with_mfa > allowed_values > regex > type, size
%% @end
%% -------------------------------------------------------------------
-spec validate(Data, Rules) -> ok | {error, Reason} when
  Data :: proplists:proplist() | maps:map(),
  Rules :: [vrule(), ...],
  Reason :: binary().

%% TODO: Opts :: [Opt], Opt :: repack | trim | {on_error, stop | continue}
%% TODO: validate_with = [vrule()] | fun M:F/1 -> boolean()
validate(Data, [Rule | Rules]) ->
  #vrule{
    field = Field,
    type = Type_,
    size = Size,
    allowed_values = AllowedValues,
    regex = RegExp,
    optional = Optional,
    validate_with = _ValidateWith
  } = Rule,
  case {get_val(Field, Data), Optional} of
    {undefined, true} ->
      validate(Data, Rules);
    {undefined, false} ->
      {error, <<"Required field '", Field/binary, "'.">>};
    {Val, _} ->
      Type =
        case Type_ of
          undefined ->
            get_type(Val);
          _ ->
            Type_
        end,
      IsValidType =
        case is_valid_type(Val, Type) of
          true ->
            true;
          false ->
            case {AllowedValues, RegExp} of
              {[_|_], _} ->
                % Ignore type when allowed values is specified
                true;
              {_, RegExp} when is_binary(RegExp) ->
                % Ignore type when regular expression is specified
                true;
              {_, _} ->
                false
            end
        end,
      IsValidSize =
        case is_valid_size(Val, Type, Size) of
          true ->
            true;
          false ->
            case AllowedValues of
              [_|_] ->
                % Ignore size limits when allowed values is specified
                true;
              _ ->
                false
            end
        end,
      IsValidValue =
        case is_valid_value(Val, AllowedValues, RegExp) of
          true ->
            true;
          false ->
            case {AllowedValues, RegExp} of
              {undefined, undefined} ->
                % We returns IsValidType when value is not limited
                IsValidType;
              _ ->
                false
            end
        end,
      Result = [
        {<<"type">>, IsValidType},
        {<<"size">>, IsValidSize},
        {<<"value">>, IsValidValue}
      ],
      case [K || {K, V} <- Result, V == false] of
        [] ->
          validate(Data, Rules);
        [_|_] = Reasons ->
%%          Msg =
%%            case AllowedValues of
%%              [_|_] ->
%%                Suffix = <<"Allowed values: ", (list_to_binary(io_lib:format("~p", [AllowedValues])))/binary>>,
%%                <<(error_msg(Field, Reasons))/binary, Suffix>>;
%%              _ ->
%%                error_msg(Field, Reasons)
%%            end,
          {error, error_msg(Field, Reasons)}
      end
  end;

validate(_, []) ->
  ok.

%% -------------------------------------------------------------------
%% @doc
%% ...
%% @end
%% -------------------------------------------------------------------
-spec validate_bulk(Data, Rules) -> ok | {error, Reason} when
  Data :: [proplists:proplist() | maps:map(), ...],
  Rules :: [vrule(), ...],
  Reason :: binary().

validate_bulk(Data, Rules) ->
  validate_bulk(Data, Rules, []).

%% -------------------------------------------------------------------
%% @doc
%% ...
%% @end
%% -------------------------------------------------------------------
-spec validate_bulk(Data, Rules, Opts) -> ok | {error, Reason}
  | {report, ValidData, InvalidData} when
  Data :: [proplists:proplist() | maps:map(), ...],
  Rules :: [vrule(), ...],
  Opts :: [Opt],
  Opt :: {on_error, stop | continue},
  Reason :: binary(),
  ValidData :: [proplists:proplist() | maps:map(), ...],
  InvalidData :: [{Reason, proplists:proplist() | maps:map()}, ...].

validate_bulk(Data, Rules, Opts) ->
  OnErrorFlag = proplists:get_value(on_error, Opts, stop),
  validate_bulk(Data, Rules, OnErrorFlag, [], []).


%%% ==================================================================
%%% Internal functions
%%% ==================================================================

%% -------------------------------------------------------------------
%% @private
%% -------------------------------------------------------------------
-spec validate_bulk(Data, Rules, OnErrorFlag, ValidData, InvalidData) ->
  ok | {error, Reason} | {report, ValidData, InvalidData} when
  Data :: [proplists:proplist() | maps:map(), ...],
  Rules :: [vrule(), ...],
  OnErrorFlag :: stop | continue,
  ValidData :: [proplists:proplist() | maps:map(), ...],
  InvalidData :: [{Reason, proplists:proplist() | maps:map()}, ...],
  Reason :: binary().

validate_bulk([H | T], Rules, stop, [], []) ->
  case validate(H, Rules) of
    ok ->
      validate_bulk(T, Rules, stop, [], []);
    {error, Reason} ->
      {error, Reason}
  end;

validate_bulk([H | T], Rules, continue, ValidData, InvalidData) ->
  case validate(H, Rules) of
    ok ->
      validate_bulk(T, Rules, continue, [H | ValidData], InvalidData);
    {error, Reason} ->
      validate_bulk(T, Rules, continue, ValidData, [{H, Reason} | InvalidData])
  end;

validate_bulk([], _, continue, ValidData, InvalidData) ->
  {report, ValidData, InvalidData};

validate_bulk([], _, _, _, _) ->
  ok.

%% -------------------------------------------------------------------
%% @private
%% -------------------------------------------------------------------
-spec is_valid_type(Val, Type) -> boolean() when
  Val :: term(),
  Type :: data_type().

is_valid_type(X, integer) ->
  is_integer(X);

is_valid_type(X, float) ->
  is_float(X);

is_valid_type(X, number) ->
  is_number(X);

is_valid_type(X, binary) ->
  is_binary(X);

is_valid_type(X, list) ->
  is_list(X);

is_valid_type(X, tuple) ->
  is_tuple(X);

is_valid_type(X, map) ->
  is_map(X);

is_valid_type(X, atom) ->
  is_atom(X);

is_valid_type(X, boolean) ->
  is_boolean(X);

is_valid_type({M, F, A}, function) ->
  erlang:function_exported(M, F, A);

is_valid_type(X, function) ->
  is_function(X);

is_valid_type(X, pid) ->
  is_pid(X);

is_valid_type(X, port) ->
  is_port(X);

is_valid_type(X, ref) ->
  is_reference(X);

is_valid_type(_, _) ->
  false.

%% -------------------------------------------------------------------
%% @private
%% -------------------------------------------------------------------
-spec is_valid_size(Val, Type, MinMax) -> boolean() when
  Val :: term(),
  Type :: atom(),
  MinMax :: min_max().

is_valid_size(X, _, {Min, Max}) when is_number(X),
  is_number(Min), is_number(Max), Min =< Max ->
    X >= Min andalso X =< Max;

is_valid_size(X, _, {infinity, Max}) when is_number(X),
  is_number(Max) ->
    X =< Max;

is_valid_size(X, _, {Min, infinity}) when is_number(X),
  is_number(Min) ->
    X >= Min;

is_valid_size(X, binary, {Min, Max}) when is_binary(X),
  is_integer(Min), is_integer(Max), Min =< Max ->
    N = erlang:size(X),
    N >= Min andalso N =< Max;

is_valid_size(X, binary, {infinity, Max}) when is_binary(X),
  is_integer(Max) ->
    erlang:size(X) =< Max;

is_valid_size(X, binary, {Min, infinity}) when is_binary(X),
  is_integer(Min) ->
    erlang:size(X) >= Min;

is_valid_size(X, tuple, {Min, Max}) when is_tuple(X), is_integer(Min),
  is_integer(Max), Min =< Max ->
    N = erlang:size(X),
    N >= Min andalso N =< Max;

is_valid_size(X, tuple, {infinity, Max}) when is_tuple(X),
  is_integer(Max) ->
    erlang:size(X) =< Max;

is_valid_size(X, tuple, {Min, infinity}) when is_tuple(X),
  is_integer(Min) ->
    erlang:size(X) >= Min;

is_valid_size(X, list, {Min, Max}) when is_list(X), is_integer(Min),
  is_integer(Max), Min =< Max ->
    N = erlang:length(X),
    N >= Min andalso N =< Max;

is_valid_size(X, list, {infinity, Max}) when is_list(X),
  is_integer(Max) ->
    erlang:length(X) =< Max;

is_valid_size(X, list, {Min, infinity}) when is_list(X),
  is_integer(Min) ->
    erlang:length(X) >= Min;

is_valid_size(X, map, {Min, Max}) when is_map(X), is_integer(Min),
  is_integer(Max) ->
    N = maps:size(X),
    N >= Min andalso N =< Max;

is_valid_size(X, map, {infinity, Max}) when is_map(X),
  is_integer(Max) ->
    maps:size(X) =< Max;

is_valid_size(X, map, {Min, infinity}) when is_map(X),
  is_integer(Min) ->
    maps:size(X) >= Min;

is_valid_size({M, F, A}, function, {infinity, Max}) when is_atom(M),
  is_atom(F), is_integer(A), is_integer(Max) ->
  erlang:function_exported(M, F, A) andalso A =< Max;

is_valid_size({M, F, A}, function, {Min, infinity}) when is_atom(M),
  is_atom(F), is_integer(A), is_integer(Min) ->
  erlang:function_exported(M, F, A) andalso A >= Min;

is_valid_size({M, F, A}, function, {Min, Max}) when is_atom(M),
  is_atom(F), is_integer(A), is_integer(Min), is_integer(Max) ->
    erlang:function_exported(M, F, A) andalso A >= Min andalso A =< Max;

is_valid_size(X, function, {infinity, Max}) when is_function(X) ->
  {arity, N} = erlang:fun_info(X, arity),
  N =< Max;

is_valid_size(X, function, {Min, infinity}) when is_function(X) ->
  {arity, N} = erlang:fun_info(X, arity),
  N >= Min;

is_valid_size(X, function, {Min, Max}) when is_function(X),
  is_integer(Min), is_integer(Max), Min =< Max ->
    {arity, N} = erlang:fun_info(X, arity),
    N >= Min andalso N =< Max;

is_valid_size(X, atom, _) when is_atom(X) ->
  true;

is_valid_size(X, boolean, _) when is_boolean(X) ->
  true;

is_valid_size(X, pid, _) when is_pid(X) ->
  true;

is_valid_size(X, port, _) when is_port(X) ->
  true;

is_valid_size(X, ref, _) when is_reference(X) ->
  true;

is_valid_size(_, _, {infinity, infinity}) ->
  true;

is_valid_size(_, _, _) ->
  false.

%% -------------------------------------------------------------------
%% @private
%% AllowedValues > RegExp
%% AllowedValues > Type
%% -------------------------------------------------------------------
-spec is_valid_value(X, AllowedValues, RegExp) -> boolean() when
  X :: term(),
  AllowedValues :: [AllowedValue],
  AllowedValue :: term(),
  RegExp :: binary().

%% TODO: is_valid_value(X, AllowedValues, RegExp, ValidationRules)
is_valid_value(X, [_|_] = AllowedValues, _) ->
  lists:member(X, AllowedValues);

is_valid_value(X, _, RegExp) when is_binary(RegExp), RegExp /= <<>> ->
  re:run(X, RegExp, [{capture, none}]) =:= match;

is_valid_value(_, _, _) ->
  false.


%%% ==================================================================
%%% Utils
%%% ==================================================================

%% -------------------------------------------------------------------
%% @private
%% -------------------------------------------------------------------
-spec get_val(K, X) -> Val | undefined when
  K :: term(),
  X :: proplists:proplist() | maps:map(),
  Val :: term().

get_val(K, X) ->
  get_val(K, X, undefined).

%% -------------------------------------------------------------------
%% @private
%% -------------------------------------------------------------------
-spec get_val(K, Struct, Default) -> Val | Default when
  K :: term(),
  Struct :: maps:map() | proplists:proplist(),
  Val :: term(),
  Default :: term().

get_val(K, Struct, Default) when is_map(Struct) ->
  maps:get(K, Struct, Default);

get_val(K, Struct, Default) when is_list(Struct) ->
  proplists:get_value(K, Struct, Default).

%%%% -------------------------------------------------------------------
%%%% @private
%%%% -------------------------------------------------------------------
%%-spec to_bin(binary() | integer() | float() | atom()) -> binary().
%%
%%to_bin(X) when is_binary(X) ->
%%  X;
%%
%%to_bin(X) when is_integer(X) ->
%%  integer_to_binary(X);
%%
%%to_bin(X) when is_float(X) ->
%%  float_to_binary(X, [{decimals, 4}, compact]);
%%
%%to_bin(X) when is_atom(X) ->
%%  atom_to_binary(X, utf8).

%% -------------------------------------------------------------------
%% @private
%% -------------------------------------------------------------------
-spec get_type(Val :: term()) -> Type :: atom().

get_type(X) when is_integer(X) ->
  integer;

get_type(X) when is_float(X) ->
  float;

get_type(X) when is_number(X) ->
  number;

get_type(X) when is_binary(X) ->
  binary;

get_type(X) when is_list(X) ->
  list;

get_type(X) when is_tuple(X) ->
  tuple;

get_type(X) when is_map(X) ->
  map;

get_type(X) when is_boolean(X) ->
  boolean;

get_type(X) when is_atom(X) ->
  atom;

get_type(X) when is_function(X) ->
  function;

get_type(X) when is_pid(X) ->
  pid;

get_type(X) when is_port(X) ->
  port;

get_type(X) when is_reference(X) ->
  ref;

get_type(_) ->
  undefined.

%% -------------------------------------------------------------------
%% @private
%% -------------------------------------------------------------------
-spec error_msg(FieldName :: binary(), Reasons :: [binary()]) ->
  Msg :: binary().

error_msg(FieldName, Reasons) when is_binary(FieldName) ->
  <<"Invalid field '", FieldName/binary, "'. Reason: invalid ",
    (enum_reasons(Reasons))/binary>>;

error_msg(FieldName, Reasons) ->
  FieldName2 = list_to_binary(io_lib:format("~p", [FieldName])),
  <<"Invalid field '", FieldName2/binary, "'. Reason: invalid ",
    (enum_reasons(Reasons))/binary>>.

%% -------------------------------------------------------------------
%% @private
%% -------------------------------------------------------------------
-spec enum_reasons([binary()]) -> binary().

enum_reasons([Reason | Reasons]) ->
  enum_reasons(Reasons, <<Reason/binary>>).

%% -------------------------------------------------------------------
%% @private
%% -------------------------------------------------------------------
-spec enum_reasons([binary()], binary()) -> binary().

enum_reasons([Reason | Reasons], Buf) ->
  enum_reasons(Reasons, <<Buf/binary, ", ", Reason/binary>>);

enum_reasons([], Buf) ->
  <<Buf/binary, ".">>.


%%% ==================================================================
%%% Unit tests
%%% ==================================================================

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

is_valid_type_test_() ->
  fun() ->
    [
      ?assertMatch(true, is_valid_type(7, integer)),
      ?assertMatch(true, is_valid_type(7.2, float)),
      ?assertMatch(true, is_valid_type(7.2, number)),
      ?assertMatch(true, is_valid_type(7, number)),
      ?assertMatch(true, is_valid_type(<<>>, binary)),
      ?assertMatch(true, is_valid_type(<<"x">>, binary)),
      ?assertMatch(true, is_valid_type([], list)),
      ?assertMatch(true, is_valid_type([1,2,3], list)),
      ?assertMatch(true, is_valid_type({}, tuple)),
      ?assertMatch(true, is_valid_type({1,2,3}, tuple)),
      ?assertMatch(true, is_valid_type(#{}, map)),
      ?assertMatch(true, is_valid_type(#{a => b}, map)),
      ?assertMatch(true, is_valid_type(test, atom)),
      ?assertMatch(true, is_valid_type(fun() -> ok end, function)),
      ?assertMatch(true, is_valid_type(true, boolean)),
      ?assertMatch(true, is_valid_type(false, boolean))
    ]
  end.

is_valid_size_test_() ->
  fun() ->
    [
      ?assertMatch(true, is_valid_size(0, integer, {-1, 1})),
      ?assertMatch(true, is_valid_size(1, integer, {-1, 1})),
      ?assertMatch(true, is_valid_size(1, integer, {infinity, 1})),
      ?assertMatch(true, is_valid_size(1, integer, {1, infinity})),
      ?assertMatch(true, is_valid_size(1, integer, {infinity, infinity})),

      ?assertMatch(true, is_valid_size(0.1, float, {-1, 1})),
      ?assertMatch(true, is_valid_size(1.0, float, {-1, 1})),
      ?assertMatch(true, is_valid_size(0.9, float, {infinity, 1})),
      ?assertMatch(true, is_valid_size(1.1, float, {1, infinity})),
      ?assertMatch(true, is_valid_size(1.0, float, {infinity, infinity})),

      ?assertMatch(true, is_valid_size(<<>>, binary, {infinity, infinity})),
      ?assertMatch(true, is_valid_size(<<"">>, binary, {infinity, 32})),
      ?assertMatch(true, is_valid_size(crypto:strong_rand_bytes(32), binary, {0, 32})),
      ?assertMatch(true, is_valid_size(crypto:strong_rand_bytes(32), binary, {32, 32})),
      ?assertMatch(false, is_valid_size(crypto:strong_rand_bytes(33), binary, {0, 32})),
      ?assertMatch(false, is_valid_size(crypto:strong_rand_bytes(30), binary, {32, 32})),

      ?assertMatch(true, is_valid_size({}, tuple, {infinity, infinity})),
      ?assertMatch(true, is_valid_size({1}, tuple, {infinity, 32})),
      ?assertMatch(true, is_valid_size({3}, tuple, {0, 3})),
      ?assertMatch(true, is_valid_size({1,2,3}, tuple, {3, 3})),
      ?assertMatch(false, is_valid_size({}, tuple, {1, 1})),
      ?assertMatch(false, is_valid_size({1,2,3}, tuple, {1, 2})),

      ?assertMatch(true, is_valid_size([], list, {infinity, infinity})),
      ?assertMatch(true, is_valid_size([], list, {0, 0})),
      ?assertMatch(true, is_valid_size([1,2,3], list, {infinity, 3})),
      ?assertMatch(true, is_valid_size([], list, {0, 3})),
      ?assertMatch(true, is_valid_size([1,2], list, {0, 3})),
      ?assertMatch(true, is_valid_size([1,2,3], list, {3, 3})),

      ?assertMatch(true, is_valid_size(fun() -> ok end, function, {0, 0})),
      ?assertMatch(true, is_valid_size(fun os:timestamp/0, function, {0, 0})),
      ?assertMatch(true, is_valid_size(fun os:timestamp/0, function, {0, 0})),
      ?assertMatch(true, is_valid_size(fun os:timestamp/0, function, {infinity, infinity})),
      ?assertMatch(true, is_valid_size(fun os:timestamp/0, function, {infinity, 0})),
      ?assertMatch(true, is_valid_size(fun os:timestamp/0, function, {0, infinity})),
      ?assertMatch(true, is_valid_size(fun(_) -> ok end, function, {0, infinity})),
      ?assertMatch(true, is_valid_size({os, timestamp, 0}, function, {0, 0})),
      ?assertMatch(false, is_valid_size({os, timestamp, 0}, function, {1, 1})),
      ?assertMatch(false, is_valid_size(fun(_) -> ok end, function, {0, 0})),
      ?assertMatch(false, is_valid_size(fun os:timestamp/0, function, {1, 1}))
    ]
  end.

is_valid_value_test_() ->
  fun() ->
    []
  end.

validate_test_() ->
  % ------------------------------------------------------------------
  % Case #1: valid data (all fields is exists and valid)
  % ------------------------------------------------------------------
  Data1L = [
    {<<"email">>, <<"john@example.org">>},
    {<<"name">>, <<"John">>},
    {<<"password">>, <<"secret">>}
  ],
  Data1M = #{
    <<"email">> => <<"john@example.org">>,
    <<"name">> => <<"John">>,
    <<"password">> => <<"secret">>
  },
  Rules1 = [
    #vrule{field = <<"email">>, type = binary, size = {5, 1024}},
    #vrule{field = <<"name">>, type = binary, size = {1, 1024}},
    #vrule{field = <<"password">>, type = binary, size = {1, 1024}}
  ],
  % ------------------------------------------------------------------
  % Case #2: valid data (all required fields is exists and valid)
  % ------------------------------------------------------------------
  Data2L = [
    {<<"email">>, <<"john@example.org">>},
    {<<"name">>, <<"John">>},
    {<<"password">>, <<"secret">>}
  ],
  Rules2 = [
    #vrule{field = <<"email">>, type = binary, size = {5, 1024}},
    #vrule{field = <<"name">>, type = binary, size = {1, 1024}},
    #vrule{field = <<"password">>, type = binary, size = {1, 1024}},
    #vrule{field = <<"subscribe">>, type = boolean, optional = true}
  ],
  % ------------------------------------------------------------------
  % Case #3: valid data (all required fields + optional field is exists and valid)
  % ------------------------------------------------------------------
  Data3L = [
    {<<"email">>, <<"john@example.org">>},
    {<<"name">>, <<"John">>},
    {<<"password">>, <<"secret">>},
    {<<"subscribe">>, true}
  ],
  Rules3 = [
    #vrule{field = <<"email">>, type = binary, size = {5, 1024}},
    #vrule{field = <<"name">>, type = binary, size = {1, 1024}},
    #vrule{field = <<"password">>, type = binary, size = {1, 1024}},
    #vrule{field = <<"subscribe">>, type = boolean, optional = true}
  ],
  % ------------------------------------------------------------------
  % Case #4: invalid data (all fields is exists but email have invalid type)
  % ------------------------------------------------------------------
  Data4L = [
    {<<"email">>, 123},
    {<<"name">>, <<"John">>},
    {<<"password">>, <<"secret">>},
    {<<"subscribe">>, true}
  ],
  Rules4 = [
    #vrule{field = <<"email">>, type = binary, size = {5, 1024}},
    #vrule{field = <<"name">>, type = binary, size = {1, 1024}},
    #vrule{field = <<"password">>, type = binary, size = {1, 1024}},
    #vrule{field = <<"subscribe">>, type = boolean, optional = true}
  ],
  % ------------------------------------------------------------------
  % Case #5: invalid data (email field doesn't exists)
  % ------------------------------------------------------------------
  Data5L = [
    {<<"name">>, <<"John">>},
    {<<"password">>, <<"secret">>},
    {<<"subscribe">>, true}
  ],
  Rules5 = [
    #vrule{field = <<"email">>, type = binary, size = {5, 1024}},
    #vrule{field = <<"name">>, type = binary, size = {1, 1024}},
    #vrule{field = <<"password">>, type = binary, size = {1, 1024}},
    #vrule{field = <<"subscribe">>, type = boolean, optional = true}
  ],
  % ------------------------------------------------------------------
  % Case #6: invalid data (all required fields + optional field is exists but have invalid type)
  % ------------------------------------------------------------------
  Data6L = [
    {<<"email">>, <<"john@example.org">>},
    {<<"name">>, <<"John">>},
    {<<"password">>, <<"secret">>},
    {<<"subscribe">>, 777}
  ],
  Rules6 = [
    #vrule{field = <<"email">>, type = binary, size = {5, 1024}},
    #vrule{field = <<"name">>, type = binary, size = {1, 1024}},
    #vrule{field = <<"password">>, type = binary, size = {1, 1024}},
    #vrule{field = <<"subscribe">>, type = boolean, optional = true}
  ],
  % ------------------------------------------------------------------
  % Case #7: valid data (all required fields + optional field is exists)
  % ------------------------------------------------------------------
  Data7L = [
    {<<"email">>, <<"john@example.org">>},
    {<<"name">>, <<"John">>},
    {<<"password">>, <<"secret">>},
    {<<"subscribe">>, false}
  ],
  Rules7 = [
    #vrule{field = <<"email">>, type = binary, size = {5, 1024}, regex = ?RE_EMAIL},
    #vrule{field = <<"name">>, type = binary, size = {1, 1024}, regex = ?RE_NAME},
    #vrule{field = <<"password">>, type = binary, size = {1, 1024}},
    #vrule{field = <<"subscribe">>, type = boolean, optional = true}
  ],
  % ------------------------------------------------------------------
  % Case #8: invalid data (all required fields + optional field is exists, but name are invalid)
  % ------------------------------------------------------------------
  Data8L = [
    {<<"email">>, <<"john@example.org">>},
    {<<"name">>, <<"<script>alert(document.cookie);</script>">>},
    {<<"password">>, <<"secret">>},
    {<<"subscribe">>, false}
  ],
  Rules8 = [
    #vrule{field = <<"email">>, type = binary, size = {5, 1024}, regex = ?RE_EMAIL},
    #vrule{field = <<"name">>, type = binary, size = {1, 1024}, regex = ?RE_NAME},
    #vrule{field = <<"password">>, type = binary, size = {1, 1024}},
    #vrule{field = <<"subscribe">>, type = boolean, optional = true}
  ],
  % ------------------------------------------------------------------
  % Case #9: valid data
  % ------------------------------------------------------------------
  Data9L = [
    {<<"method">>, <<"GET">>}
  ],
  Rules9 = [
    #vrule{field = <<"method">>, allowed_values = [<<"GET">>, <<"POST">>, <<"PUT">>, <<"DELETE">>]}
  ],
  % ------------------------------------------------------------------
  % Case #10: valid data
  % ------------------------------------------------------------------
  Data10L = [
    {<<"xxx">>, 3}
  ],
  Rules10 = [
    #vrule{field = <<"xxx">>, allowed_values = [null, 1, 2, 3, <<"hello">>]}
  ],
  % ------------------------------------------------------------------
  % Case #11: invalid data
  % ------------------------------------------------------------------
  Data11L = [
    {<<"method">>, <<"PATCH">>}
  ],
  Rules11 = [
    #vrule{field = <<"method">>, allowed_values = [<<"GET">>, <<"POST">>, <<"PUT">>, <<"DELETE">>]}
  ],
  % ------------------------------------------------------------------
  % Case #12: valid data
  % ------------------------------------------------------------------
  Data12L = [
    {<<"email">>, <<"john@example.org">>}
  ],
  Rules12 = [
    #vrule{field = <<"email">>, size = {5, 1024}, regex = ?RE_EMAIL}
  ],
  % ------------------------------------------------------------------
  % Case #13: valid data
  % ------------------------------------------------------------------
  Data13L = [
    {<<"email">>, <<"john@example.org">>}
  ],
  Rules13 = [
    #vrule{field = <<"email">>, regex = ?RE_EMAIL}
  ],
  % ------------------------------------------------------------------
  % Case #14: invalid data (email is valid but too long)
  % ------------------------------------------------------------------
  Data14L = [
    {<<"email">>, <<"john@example.org">>}
  ],
  Rules14 = [
    #vrule{field = <<"email">>, size = {5, 5}, regex = ?RE_EMAIL}
  ],
  % ------------------------------------------------------------------
  % Case #15: valid data
  % ------------------------------------------------------------------
  Data15L = [
    {<<"callback">>, fun validator:is_valid_type/2}
  ],
  Rules15 = [
    #vrule{field = <<"callback">>, type = function, size = {2, 2}}
  ],
  % ------------------------------------------------------------------
  % Case #16: valid data
  % ------------------------------------------------------------------
  Data16L = [
    {<<"callback">>, fun () -> os:timestamp() end}
  ],
  Rules16 = [
    #vrule{field = <<"callback">>, type = function, size = {0, 0}}
  ],
  % ------------------------------------------------------------------
  % Case #17: invalid data (bad arity)
  % ------------------------------------------------------------------
  Data17L = [
    {<<"callback">>, fun (N) -> N + 1 end}
  ],
  Rules17 = [
    #vrule{field = <<"callback">>, type = function, size = {0, 0}}
  ],
  % ------------------------------------------------------------------
  % Case #18: valid data
  % ------------------------------------------------------------------
  Data18L = [
    {<<"pid">>, self()}
  ],
  Rules18 = [
    #vrule{field = <<"pid">>, type = pid}
  ],
  % ------------------------------------------------------------------
  % Case #19: valid data
  % ------------------------------------------------------------------
  Data19L = [
    {<<"ref">>, make_ref()}
  ],
  Rules19 = [
    #vrule{field = <<"ref">>, type = ref}
  ],
  % ------------------------------------------------------------------
  % Case #20: valid data
  % ------------------------------------------------------------------
  Data20L = [
    {<<"timestamp">>, {os, timestamp, 0}}
  ],
  Rules20 = [
    #vrule{field = <<"timestamp">>, type = function}
  ],
  % ------------------------------------------------------------------
  % Case #21: valid data
  % ------------------------------------------------------------------
  Data21L = [
    {<<"timestamp">>, {os, timestamp, 0}}
  ],
  Rules21 = [
    #vrule{field = <<"timestamp">>, type = function, size = {0, 0}}
  ],
  % ------------------------------------------------------------------
  % Case #22: valid data
  % ------------------------------------------------------------------
  Data22L = [
    {<<"timestamp">>, {os, timestamp, 0}}
  ],
  Rules22 = [
    #vrule{field = <<"timestamp">>}
  ],
  % ------------------------------------------------------------------
  % Case #23: valid data
  % ------------------------------------------------------------------
  Data23L = [
    {<<"timestamp">>, fun os:timestamp/0}
  ],
  Rules23 = [
    #vrule{field = <<"timestamp">>}
  ],
  fun() ->
    [
      ?assertMatch(ok, validate(Data1L, Rules1)),
      ?assertMatch(ok, validate(Data1M, Rules1)),

      ?assertMatch(ok, validate(Data2L, Rules2)),
      ?assertMatch(ok, validate(Data3L, Rules3)),

      ?assertMatch({error, _}, validate(Data4L, Rules4)),
      ?assertMatch({error, _}, validate(Data5L, Rules5)),
      ?assertMatch({error, _}, validate(Data6L, Rules6)),

      ?assertMatch(ok, validate(Data7L, Rules7)),

      ?assertMatch({error, _}, validate(Data8L, Rules8)),

      ?assertMatch(ok, validate(Data9L, Rules9)),
      ?assertMatch(ok, validate(Data10L, Rules10)),

      ?assertMatch({error, _}, validate(Data11L, Rules11)),

      ?assertMatch(ok, validate(Data12L, Rules12)),
      ?assertMatch(ok, validate(Data13L, Rules13)),

      ?assertMatch({error, _}, validate(Data14L, Rules14)),

      ?assertMatch(ok, validate(Data15L, Rules15)),
      ?assertMatch(ok, validate(Data16L, Rules16)),

      ?assertMatch({error, _}, validate(Data17L, Rules17)),

      ?assertMatch(ok, validate(Data18L, Rules18)),
      ?assertMatch(ok, validate(Data19L, Rules19)),
      ?assertMatch(ok, validate(Data20L, Rules20)),
      ?assertMatch(ok, validate(Data21L, Rules21)),
      ?assertMatch(ok, validate(Data22L, Rules22)),
      ?assertMatch(ok, validate(Data23L, Rules23))
    ]
  end.

validate_bulk_test_() ->
  Rules = [
    #vrule{field = <<"email">>, type = binary, size = {5, 1024}, regex = ?RE_EMAIL},
    #vrule{field = <<"name">>, type = binary, size = {1, 1024}, regex = ?RE_NAME},
    #vrule{field = <<"password">>, type = binary, size = {1, 1024}},
    #vrule{field = <<"subscribe">>, type = boolean, optional = true}
  ],
  % ------------------------------------------------------------------
  % Case #1: valid data (list of lists)
  % ------------------------------------------------------------------
  Data1 = [
    [
      {<<"email">>, <<"john@example.org">>},
      {<<"name">>, <<"John">>},
      {<<"password">>, <<"secret">>},
      {<<"subscribe">>, false}
    ],
    [
      {<<"email">>, <<"alice@example.org">>},
      {<<"name">>, <<"Alice">>},
      {<<"password">>, <<"secret">>},
      {<<"subscribe">>, true}
    ],
    [
      {<<"email">>, <<"bob@example.org">>},
      {<<"name">>, <<"Bob">>},
      {<<"password">>, <<"secret">>}
    ]
  ],
  % ------------------------------------------------------------------
  % Case #2: valid data (list of maps)
  % ------------------------------------------------------------------
  Data2 = [
    #{
      <<"email">> => <<"john@example.org">>,
      <<"name">> => <<"John">>,
      <<"password">> => <<"secret">>,
      <<"subscribe">> => false
    },
    #{
      <<"email">> => <<"alice@example.org">>,
      <<"name">> => <<"Alice">>,
      <<"password">> => <<"secret">>,
      <<"subscribe">> => true
    },
    #{
      <<"email">> => <<"bob@example.org">>,
      <<"name">> => <<"Bob">>,
      <<"password">> => <<"secret">>
    }
  ],
  % ------------------------------------------------------------------
  % Case #3: valid + invalid data (list of maps)
  % ------------------------------------------------------------------
  Data3 = [
    #{
      <<"email">> => <<"john@example.org">>,
      <<"name">> => <<"John">>,
      <<"password">> => <<"secret">>,
      <<"subscribe">> => false
    },
    #{
      <<"email">> => <<"bad-email">>, % <--- NOTE: invalid email
      <<"name">> => <<"Alice">>,
      <<"password">> => <<"secret">>,
      <<"subscribe">> => true
    },
    #{
      <<"email">> => <<"bob@example.org">>,
      <<"name">> => <<"Bob">>,
      <<"password">> => <<"secret">>
    }
  ],
  fun () ->
    [
      ?assertMatch(ok, validate_bulk(Data1, Rules)),
      ?assertMatch(ok, validate_bulk(Data2, Rules)),
      ?assertMatch({error, _}, validate_bulk(Data3, Rules)),
      ?assertMatch({report, [_, _], [_]}, validate_bulk(Data3, Rules, [{on_error, continue}]))
    ]
  end.

utils_test_() ->
  fun() ->
    [
      ?assertMatch(hello, get_val(k, #{k => hello})),
      ?assertMatch(hello, get_val(k, [{k, hello}])),

      ?assertMatch(<<"Invalid field 'email'. Reason: invalid type, value.">>,
        error_msg(<<"email">>, [<<"type">>, <<"value">>])),

      ?assertMatch(<<"Invalid field 'max_conn'. Reason: invalid type, value.">>,
        error_msg(max_conn, [<<"type">>, <<"value">>])),

      ?assertMatch(<<"Invalid field '123'. Reason: invalid type, value.">>,
        error_msg(123, [<<"type">>, <<"value">>])),

      ?assertMatch(<<"Invalid field '1.5'. Reason: invalid type, value.">>,
        error_msg(1.5, [<<"type">>, <<"value">>])),

      ?assertMatch(<<"Invalid field '{user,max_login_attempts}'. Reason: invalid type, value.">>,
        error_msg({user, max_login_attempts}, [<<"type">>, <<"value">>])),

      ?assertMatch(<<"Invalid field '[user,max_login_attempts]'. Reason: invalid type, value.">>,
        error_msg([user, max_login_attempts], [<<"type">>, <<"value">>])),

      ?assertMatch(<<"Invalid field '{}'. Reason: invalid type, value.">>,
        error_msg({}, [<<"type">>, <<"value">>])),

      ?assertMatch(<<"Invalid field '[]'. Reason: invalid type, value.">>,
        error_msg([], [<<"type">>, <<"value">>])),

      ?assertMatch(<<"Invalid field '#{}'. Reason: invalid type, value.">>,
        error_msg(#{}, [<<"type">>, <<"value">>]))
    ]
  end.

-endif.