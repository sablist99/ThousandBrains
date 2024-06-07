%%%-------------------------------------------------------------------
%%% @author Potap
%%% @copyright (C) 2024, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 18. май 2024 15:29
%%%-------------------------------------------------------------------
-module('MyMath').
-author("Potap").

-include("Model/BrainSettings.hrl").
-include("Model/Model.hrl").

%% API
-export([getPoissonValue/0, getId/0, getGuid/0]).

factorial(0) -> 1;
factorial(N) when N > 0 -> N * factorial(N - 1).

poisson(Lambda, K) ->
  (math:pow(Lambda, K) * math:exp(-Lambda)) / factorial(K).

% Функция получения числа через распределение Пуассона
% Коэффициенты подобраны опытным путем
getPoissonValue() -> round(poisson(rand:uniform() * 7, 1) * ?ROUND_EPSILON) / ?ROUND_EPSILON.



% GUID
getGuid() ->
  <<A:32, B:16, C:16, D:16, E:48>> = rand:bytes(16),
  list_to_binary(io_lib:format("~8.16.0b-~4.16.0b-4~3.16.0b-~4.16.0b-~12.16.0b",
    [A, B, C band 16#0fff, D band 16#3fff bor 16#8000, E])).



getId() ->
  put(?ID, get(?ID) + 1),
  get(?ID).