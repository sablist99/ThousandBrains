%%%-------------------------------------------------------------------
%%% @author Potap
%%% @copyright (C) 2024, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 18. май 2024 19:00
%%%-------------------------------------------------------------------
-module('HelpFunctions').
-author("Potap").

%% API
-export([mapWriteToFile/2, listWriteToFile/2, getIntegerFromString/1, printGlobalData/0]).

-include("Model/ProjectSettings.hrl").
-include("Model/Model.hrl").

% Функция вывода Map в файл с заданным именем
% TODO Рассмотреть идею - сначала формировать большую строку, а затем разом писать ее в файл
mapWriteToFile(File, Map) ->
  case Map of
    undefined ->
      error;
    _ ->
      {ok, S} = file:open(?FileDirectory ++ File, write),
      maps:foreach(
        fun(Key, Value) ->
          if
            Value#synapse.permanenceWeight == false -> none;
            true -> file:write_file(?FileDirectory ++ File, io_lib:fwrite("~p -> ~p \n", [Key, Value]), [append])
          end
        end, Map),
      file:close(S)
  end.

% Функция вывода List в файл
listWriteToFile(File, List) ->
  case List of
    undefined ->
      error;
    _ ->
      {ok, S} = file:open(?FileDirectory ++ File, write),
      lists:foreach(
        fun(Value) ->
          file:write_file(?FileDirectory ++ File, io_lib:fwrite("~p\n", [Value]), [append])
        end, List),
      file:close(S)
  end.

% Функция выводит в файлы все глобальные данные
printGlobalData() ->
  'HelpFunctions':mapWriteToFile('InLayer.tb', 'GlobalDataService':getInLayer()),
  'HelpFunctions':mapWriteToFile('OutLayer.tb', 'GlobalDataService':getOutLayer()),
  'HelpFunctions':mapWriteToFile('FeedForward.tb', 'GlobalDataService':getFeedForward()),
  'HelpFunctions':mapWriteToFile('FeedBack.tb', 'GlobalDataService':getFeedBack()),
  'HelpFunctions':mapWriteToFile('InActiveLayer.tb', 'GlobalDataService':getInActiveCells()),
  'HelpFunctions':mapWriteToFile('InPredictedCells.tb', 'GlobalDataService':getInPredictedCells()),
  'HelpFunctions':listWriteToFile('OutActiveLayer.tb', 'GlobalDataService':getOutActiveCells()).

% Функция получает на вход строку.
% Если получилось преобразовать в число, то вернет это число.
% Иначе вернет false
getIntegerFromString(X) ->
  case string:to_integer(X) of
    {A, B} ->
      if
      % Если получилось распарсить и такм не оказалось посторонних символов
        length(B) == 0 -> A;
      % Если есть мусор, то это не число
        true -> false
      end;
    % Не получилось распарсить
    _ -> false
  end.
