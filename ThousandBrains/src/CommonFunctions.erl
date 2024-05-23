%%%-------------------------------------------------------------------
%%% @author Potap
%%% @copyright (C) 2024, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 19. май 2024 16:06
%%%-------------------------------------------------------------------
-module('CommonFunctions').
-author("Potap").

%% API
-export([existSynapseInMap/2, existSynapseInFeedForwardMap/3, existActiveApicalDendrite/1, hasMiniColumnInPredict/1, hasActiveApicalDendriteInPredict/1, getListSize/1]).

-include("Model.hrl").

% Проверка существования синапса
existSynapseInMap(Range, SynapsesMap) ->
  case maps:find(Range, SynapsesMap) of
    {ok, Value} ->
      case Value#synapse.permanenceWeight of
        true -> {1, Value};
        false -> 0
      end;
    _ -> 0
  end.


% Проверка существования синапса между клеткой входного и клеткой выходного слоя
findSynapseByKeyInFeedForwardMap(Iterator, ActiveCellGuid, OutCellGuid) ->
  case maps:next(Iterator) of
    none -> none;
    {{{_, ActiveCellGuid}, {_, OutCellGuid}}, Value, _NewIterator} ->
      {ok, Value};
    {_, _, NewIterator} -> findSynapseByKeyInFeedForwardMap(NewIterator, ActiveCellGuid, OutCellGuid)
  end.

existSynapseInFeedForwardMap(ActiveCellGuid, OutCellGuid, SynapsesMap) ->
  case findSynapseByKeyInFeedForwardMap(maps:iterator(SynapsesMap), ActiveCellGuid, OutCellGuid) of
    {ok, Value} ->
      case Value#synapse.permanenceWeight of
        true -> {1, Value};
        false -> 0
      end;
    _ -> 0
  end.


% TODO переделать на обращение к FeedBack + изменить сохранение идентификатора апикального дендрита на составнок ключ
% Проверка на наличие апикального дендрита у клетки по Guid
existActiveApicalDendrite(CellGuid) ->
%  lists:member(CellGuid, get(activeApicalDendrites)).
  lists:member(CellGuid, []). % [] - заглушка



% Проверка на наличие мини-колонки в предсказанных
hasMiniColumnInPredict(RangeOfColumnWithFeedForward) ->
% TODO Сделать прослойку для доступа к глобальным данным
  case maps:find(RangeOfColumnWithFeedForward, 'GlobalDataService':getInPredictedCells()) of
    {ok, _Value} -> true;
    _ -> false
  end.



% Определение наличия апикального дендрита хотя бы у одной клетки в мини-колонке
hasActiveApicalDendriteInPredict(Iterator) ->
  case maps:next(Iterator) of
    none -> false;
    {_CellGuid, {?HasActiveApicalDendrite, _}, _NewIterator} -> true;
    {_, _, NewIterator} -> hasActiveApicalDendriteInPredict(NewIterator)
  end.


% Количество элементов в списке
getListSize([]) -> 0;
getListSize(List) ->
  getListSize(List, 0).

getListSize([], Count) ->
  Count;
getListSize([_|L], Count) ->
  getListSize(L, Count + 1).
