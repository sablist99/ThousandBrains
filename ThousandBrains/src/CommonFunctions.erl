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
-export([existSynapseInMap/2, existSynapseInFeedForwardMap/3, existActiveApicalDendrite/1, hasMiniColumnInPredict/1, hasActiveApicalDendriteInPredict/1]).

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



% Проверка на наличие апикального дендрита у клетки по Guid
existActiveApicalDendrite(CellGuid) ->
  lists:member(CellGuid, get(activeApicalDendrites)).



% Проверка на наличие мини-колонки в предсказанных
hasMiniColumnInPredict(RangeOfColumnWithFeedForward) ->
% TODO Сделать прослойку для доступа к глобальным данным
  case maps:find(RangeOfColumnWithFeedForward, get(?InPredictedCells)) of
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
