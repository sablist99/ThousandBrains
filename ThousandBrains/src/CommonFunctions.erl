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
-export([existSynapseInMap/2, existSynapseBetweenLayers/3, existActiveApicalDendrite/1, hasMiniColumnInPredict/1, hasActiveApicalDendriteInPredict/1, getListSize/1]).

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


% Проверка существования синапса между клетками слоев
findSynapseByKeyInMap(Iterator, FromCellGuid, ToCellGuid) ->
  case maps:next(Iterator) of
    none -> none;
    {{{_, FromCellGuid}, {_, ToCellGuid}}, Value, _NewIterator} ->
      {ok, Value};
    {_, _, NewIterator} -> findSynapseByKeyInMap(NewIterator, FromCellGuid, ToCellGuid)
  end.

% TODO Перейти на true/false
existSynapseBetweenLayers(FromCellGuid, ToCellGuid, SynapsesMap) ->
  case findSynapseByKeyInMap(maps:iterator(SynapsesMap), FromCellGuid, ToCellGuid) of
    {ok, Value} ->
      case Value#synapse.permanenceWeight of
        true -> {1, Value};
        false -> 0
      end;
    _ -> 0
  end.


% Проверка на наличие апикального дендрита у клетки по Guid
existActiveApicalDendrite(_FromCellGuid, []) ->
  false;
existActiveApicalDendrite(_FromCellGuid, undefined) ->
  false;
existActiveApicalDendrite(FromCellGuid, [ActiveOutCell | H]) ->
  case existSynapseBetweenLayers(FromCellGuid, ActiveOutCell, 'GlobalDataService':getFeedBack()) of
    {1, _Value} -> {true, ActiveOutCell};
    0 -> existActiveApicalDendrite(FromCellGuid, H)
  end.
existActiveApicalDendrite(FromCellGuid) ->
  existActiveApicalDendrite(FromCellGuid, 'GlobalDataService':getOutActiveCells()).



% Проверка на наличие мини-колонки в предсказанных
hasMiniColumnInPredict(RangeOfColumnWithFeedForward) ->
  case maps:find(RangeOfColumnWithFeedForward, 'GlobalDataService':getInPredictedCells()) of
    {ok, _Value} -> true;
    _ -> false
  end.

hasActiveApicalDendriteInPredictByMiniColumn(Iterator) ->
  case maps:next(Iterator) of
    none -> false;
    {_CellGuid, {?NoActiveApicalDendrite, _}, NewIterator} -> hasActiveApicalDendriteInPredictByMiniColumn(NewIterator);
    {_CellGuid, {_ApicalDendrite, _}, _NewIterator} -> true
end.

% Определение наличия апикального дендрита хотя бы у одной клетки в мини-колонке
hasActiveApicalDendriteInPredict(LayerIterator, Ret) ->
  case maps:next(LayerIterator) of
    none -> Ret;
    {_Range, MiniColumn, NewIterator} -> hasActiveApicalDendriteInPredict(NewIterator, hasActiveApicalDendriteInPredictByMiniColumn(maps:iterator(MiniColumn)))
  end.

hasActiveApicalDendriteInPredict(LayerIterator) ->
  hasActiveApicalDendriteInPredict(LayerIterator, false).


% Количество элементов в списке
getListSize([]) -> 0;
getListSize(List) ->
  getListSize(List, 0).

getListSize([], Count) ->
  Count;
getListSize([_|L], Count) ->
  getListSize(L, Count + 1).
