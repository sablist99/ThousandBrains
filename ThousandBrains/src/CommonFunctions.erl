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
-export([existSynapseInMap/2, existActiveApicalDendriteByCellGuid/1,
  hasMiniColumnInPredict/1, hasActiveApicalDendriteInPredict/1, getListSize/1, getMiniColumnsWithActiveApicalDendrite/0,
  existActiveApicalDendriteByRanges/1, existSynapseBetweenLayersByRanges/3, findSynapseInMapByFRTG/3,
  findSynapseInMapByFRTR/3, existSynapseBetweenLayersByFRTG/3, existSynapseBetweenLayersByFGTR/3]).

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
% По рэнджу клетки входного слоя и guid клетки выходного слоя
findSynapseInMapByFRTG(FromCellRange, ToCellGuid, Iterator) ->
  case maps:next(Iterator) of
    none -> none;
    {{{FromCellRange, _}, {_, ToCellGuid}}, Value, _NewIterator} ->
      {ok, Value};
    {_, _, NewIterator} -> findSynapseInMapByFRTG(FromCellRange, ToCellGuid, NewIterator)
  end.

% Проверка существования синапса между клетками слоев
% По guid клетки входного слоя и рэнджу клетки выходного слоя
findSynapseInMapByFGTR(FromCellGuid, ToCellRange, Iterator) ->
  case maps:next(Iterator) of
    none -> none;
    {{{_, FromCellGuid}, {ToCellRange, _}}, Value, _NewIterator} ->
      {ok, Value};
    {_, _, NewIterator} -> findSynapseInMapByFGTR(FromCellGuid, ToCellRange, NewIterator)
  end.

% Проверка существования синапса между клетками слоев
% По рэнджу клетки входного слоя и рэнджу клетки выходного слоя
findSynapseInMapByFRTR(FromCellRange, ToCellRange, Iterator) ->
  case maps:next(Iterator) of
    none -> none;
    {{{FromCellRange, _}, {ToCellRange, _}}, Value, _NewIterator} ->
      {ok, Value};
    {_, _, NewIterator} -> findSynapseInMapByFRTR(FromCellRange, ToCellRange, NewIterator)
  end.


% TODO Подумать, как параметризировать три функции ниже
existSynapseBetweenLayersByFRTG(FromCellRange, ToCellGuid, SynapsesMap) ->
  case findSynapseInMapByFRTG(FromCellRange, ToCellGuid, maps:iterator(SynapsesMap)) of
    {ok, Value} ->
      case Value#synapse.permanenceWeight of
        true -> {true, Value};
        false -> false
      end;
    _ -> false
  end.

existSynapseBetweenLayersByFGTR(FromCellGuid, ToCellRange, SynapsesMap) ->
  case findSynapseInMapByFGTR(FromCellGuid, ToCellRange, maps:iterator(SynapsesMap)) of
    {ok, Value} ->
      case Value#synapse.permanenceWeight of
        true -> {true, Value};
        false -> false
      end;
    _ -> false
  end.

existSynapseBetweenLayersByRanges(FromRange, ToRange, SynapsesMap) ->
  case findSynapseInMapByFRTR(FromRange, ToRange, maps:iterator(SynapsesMap)) of
    {ok, Value} ->
      case Value#synapse.permanenceWeight of
        true -> {true, Value};
        false -> false
      end;
    _ -> false
  end.



% Проверка на наличие апикального дендрита у клетки входного слоя по Guid
existActiveApicalDendriteByCellGuid(_ToInCellGuid, []) ->
  false;
existActiveApicalDendriteByCellGuid(_ToInCellGuid, undefined) ->
  false;
existActiveApicalDendriteByCellGuid(ToInCellGuid, [ActiveOutCellRange | H]) ->
  case existSynapseBetweenLayersByFRTG(ActiveOutCellRange, ToInCellGuid, 'GlobalDataService':getFeedBack()) of
    {true, _Value} -> {true, ActiveOutCellRange};
    false -> existActiveApicalDendriteByCellGuid(ToInCellGuid, H)
  end.
existActiveApicalDendriteByCellGuid(ToInCellGuid) ->
  existActiveApicalDendriteByCellGuid(ToInCellGuid, 'GlobalDataService':getOutActiveCells()).



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








% Проверка на наличие апикального дендрита у клетки входного слоя по Guid
existActiveApicalDendriteByRanges(_ToRange, []) ->
  false;
existActiveApicalDendriteByRanges(_ToRange, undefined) ->
  false;
existActiveApicalDendriteByRanges(ToRange, [ActiveOutCellRange | H]) ->
  case existSynapseBetweenLayersByRanges(ActiveOutCellRange, ToRange, 'GlobalDataService':getFeedBack()) of
    {true, _Value} -> true;
    false -> existActiveApicalDendriteByRanges(ToRange, H)
  end.
existActiveApicalDendriteByRanges(ToRange) ->
  existActiveApicalDendriteByRanges(ToRange, 'GlobalDataService':getOutActiveCells()).



% Получение списка мини-колонок, у которых есть апикальный денднрит
getMiniColumnsWithActiveApicalDendrite(LayerIterator, TargetColumns) ->
  case maps:next(LayerIterator) of
    none -> TargetColumns;
    {Range, _MiniColumnMap, NewIterator} ->
      case existActiveApicalDendriteByRanges(Range) of
        true -> getMiniColumnsWithActiveApicalDendrite(NewIterator, lists:append(TargetColumns, [Range]));
        false -> getMiniColumnsWithActiveApicalDendrite(NewIterator, TargetColumns)
      end
end.

getMiniColumnsWithActiveApicalDendrite() -> getMiniColumnsWithActiveApicalDendrite(maps:iterator('GlobalDataService':getInLayer()), []).



% Количество элементов в списке
getListSize([]) -> 0;
getListSize(List) ->
  getListSize(List, 0).

getListSize([], Count) ->
  Count;
getListSize([_|L], Count) ->
  getListSize(L, Count + 1).
