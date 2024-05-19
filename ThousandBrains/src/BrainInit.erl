%%%-------------------------------------------------------------------
%%% @author Potap
%%% @copyright (C) 2024, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 16. май 2024 0:59
%%%-------------------------------------------------------------------
-module('BrainInit').
-author("Potap").

-include("Model.hrl").

%% API
-export([initializeGlobalData/0]).


% Определение наличия связи в зависимости от поргового значения
getPermanenceWeight(Value) when Value >= ?PERMANENCE_WEIGHT_BORDER ->
  true;
getPermanenceWeight(_Value) ->
  false.

% Функция создания синапса
getSynapseHelper(Value) ->
  #synapse{
    guid = 'MyMath':getGuid(),
    permanenceValue = Value,
    permanenceWeight = getPermanenceWeight(Value)}.

getSynapse() ->
  getSynapseHelper('MyMath':getPoissonValue()).



% Функция создания дендрита клетки любого слоя
% CurrentSynapse - счетчик, количество синапсов на текущем дендрите.
% Dendrite - out переменная, в которой копится результат (Map)
% SynapseCount - количество синапсов на дендрите (меняется в зависимости от типа слоя)
getDendriteHelper(CurrentSynapse, Dendrite, SynapseCount) when CurrentSynapse == SynapseCount ->
  Dendrite;
getDendriteHelper(CurrentSynapse, Dendrite, SynapseCount) ->
  getDendriteHelper(CurrentSynapse + 1, maps:put(CurrentSynapse, getSynapse(), Dendrite), SynapseCount).

getDendrite(SynapseCount) -> getDendriteHelper(0, #{}, SynapseCount).



% Функция создания клетки для любого слоя
% CurrentDendrite - счетчик, количество дендритов на текущей клетке.
% Cell - out переменная, в которой копится результат (Map)
% DendriteCount
getCellHelper(CurrentDendrite, Cell, _SynapseCount) when CurrentDendrite == ?D ->
  Cell;
getCellHelper(CurrentDendrite, Cell, SynapseCount) ->
  getCellHelper(CurrentDendrite + 1, maps:put('MyMath':getGuid(), getDendrite(SynapseCount), Cell), SynapseCount).

getCell(SynapseCount) -> getCellHelper(0, #{}, SynapseCount).



% Функция создания мини-колонки входного слоя
getInMiniColumnHelper(CurrentCell, MiniColumn) when CurrentCell == ?M ->
  MiniColumn;
getInMiniColumnHelper(CurrentCell, MiniColumn) ->
  getInMiniColumnHelper(CurrentCell + 1, maps:put('MyMath':getGuid(), getCell(?N_EXT), MiniColumn)).

getInMiniColumn() -> getInMiniColumnHelper(0, #{}).



% Функция создания входного слоя одной колонки
getInLayerHelper(CurrentMiniColumn, Layer) when CurrentMiniColumn == ?N_IN ->
  Layer;
getInLayerHelper(CurrentMiniColumn, Layer) ->
  getInLayerHelper(CurrentMiniColumn + 1, maps:put(CurrentMiniColumn, getInMiniColumn(), Layer)).

getInLayer() -> getInLayerHelper(0, #{}).



% Функция создания выходного слоя одной колонки
getOutLayerHelper(CurrentCell, Layer) when CurrentCell == ?N_OUT ->
  Layer;
getOutLayerHelper(CurrentCell, Layer) ->
  getOutLayerHelper(CurrentCell + 1, maps:put(CurrentCell, {'MyMath':getGuid(), getCell(?N_OUT)}, Layer)).

getOutLayer() -> getOutLayerHelper(0, #{}).



% Обход клеток выходного слоя
getSynapsesBetweenLayersHelper([], _CurrentInCell, ResultMap) ->
  ResultMap;
getSynapsesBetweenLayersHelper([CurrentOutCell | TOut], CurrentInCell, ResultMap) ->
  getSynapsesBetweenLayersHelper(TOut, CurrentInCell, maps:put({CurrentInCell, CurrentOutCell}, getSynapse() , ResultMap)).

% Обход клеток входного слоя
getSynapsesBetweenLayers([], _To, ResultMap) ->
  ResultMap;
getSynapsesBetweenLayers([CurrentFrom | FromT], To,  ResultMap) ->
  getSynapsesBetweenLayers(FromT, To, getSynapsesBetweenLayersHelper(To, CurrentFrom, ResultMap)).

% Функция возвращает мапу с синапсами между входным и вЫходным слоем по ключу {CurrentInCell, CurrentOutCell}
getFeedForward() -> getSynapsesBetweenLayers(get(?AllInCells), get(?AllOutCells), #{}).

% Функция возвращает мапу с синапсами между вЫходным и входным слоем по ключу {CurrentOutCell, CurrentInCell}
getFeedBack() -> getSynapsesBetweenLayers(get(?AllOutCells), get(?AllInCells), #{}).



% Перебор клеток в мини-колонке
getAllInCellsByCells(CellIterator, AllInCells) ->
  case maps:next(CellIterator) of
    none -> AllInCells;
    {CellGuid, _DendriteMap, NewCellIterator} ->
      getAllInCellsByCells(NewCellIterator, lists:append(AllInCells, [CellGuid]))
  end.

% Перебор мини-колонок во входном слое
getAllInCellsByMiniColumns(MiniColumnIterator, AllInCells) ->
  case maps:next(MiniColumnIterator) of
    none -> AllInCells;
    {_ColumnRange, CellMap, NewMiniColumnIterator} ->
      getAllInCellsByMiniColumns(NewMiniColumnIterator,
        getAllInCellsByCells(maps:iterator(CellMap), AllInCells))
  end.

% Функция возвращает Guid всех клеток входного слоя
getAllInCells() -> getAllInCellsByMiniColumns(maps:iterator(get(?InLayer)), []).



% Перебор клеток в выходном слое.
% Да, дубль функции, но причины для изменения разные
getAllOutCellsHelper(CellIterator, AllOutCells) ->
  case maps:next(CellIterator) of
    none -> AllOutCells;
    {_Range, {CellGuid, _Cell}, NewCellIterator} ->
      getAllOutCellsHelper(NewCellIterator, lists:append(AllOutCells, [CellGuid]))
  end.

% Функция возвращает Guid всех клеток выходного слоя
getAllOutCells() -> getAllOutCellsHelper(maps:iterator(get(?OutLayer)), []).



initializeGlobalData() ->
  put(?InLayer, getInLayer()),
  put(?OutLayer, getOutLayer()),
  put(?AllInCells, getAllInCells()),
  put(?AllOutCells, getAllOutCells()),
  put(?FeedForward, getFeedForward()),
  put(?FeedBack, getFeedBack()),
  put(?ActiveApicalDendrites, []),
  ok.
