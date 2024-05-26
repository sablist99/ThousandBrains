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
-export([initializeGlobalData/0, getSynapse/0]).


% Определение наличия связи в зависимости от поргового значения
getPermanenceWeight(Value) when Value >= ?PERMANENCE_WEIGHT_BORDER ->
  true;
getPermanenceWeight(_Value) ->
  false.

% Функция создания синапса
getSynapseHelper(Value) ->
  case Value < ?POTENTIAL_SYNAPSE_BORDER of
    true -> none;
    false ->
      #synapse{
        guid = 'MyMath':getId(),
        permanenceValue = Value,
        permanenceWeight = getPermanenceWeight(Value)}
  end.


getSynapse() ->
  getSynapseHelper('MyMath':getPoissonValue()).



% Функция добавления синапса в дендрит. Добавляет в дендрит только в том случае, если вернулся "синапс" из которого потенциально может вырасти настоящий синапс Определяется пороговым значением.
addSynapseInDendrite(_Key, Synapse, Dendrite) when Synapse == none ->
  Dendrite;
addSynapseInDendrite(Key, Synapse, Dendrite) ->
  maps:put(Key, Synapse, Dendrite).

% Функция создания дендрита клетки любого слоя
% CurrentSynapseNumber - счетчик, количество синапсов на текущем дендрите.
% Dendrite - out переменная, в которой копится результат (Map)
% SynapseCount - количество синапсов на дендрите (меняется в зависимости от типа слоя)
getDendriteHelper(CurrentSynapseNumber, Dendrite, SynapseCount) when CurrentSynapseNumber == SynapseCount ->
  Dendrite;
getDendriteHelper(CurrentSynapseNumber, Dendrite, SynapseCount) ->
  getDendriteHelper(CurrentSynapseNumber + 1, addSynapseInDendrite(CurrentSynapseNumber, getSynapse(), Dendrite), SynapseCount).

getDendrite(SynapseCount) -> getDendriteHelper(0, #{}, SynapseCount).



% Функция создания клетки для любого слоя
% CurrentDendrite - счетчик, количество дендритов на текущей клетке.
% Cell - out переменная, в которой копится результат (Map)
% DendriteCount
getCellHelper(CurrentDendrite, Cell, _SynapseCount) when CurrentDendrite == ?D ->
  Cell;
getCellHelper(CurrentDendrite, Cell, SynapseCount) ->
  getCellHelper(CurrentDendrite + 1, maps:put('MyMath':getId(), getDendrite(SynapseCount), Cell), SynapseCount).

getCell(SynapseCount) -> getCellHelper(0, #{}, SynapseCount).



% Функция создания мини-колонки входного слоя
getInMiniColumnHelper(CurrentCell, MiniColumn) when CurrentCell == ?M ->
  MiniColumn;
getInMiniColumnHelper(CurrentCell, MiniColumn) ->
  getInMiniColumnHelper(CurrentCell + 1, maps:put('MyMath':getId(), getCell(?N_EXT), MiniColumn)).

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
  getOutLayerHelper(CurrentCell + 1, maps:put(CurrentCell, {'MyMath':getId(), getCell(?N_OUT)}, Layer)).

getOutLayer() -> getOutLayerHelper(0, #{}).



% Обход клеток выходного слоя
getSynapsesBetweenLayersHelper([], _CurrentInCell, ResultMap) ->
  ResultMap;
getSynapsesBetweenLayersHelper([CurrentOutCell | TOut], CurrentInCell, ResultMap) ->
  getSynapsesBetweenLayersHelper(TOut, CurrentInCell, addSynapseInDendrite({CurrentInCell, CurrentOutCell}, getSynapse() , ResultMap)).

% Обход клеток входного слоя
getSynapsesBetweenLayers([], _To, ResultMap) ->
  ResultMap;
getSynapsesBetweenLayers([CurrentFrom | FromT], To,  ResultMap) ->
  getSynapsesBetweenLayers(FromT, To, getSynapsesBetweenLayersHelper(To, CurrentFrom, ResultMap)).

% Функция возвращает мапу с синапсами между входным и вЫходным слоем по ключу {CurrentInCell, CurrentOutCell}
getFeedForward() -> getSynapsesBetweenLayers('GlobalDataService':getAllInCells(), 'GlobalDataService':getAllOutCells(), #{}).

% Функция возвращает мапу с синапсами между вЫходным и входным слоем по ключу {CurrentOutCell, CurrentInCell}
getFeedBack() -> getSynapsesBetweenLayers('GlobalDataService':getAllOutCells(), 'GlobalDataService':getAllInCells(), #{}).



% Перебор клеток в мини-колонке
getAllInCellsByCells(CellIterator, AllInCells, ColumnRange) ->
  case maps:next(CellIterator) of
    none -> AllInCells;
    {CellGuid, _DendriteMap, NewCellIterator} ->
      getAllInCellsByCells(NewCellIterator, lists:append(AllInCells, [{ColumnRange, CellGuid}]), ColumnRange)
  end.

% Перебор мини-колонок во входном слое
getAllInCellsByMiniColumns(MiniColumnIterator, AllInCells) ->
  case maps:next(MiniColumnIterator) of
    none -> AllInCells;
    {ColumnRange, CellMap, NewMiniColumnIterator} ->
      getAllInCellsByMiniColumns(NewMiniColumnIterator,
        getAllInCellsByCells(maps:iterator(CellMap), AllInCells, ColumnRange))
  end.

% Функция возвращает номер мини-колонки и Guid всех клеток входного слоя
getAllInCells() -> getAllInCellsByMiniColumns(maps:iterator('GlobalDataService':getInLayer()), []).



% Перебор клеток в выходном слое.
% Да, дубль функции, но причины для изменения разные
getAllOutCellsHelper(CellIterator, AllOutCells) ->
  case maps:next(CellIterator) of
    none -> AllOutCells;
    {Range, {CellGuid, _Cell}, NewCellIterator} ->
      getAllOutCellsHelper(NewCellIterator, lists:append(AllOutCells, [{Range, CellGuid}]))
  end.

% Функция возвращает Guid всех клеток выходного слоя
getAllOutCells() -> getAllOutCellsHelper(maps:iterator('GlobalDataService':getOutLayer()), []).



initializeGlobalData() ->
  put(?ID, 0),
  'GlobalDataService':putInLayer(getInLayer()),
  'GlobalDataService':putOutLayer(getOutLayer()),
  'GlobalDataService':putAllInCells(getAllInCells()),
  'GlobalDataService':putAllOutCells(getAllOutCells()),
  'GlobalDataService':putFeedForward(getFeedForward()),
  'GlobalDataService':putFeedBack(getFeedBack()),
  ok.
