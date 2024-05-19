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



% Функция создания дендрита клетки входного слоя
% CurrentSynapse - счетчик, количество синапсов на текущем дендрите.
% Dendrite - out переменная, в которой копится результат (Map)
getInDendriteHelper(CurrentSynapse, Dendrite) when CurrentSynapse == ?N_EXT ->
  Dendrite;
getInDendriteHelper(CurrentSynapse, Dendrite) ->
  getInDendriteHelper(CurrentSynapse + 1, maps:put(CurrentSynapse, getSynapse(), Dendrite)).

getInDendrite() -> getInDendriteHelper(0, #{}).



% Функция создания клетки входного слоя
% CurrentDendrite - счетчик, количество дендритов на текущей клетке.
% Cell - out переменная, в которой копится результат (Map)
getInCellHelper(CurrentDendrite, Cell) when CurrentDendrite == ?D ->
  Cell;
getInCellHelper(CurrentDendrite, Cell) ->
  getInCellHelper(CurrentDendrite + 1, maps:put('MyMath':getGuid(), getInDendrite(), Cell)).

getInCell() -> getInCellHelper(0, #{}).



% Функция создания мини-колонки входного слоя
getInMiniColumnHelper(CurrentCell, MiniColumn) when CurrentCell == ?M ->
  MiniColumn;
getInMiniColumnHelper(CurrentCell, MiniColumn) ->
  getInMiniColumnHelper(CurrentCell + 1, maps:put('MyMath':getGuid(), getInCell(), MiniColumn)).

getInMiniColumn() -> getInMiniColumnHelper(0, #{}).



% Функция создания входного слоя одной колонки
getInLayerHelper(CurrentMiniColumn, Layer) when CurrentMiniColumn == ?N_IN ->
  Layer;
getInLayerHelper(CurrentMiniColumn, Layer) ->
  getInLayerHelper(CurrentMiniColumn + 1, maps:put(CurrentMiniColumn, getInMiniColumn(), Layer)).

getInLayer() -> getInLayerHelper(0, #{}).


initializeGlobalData() ->
  put(inputLayer, getInLayer()),
  put(apicalDendrites, []).
