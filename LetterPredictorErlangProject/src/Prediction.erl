-module('Prediction').

-include("Model.hrl").

%% API
-export([selectWinColumns/1, selectActiveCells/4, selectPredictedCells/4, selectCellAndSegmentForPattern/4, searchCorrectlyPredictedCells/4, rewardSynapses/3]).


% В зависимости от способа хранения, может быть несколько столбцов, отвечающих за один символ
% Поэтому нужна соответствующая прослойка, чтобы была возможность для расширения
% Текущая реализация преполагает, что входящий символ соответсвует номеру столбца.
selectWinColumns(InputSymbol) ->
  [InputSymbol].


% Ищем в списке пар координат пару с заданным Y
doesLayerContainCoordinate(_Y, []) -> 1; %Если так и не встретили Y
doesLayerContainCoordinate(Y, [{_X_H, Y_H}, _Layer_T]) when Y == Y_H -> 0; % Если есть хотя бы один кортеж с Y
doesLayerContainCoordinate(Y, [_H, Layer_T]) -> doesLayerContainCoordinate(Y, Layer_T). % Перебираем дальше

% Вычисление состояния клетки
computeOneCellStateHelper(X_Coordinate, Y_Coordinate, PredictionLayerSate) ->
  case lists:member({X_Coordinate, Y_Coordinate}, PredictionLayerSate) of
    true -> 1;
    false -> doesLayerContainCoordinate(Y_Coordinate, PredictionLayerSate)
  end.

% Вычисление состояния клетки
% cell = (x,y), prev_brain_state = predict_all_cell_states(t-1), W = набор столбцов
computeOneCellState(X_Coordinate, Y_Coordinate, PredictionLayerSate, WinColumns) ->
  case lists:member(Y_Coordinate, WinColumns) of
    true -> computeOneCellStateHelper(X_Coordinate, Y_Coordinate, PredictionLayerSate);
    false -> 0
  end.


% Добавление координат к списку, если выполнено условие
appendCoordinateByCondition(TargetList, X, Y, State) when State == 1; State == true ->
  lists:append(TargetList, [{X, Y}]);
appendCoordinateByCondition(TargetList, _X, _Y, _State) ->
  TargetList.

getCoordinateForCycle(_X, Y, Y_Target) when Y == Y_Target - 1 ->
  {x + 1, 0};
getCoordinateForCycle(X, Y, _Y_Target) ->
  {X, Y + 1}.

% Возвращение активных клеток слоя
% По сути, это двойной цикл FOR
selectActiveCellsHelper({X, Y}, X_Target, Y_Target, _PrevPredictionState, _WinColumns, LayerState) when X == X_Target - 1, Y == Y_Target - 1 ->
  LayerState;
selectActiveCellsHelper({X, Y}, _X_Target, Y_Target, PrevPredictionState, WinColumns, LayerState) ->
  selectActiveCellsHelper(getCoordinateForCycle(X, Y, Y_Target), _X_Target, _Y_Target, PrevPredictionState, WinColumns,
    appendCoordinateByCondition(LayerState, X, Y, % Добавляем клетку, если она активная
      computeOneCellState(X, Y, PrevPredictionState, WinColumns))). % Определяем активность клетки

% Возвращение активных клеток слоя
selectActiveCells(X_Target, Y_Target, PrevPredictionState, WinColumns) ->
  selectActiveCellsHelper({0, 0}, X_Target, Y_Target, PrevPredictionState, WinColumns, []).


% Обновление активности клетки (В состоянии предсказания или нет)
getDendriteByCellInLayer({X, Y}, Layer) ->
  lists:nth(Y, lists:nth(X, Layer)).

getSynapseByCellInDendrite({X, Y}, Dendrite) ->
  lists:nth(Y, lists:nth(X, Dendrite)).

getSynapseWeight(Synapse) ->
  Synapse#synapse.permanenceWeight.

incrementCount(Weight, Count) when Weight == 1 -> Count + 1;
incrementCount(_Weight, Count) -> Count.

compareThetaAndActiveCells(Count) ->
  if Count >= ?THETA -> 1;
    true -> 0
  end.

isCellInPredictionStateHelper(_X, _Y, _Layer, [], Count) -> Count;
isCellInPredictionStateHelper(X, Y, Layer, [LayerState_H, LayerState_T], Count) ->
  isCellInPredictionStateHelper(X, Y, Layer, LayerState_T,
    incrementCount(
      getSynapseWeight(
        getSynapseByCellInDendrite({X, Y},
          getDendriteByCellInLayer(LayerState_H, Layer)
        )
      ),
      Count
    )
  ).

% Не факт, что название соответствует тому, что функция делает в действительности:)
isCellInPredictionState(X, Y, Layer, LayerState) ->
  compareThetaAndActiveCells(isCellInPredictionStateHelper(X, Y, Layer, LayerState, 0)).


% Вычисление матрицы предсказания
selectPredictedCellsHelper({X, Y}, X_Target, Y_Target, _Layer, _ActiveLayerState, PredictionMatrix) when X == X_Target - 1, Y == Y_Target - 1 ->
  PredictionMatrix;
selectPredictedCellsHelper({X, Y}, X_Target, Y_Target, Layer, ActiveLayerState, PredictionMatrix) ->
  selectPredictedCellsHelper(getCoordinateForCycle(X, Y, Y_Target), X_Target, Y_Target, Layer, ActiveLayerState,
    appendCoordinateByCondition(PredictionMatrix, X, Y,
      isCellInPredictionState(X, Y, Layer, ActiveLayerState))).

selectPredictedCells(X_Target, Y_Target, Layer, ActiveLayerState) ->
  selectPredictedCellsHelper({0, 0}, X_Target, Y_Target, Layer, ActiveLayerState, []).




intToBool(Expression) when Expression == 0 ->
  false;
intToBool(_Expression) ->
  true.

% Клетки, которые сделали верное предсказание
% TODO в пердыдущей реализации (на python) сделано не верно, поощерать нужно другие клетки, см. оригинальную стсатью Хоккинса
searchCorrectlyPredictedCellsHelper([], _Layer, _PrevActiveLayer, _PrevPredictionLayer, ReinforcementCells) ->
  ReinforcementCells;
searchCorrectlyPredictedCellsHelper([{X, Y} | ActiveLayer_T], Layer, PrevActiveLayer, PrevPredictionLayer, ReinforcementCells) ->
  searchCorrectlyPredictedCellsHelper(ActiveLayer_T, Layer, PrevActiveLayer, PrevPredictionLayer,
    appendCoordinateByCondition(ReinforcementCells, X, Y,
      intToBool(isCellInPredictionState(X, Y, Layer, PrevActiveLayer)) and lists:any({X, Y}, PrevPredictionLayer))).


searchCorrectlyPredictedCells(ActiveLayer, Layer, PrevActiveLayer, PrevPredictionLayer) ->
  searchCorrectlyPredictedCellsHelper(ActiveLayer, Layer, PrevActiveLayer, PrevPredictionLayer, []).



-record(targetValues, {
  column :: integer(),
  maxValue :: integer(),
  maxCellCoordinates_X :: integer(),
  maxCellCoordinates_Y :: integer(),
  targetCoordinates_X :: integer(),
  targetCoordinates_Y :: integer()}).

% TODO Подумать о вынесении обращения к двумерному массиву в отдельную функцию
setValuesForPattern([], _TargetValues) ->
  true;
setValuesForPattern([CurrentDendrite | Dendrites_T], TargetValues) ->
  Synapse = lists:nth(TargetValues#targetValues.targetCoordinates_Y, lists:nth(TargetValues#targetValues.targetCoordinates_X, CurrentDendrite)),
  Synapse#synapse{permanenceValue = ?PERMANENCE_WEIGHT_BORDER, permanenceWeight = 1},
  setValuesForPattern(Dendrites_T, TargetValues).



updateDendritesForPattern(Layer, TargetValues) ->
  setValuesForPattern(lists:nth(TargetValues#targetValues.maxCellCoordinates_Y, lists:nth(TargetValues#targetValues.maxCellCoordinates_X, Layer)), TargetValues).



updateMaxValuesForPattern(TargetValues, CurrentDendritePermanenceValue, {PrevActiveCell_X, PrevActiveCell_Y}, M)
  when CurrentDendritePermanenceValue > TargetValues#targetValues.maxValue ->
  TargetValues#targetValues{
    maxValue = CurrentDendritePermanenceValue,
    maxCellCoordinates_X = PrevActiveCell_X,
    maxCellCoordinates_Y = PrevActiveCell_Y,
    targetCoordinates_X = M,
    targetCoordinates_Y = TargetValues#targetValues.column};
updateMaxValuesForPattern(TargetValues, _CurrentDendritePermanenceValue, {_PrevActiveCell_X, _PrevActiveCell_Y}, _M) ->
  TargetValues.



selectCellAndSegmentForPatternColumnHelper(TargetValues, _PrevActiveCellCoordinates, _CurrentDendrite, CurrentM, TargetM) when CurrentM == TargetM ->
  TargetValues;
selectCellAndSegmentForPatternColumnHelper(TargetValues, PrevActiveCellCoordinates, CurrentDendrite, CurrentM, TargetM) ->
  selectCellAndSegmentForPatternColumnHelper(
    updateMaxValuesForPattern(
      TargetValues,
      lists:nth(CurrentM, lists:nth(TargetValues#targetValues.column, CurrentDendrite))#synapse.permanenceValue,
      PrevActiveCellCoordinates,
      CurrentM),
    PrevActiveCellCoordinates, CurrentDendrite, CurrentM + 1, TargetM
  ).



selectCellAndSegmentForPatternDendriteHelper(TargetValues, [], _PrevActiveCellCoordinates, _TargetM) ->
  TargetValues;
selectCellAndSegmentForPatternDendriteHelper(TargetValues, [CurrentDendrite | Dendrites_T], PrevActiveCellCoordinates, TargetM) ->
  selectCellAndSegmentForPatternDendriteHelper(
    selectCellAndSegmentForPatternColumnHelper(
      TargetValues, PrevActiveCellCoordinates, CurrentDendrite, 0, TargetM
    ),
    Dendrites_T, PrevActiveCellCoordinates, TargetM
  ).



selectCellAndSegmentForPatternCellHelper(TargetValues, Layer, [], _TargetM) ->
  updateDendritesForPattern(Layer, TargetValues);
selectCellAndSegmentForPatternCellHelper(TargetValues, Layer, [{X_PALS, Y_PALS} | PrevActiveLayerState_T], TargetM) ->
  selectCellAndSegmentForPatternCellHelper(
    selectCellAndSegmentForPatternDendriteHelper(
      TargetValues, lists:nth(Y_PALS, lists:nth(X_PALS, Layer)), {X_PALS, Y_PALS}, TargetM
    ),
    Layer, PrevActiveLayerState_T, TargetM
  ).



selectCellAndSegmentForPattern(Layer, WinColumns, PrevActiveLayerState, TargetM) ->
  selectCellAndSegmentForPatternCellHelper(#targetValues{
    column = lists:nth(0, WinColumns),
    maxValue = -1,
    maxCellCoordinates_X = -1,
    maxCellCoordinates_Y = -1,
    targetCoordinates_X = -1,
    targetCoordinates_Y = -1},
    Layer, PrevActiveLayerState, TargetM).

% TODO Возможно, нужно контролировать выход за единицу. Уточнить в оригинальной статье
rewardOneSynapseInDendrite(Synapse) when Synapse#synapse.permanenceWeight == 1 ->
  Synapse#synapse{permanenceWeight = Synapse#synapse.permanenceWeight + ?P_PLUS}.



rewardSynapsesInCell([], {_CurrentCorrectlyCell_X, _CurrentCorrectlyCell_Y}) ->
  false;
rewardSynapsesInCell([CurrentDendrite | _Dendrites_T], {CurrentCorrectlyCell_X, CurrentCorrectlyCell_Y}) ->
  rewardOneSynapseInDendrite(lists:nth(CurrentCorrectlyCell_Y, lists:nth(CurrentCorrectlyCell_X, CurrentDendrite))),
  rewardSynapsesInCell(_Dendrites_T, {CurrentCorrectlyCell_X, CurrentCorrectlyCell_Y}).



rewardSynapsesInLayer(Layer, {CurrentActiveCell_X, CurrentActiveCell_Y}, {CurrentCorrectlyCell_X, CurrentCorrectlyCell_Y}) ->
  rewardSynapsesInCell(lists:nth(CurrentActiveCell_Y, lists:nth(CurrentActiveCell_X, Layer)), {CurrentCorrectlyCell_X, CurrentCorrectlyCell_Y}).



rewardSynapsesInPrevActiveLayerState(_Layer, [], _CurrentCorrectlyCell) ->
  false;
rewardSynapsesInPrevActiveLayerState(Layer, [{PALS_X, PALS_Y} | PALS_T], CurrentCorrectlyCell) ->
  rewardSynapsesInLayer(Layer, {PALS_X, PALS_Y}, CurrentCorrectlyCell),
  rewardSynapsesInPrevActiveLayerState(Layer, PALS_T, CurrentCorrectlyCell).



rewardSynapsesInCurrentlyPredirectedCells(_Layer, _PrevActiveLayerState, []) ->
  false;
rewardSynapsesInCurrentlyPredirectedCells(Layer, PrevActiveLayerState, [{CurrentCorrectlyCell_X, CurrentCorrectlyCell_Y} | CurrentCorrectlyCell_T]) ->
  rewardSynapsesInPrevActiveLayerState(Layer, PrevActiveLayerState, {CurrentCorrectlyCell_X, CurrentCorrectlyCell_Y}),
  rewardSynapsesInCurrentlyPredirectedCells(Layer, PrevActiveLayerState, CurrentCorrectlyCell_T).


% По сути, лишняя прослойка, но с ней нагляднее.
rewardSynapses(Layer, PrevActiveLayerState, CurrentCorrectlyCell) ->
  rewardSynapsesInCurrentlyPredirectedCells(Layer, PrevActiveLayerState, CurrentCorrectlyCell).














