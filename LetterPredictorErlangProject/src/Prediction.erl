-module('Prediction').

-include("Model.hrl").

%% API
-export([selectWinColumns/1]).

-define(THETA, 3).


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
  selectPredictedCellsHelper ({0, 0}, X_Target, Y_Target, Layer, ActiveLayerState, []).




intToBool(Expression) when Expression == 0 ->
  false;
intToBool(_Expression) ->
  true.

% Клетки, которые сделали верное предсказание
searchCorrectlyPredictedCellsHelper([], _Layer, _PrevActiveLayer, _PrevPredictionLayer, ReinforcementCells) ->
  ReinforcementCells;
searchCorrectlyPredictedCellsHelper([{X, Y} | ActiveLayer_T], Layer, PrevActiveLayer, PrevPredictionLayer, ReinforcementCells) ->
  searchCorrectlyPredictedCellsHelper(ActiveLayer_T, Layer, PrevActiveLayer, PrevPredictionLayer,
    appendCoordinateByCondition(ReinforcementCells, X, Y,
      intToBool(isCellInPredictionState(X, Y, Layer, PrevActiveLayer)) and lists:any({X, Y}, PrevPredictionLayer))).


searchCorrectlyPredictedCells(ActiveLayer, Layer, PrevActiveLayer, PrevPredictionLayer) ->
  searchCorrectlyPredictedCellsHelper(ActiveLayer, Layer, PrevActiveLayer, PrevPredictionLayer, []).




selectCellAndSegmentForPatternHelper(Layer, PrevActiveLayerState, Stolb, MaxCellCoords, TargetCoords, MaxCount, MaxDendriteIndex, MaxValue) ->
  .

selectCellAndSegmentForPattern(Layer, WinColumns, PrevActiveLayerState) ->
  selectCellAndSegmentForPatternHelper(Layer, PrevActiveLayerState, lists:nth(0, WinColumns), {-1, -1}, [], -1, -1, -1).


