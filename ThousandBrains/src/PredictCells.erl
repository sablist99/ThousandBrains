%%%-------------------------------------------------------------------
%%% @author Potap
%%% @copyright (C) 2024, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 19. май 2024 14:57
%%%-------------------------------------------------------------------
-module('PredictCells').
-author("Potap").

%% API
-export([getPredictedCellsInInputLayer/1, getPredictedCellsInOutputLayer/3]).

-include("Model.hrl").

% В этой функции определяется есть ли на дендрите достаточное количество синапсов, соответствующих входному сигналу
% Первый аргумент - Входной сигнал (L - для входного слоя, Выходной слой - для выходного слоя)
% SynapseMap - Мапа со значениями постоянства синапсов. Берется из входного слоя (Map<Range, Synapse>)
% SynapseCount - Количество существующих синапсов (соответствующих входному сигналу)
% Theta - Порог активации дендрита
passedThetaInBThreshold([], _SynapsesMap, SynapseCount, Theta) when SynapseCount >= Theta ->
  % Если порог активации дендрита пройден
  true;
passedThetaInBThreshold([], _SynapsesMap, _SynapseCount, _Theta) ->
  % Если порог активации НЕ пройден, то дендрит не добавляется
  false;
passedThetaInBThreshold([Range|T], SynapsesMap, SynapseCount, Theta) ->
  % Рекурсивная обработка входных сигналов
  case 'CommonFunctions':existSynapseInMap(Range, SynapsesMap) of
    {1, _Value} -> passedThetaInBThreshold(T, SynapsesMap, SynapseCount + 1, Theta);
    0 -> passedThetaInBThreshold(T, SynapsesMap, SynapseCount, Theta)
  end.



% В этой функции собираем активные дендриты клетки
% Signal - передается дальше для сравнения
% DendriteIterator - итератор для перебора мапы с дендритами
% ActiveDendrites - out переменная, список для сохранения идентификаторов активных дендритов
% Theta - Порог активации дендрита
findActiveDendrites(Signal, DendriteIterator, ActiveDendrites, Theta) ->
  case maps:next(DendriteIterator) of
    % Когда перебрали все дендриты в мапе - возвращаем список GUID активных деднритов
    none -> ActiveDendrites;
    {DendriteGuid, SynapsesMap, NewDendriteIterator} ->
      case passedThetaInBThreshold(Signal, SynapsesMap, 0, Theta) of
        % Если на дендрите есть достаточно синапсов, то добавляем GUID дендрита в список активных
        true -> findActiveDendrites(Signal, NewDendriteIterator, lists:append(ActiveDendrites, [DendriteGuid]), Theta);
        % Иначе просто переходим на следующую итерацию
        false -> findActiveDendrites(Signal, NewDendriteIterator, ActiveDendrites, Theta)
      end
  end.



% В этой функции определяем предсказанные клетки
% Signal - передается дальше для сравнения
% CellIterator - итератор для перебора мапы с клетками
% PredictedCells - мапа, где ключ - Guid предсказанной клетки, значение - список Guid дендритов, которые привели к предсказанию
% TODO Подумать, как избежать дублирования кода в case по поиску апикального дендрита
findPredictedCells(Signal, CellIterator, PredictedCells) ->
  case maps:next(CellIterator) of
    % Когда перебрали все клетки в мапе (мини-столбце). Сразу вернем количество объектов в мапе, потому что сопоставление по пустоте недопустимо
    none -> {map_size(PredictedCells), PredictedCells};
    {CellGuid, DendriteMap, NewCellIterator} ->
      % Определяем, есть ли активные дендриты на клетке
      case 'CommonFunctions':existActiveApicalDendrite(CellGuid) of
        % Есть апикальный дендрит
        {true, OutActiveCellRange} ->
          case findActiveDendrites(Signal, maps:iterator(DendriteMap), [], ?THETA_IN_B_MIN) of
            % Если функция вернула пустой список, значит нет активных дендритов, а значит, клетка не станет предсказанной. Просто переходим к следующей итерации
            [] -> findPredictedCells(Signal, NewCellIterator, PredictedCells);
            % Иначе возвращен список активных дендритов, значит клетка предсказана (Так как достаточно одного активного дендрита)
            ActiveDendrites -> findPredictedCells(Signal, NewCellIterator, maps:put(CellGuid, {OutActiveCellRange, ActiveDendrites}, PredictedCells))
          end;
        % Нет апикального дендрита
        false ->
          case findActiveDendrites(Signal, maps:iterator(DendriteMap), [], ?THETA_IN_B_MAX) of
            % Если функция вернула пустой список, значит нет активных дендритов, а значит, клетка не станет предсказанной. Просто переходим к следующей итерации
            [] -> findPredictedCells(Signal, NewCellIterator, PredictedCells);
            % Иначе возвращен список активных дендритов, значит клетка предсказана (Так как достаточно одного активного дендрита)
            ActiveDendrites -> findPredictedCells(Signal, NewCellIterator, maps:put(CellGuid, {?NoActiveApicalDendrite, ActiveDendrites}, PredictedCells))
          end
      end
  end.



% В этой функции определяем мини-колонки с деполяризованными клетками
findMiniColumnWithPredictedCells(Signal, MiniColumnIterator, MiniColumns) ->
  case maps:next(MiniColumnIterator) of
    % Когда перебрали все клетки в мапе (мини-столбце)
    none -> MiniColumns;
    {ColumnRange, CellMap, NewMiniColumnIterator} ->
      case findPredictedCells(Signal, maps:iterator(CellMap), #{}) of
        % Если функция вернула пустую мапу, значит нет предсказанных клеток в мини-колонке. Просто переходим к следующей итерации
        {0, _} -> findMiniColumnWithPredictedCells(Signal, NewMiniColumnIterator, MiniColumns);
        {_, PredictedCells} -> findMiniColumnWithPredictedCells(Signal, NewMiniColumnIterator, maps:put(ColumnRange, PredictedCells, MiniColumns))
      end
  end.



% Функция возвращает предсказанные клетки и дендриты, которые привели к деполяризации. Данные упакованы в иерархию, аналогичную структуре хранения данных
getPredictedCellsInInputLayer(Signal) ->
  findMiniColumnWithPredictedCells(Signal, maps:iterator('GlobalDataService':getInLayer()), #{}).



% Функция возвращает предсказанные клетки в выходном слое, основываясь на активных клетках предыдущего шага
% Первый аргумент - сигнал, активные клетки с предыдущего шага - мапа
getPredictedCellsInOutputLayer([], _OutCellsIterator, PredictedCells) ->
  PredictedCells;
getPredictedCellsInOutputLayer(ActiveCellsInOutputLayerOnPreviousTimeStep, OutCellsIterator, PredictedCells) ->
  case maps:next(OutCellsIterator) of
    none -> PredictedCells;
    {Range, {_OutCellGuid, DendriteMap}, NewCellIterator} ->
      case findActiveDendrites(ActiveCellsInOutputLayerOnPreviousTimeStep, maps:iterator(DendriteMap), [], ?THETA_OUT_B) of
        [] -> getPredictedCellsInOutputLayer(ActiveCellsInOutputLayerOnPreviousTimeStep, NewCellIterator, PredictedCells);
        ActiveDendrites -> getPredictedCellsInOutputLayer(ActiveCellsInOutputLayerOnPreviousTimeStep, NewCellIterator, maps:put(Range, ActiveDendrites, PredictedCells))
      end
  end.
