%%%-------------------------------------------------------------------
%%% @author Potap
%%% @copyright (C) 2024, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 19. май 2024 14:57
%%%-------------------------------------------------------------------
-module('PredictCellsInInputLayer').
-author("Potap").

%% API
-export([getPredictedCells/1]).

-include("Model.hrl").

% Проверка существования синапса
existSynapseInMap(Range, SynapsesMap) ->
  case maps:find(Range, SynapsesMap) of
    {ok, Value} ->
      case Value#synapse.permanenceWeight of
        true -> 1;
        false -> 0
      end;
    _ -> 0
  end.



% Определяем порог активации дендрита (по наличию апикального дендрита)
getThetaInB(CellGuid) ->
  case lists:member(CellGuid, get(apicalDendrites)) of
    true -> ?THETA_IN_B_MIN;
    _ -> ?THETA_IN_B_MAX
  end.


% В этой функции определяется есть ли на дендрите достаточное количество синапсов, соответствующих входному сигналу
% Первый аргумент - Входной сигнал L
% SynapseMap - Мапа со значениями постоянства синапсов. Берется из входного слоя (Map<Range, Synapse>)
% SynapseCount - Количество существующих синапсов (соответствующих входному сигналу)
% ThetaInB - Порог активации дендрита
passedThetaInBThreshold([], _SynapsesMap, SynapseCount, ThetaInB) when SynapseCount >= ThetaInB ->
  % Если порог активации дендрита пройден
  true;
passedThetaInBThreshold([], _SynapsesMap, _SynapseCount, _ThetaInB) ->
  % Если порог активации НЕ пройден, то дендрит не добавляется
  false;
passedThetaInBThreshold([Range|T], SynapsesMap, SynapseCount, ThetaInB) ->
  % Рекурсивная обработка входных сигналов
  passedThetaInBThreshold(T, SynapsesMap, SynapseCount + existSynapseInMap(Range, SynapsesMap), ThetaInB).



% В этой функции собираем активные дендриты клетки
% Signal - передается дальше для сравнения
% DendriteIterator - итератор для перебора мапы с дендритами
% ActiveDendrites - out переменная, список для сохранения идентификаторов активных дендритов
% ThetaInB - Порог активации дендрита
findActiveDendrites(Signal, DendriteIterator, ActiveDendrites, ThetaInB) ->
  case maps:next(DendriteIterator) of
    % Когда перебрали все дендриты в мапе - возвращаем список GUID активных деднритов
    none -> ActiveDendrites;
    {DendriteGuid, SynapsesMap, NewDendriteIterator} ->
      case passedThetaInBThreshold(Signal, SynapsesMap, 0, ThetaInB) of
        % Если на дендрите есть достаточно синапсов, то добавляем GUID дендрита в список активных
        true -> findActiveDendrites(Signal, NewDendriteIterator, lists:append(ActiveDendrites, [DendriteGuid]), ThetaInB);
        % Иначе просто переходим на следующую итерацию
        false -> findActiveDendrites(Signal, NewDendriteIterator, ActiveDendrites, ThetaInB)
      end
  end.



% В этой функции определяем предсказанные клетки
% Signal - передается дальше для сравнения
% CellIterator - итератор для перебора мапы с клетками
% PredictedCells - мапа, где ключ - Guid предсказанной клетки, значение - список Guid дендритов, которые привели к предсказанию
findPredictedCells(Signal, CellIterator, PredictedCells) ->
  case maps:next(CellIterator) of
    % Когда перебрали все клетки в мапе (мини-столбце). Срузе вернем количество объектов в мапе, потому что сопоставление по пустоте недопустимо
    none -> {map_size(PredictedCells), PredictedCells};
    {CellGuid, DendriteMap, NewCellIterator} ->
      % Определяем, есть ли активные дендриты на клетке
      case findActiveDendrites(Signal, maps:iterator(DendriteMap), [], getThetaInB(CellGuid)) of
        % Если функция вернула пустой список, значит нет активных дендритов, а значит, клетка не станет предсказанной. Просто переходим к следующей итерации
        [] -> findPredictedCells(Signal, NewCellIterator, PredictedCells);
        % Иначе возвращен список активных дендритов, значит клетка предсказана (Так как достаточно одного активного дендрита)
        ActiveDendrites -> findPredictedCells(Signal, NewCellIterator, maps:put(CellGuid, ActiveDendrites, PredictedCells))
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
getPredictedCells(Signal) ->
  findMiniColumnWithPredictedCells(Signal, maps:iterator(get(inputLayer)), #{}).