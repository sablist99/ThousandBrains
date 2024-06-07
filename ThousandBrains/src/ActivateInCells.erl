%%%-------------------------------------------------------------------
%%% @author Potap
%%% @copyright (C) 2024, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 19. май 2024 15:56
%%%-------------------------------------------------------------------
-module('ActivateInCells').
-author("Potap").

%% API
-export([getActiveCells/1]).

% Получение списка Guid всех клеток из мапы (из мини-колонки) через итератор
% Iterator - итератор для перебора клеток
% ActiveCells - out переменная, список Guid активных клеток
getCellsFromMapByIterator(Iterator, ActiveCells) ->
  case maps:next(Iterator) of
    none -> ActiveCells;
    {CellGuid, _Value, NewIterator} ->
      getCellsFromMapByIterator(NewIterator, lists:append(ActiveCells, [CellGuid]))
  end.



% Получение списка Guid всех предсказанных клеток с апикальным дендритом (из мини-колонки)
% Iterator - итератор для перебора клеток
% ActiveCells - out переменная, список Guid активных клеток
getAllPredictedCellsWithActiveApicalDendriteFromMiniColumn(Iterator, ActiveCells) ->
  case maps:next(Iterator) of
    none -> ActiveCells;
    {CellGuid, _Value, NewIterator} ->
      case 'CommonFunctions':existActiveApicalDendriteByCellGuid(CellGuid) of
        % Есть апикальный дендрит, поэтому добавляем текущую клетку к списку активных
        {true, _ActiveOutCellRange} -> getAllPredictedCellsWithActiveApicalDendriteFromMiniColumn(NewIterator, lists:append(ActiveCells, [CellGuid]));
        false -> getAllPredictedCellsWithActiveApicalDendriteFromMiniColumn(NewIterator, ActiveCells)
      end
  end.



% Получение списка активных клеток в мини-колонке
% RangeOfColumn - номер мини-колонки (разряд), которая выстрелила
% ActiveCells - out переменная, содержит информаци об активных клетках. Map<разряд колонки, [Guid активных клеток]>
% PredictedCells - Информация о деполяризованных клетках. Берется из глобальных данных
getActiveCellsInMiniColumn(RangeOfColumn, ActiveCells, PredictedCells) ->
  % Колонка была ранее предсказана?
  case 'CommonFunctions':hasMiniColumnInPredict(RangeOfColumn) of
    % Если нет - активируем все клетки
    false -> maps:put(RangeOfColumn,
      getCellsFromMapByIterator(
        maps:iterator( % Передаем в функцию итератор по мини-колонке
          maps:get(RangeOfColumn, % Получаем мини-колонку по разряду
            'GlobalDataService':getInLayer() % Получаем исходную структуру данных
          )), []), ActiveCells);
    % Если да - проверяем, есть ли активный апикальный дендрит у предсказанных клеток
    true ->
      case 'CommonFunctions':existActiveApicalDendriteByRanges(RangeOfColumn) of
        % Если нет - активируем все предсказанные клетки
        false -> maps:put(RangeOfColumn, getCellsFromMapByIterator(
          maps:iterator( % Передаем в функцию итератор по мини-колонке
            maps:get(RangeOfColumn, PredictedCells) % Получаем мини-колонку по разряду
          ), []), ActiveCells);
        % Если да - активируем клетки с активным апикальным дендритом
        true -> maps:put(RangeOfColumn, getAllPredictedCellsWithActiveApicalDendriteFromMiniColumn(maps:iterator(PredictedCells), []), ActiveCells)
      end
  end.



% Перебор всех выстреливших мини-колонок
% Первый аргумент - входной сигнал (список разрядов)
% ActiveCells - out переменная, содержит информаци об активных клетках. Map<разряд колонки, [Guid активных клеток]>
% PredictedCells - Информация о деполяризованных клетках. Берется из глобальных данных
getActiveCellsHelper([], ActiveCells, _PredictedCells) ->
  % Когда обработан весь FeedForward
  ActiveCells;
getActiveCellsHelper([RangeOfColumnWithFeedForward | TFeedForward], ActiveCells, PredictedCells) ->
  % Обрабатываем очередной сигнал из FeedForward
  getActiveCellsHelper(TFeedForward, getActiveCellsInMiniColumn(RangeOfColumnWithFeedForward, ActiveCells, PredictedCells), PredictedCells).



% Функция возвращает активные клетки. Данные упакованы в иерархию, аналогичную структуре хранения данных
% FeedForward - входной сигнал (список разрядов)
getActiveCells(FeedForward) ->
  getActiveCellsHelper(FeedForward, #{}, 'GlobalDataService':getInPredictedCells()).
