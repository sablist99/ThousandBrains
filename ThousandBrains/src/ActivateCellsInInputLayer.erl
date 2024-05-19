%%%-------------------------------------------------------------------
%%% @author Potap
%%% @copyright (C) 2024, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 19. май 2024 15:56
%%%-------------------------------------------------------------------
-module('ActivateCellsInInputLayer').
-author("Potap").

%% API
-export([getActiveCells/1]).

-include("Model.hrl").

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
      case 'CommonFunctions':existActiveApicalDendrite(CellGuid) of
        % Есть апикальный дендрит, поэтому добавляем текущую клетку к спсику активных
        true -> getAllPredictedCellsWithActiveApicalDendriteFromMiniColumn(NewIterator, lists:append(ActiveCells, [CellGuid]));
        false -> getAllPredictedCellsWithActiveApicalDendriteFromMiniColumn(NewIterator, ActiveCells)
      end
  end.


% Получение списка активных клеток в мини-колонке
% RangeOfColumn - номер мини-колонки (разряд)
% ActiveCells - out переменная, содержит информаци об активных клетках. Map<разряд колонки, [Guid активных клеток]>
% PredictedCells - Информация о деполяризованных клетках. Берется из глобальных данных
getActiveCellsInMiniColumn(RangeOfColumn, ActiveCells, PredictedCells) ->
  case 'CommonFunctions':hasMiniColumnInPredict(RangeOfColumn) of
    % Если нет - активируем все клетки
    false -> maps:put(RangeOfColumn,
      getCellsFromMapByIterator(
        maps:iterator( % Передаем в функцию итератор по мини-колонке
          maps:get(RangeOfColumn, % Получаем мини-колонку по разряду
            get(?InLayer) % Получаем исходную структуру данных
          )), []), ActiveCells);
    % Если да - проверяем, есть ли активный апикальный дендрит у предсказанных клеток
    true ->
      case 'CommonFunctions':hasActiveApicalDendriteInPredict(maps:iterator(PredictedCells)) of
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
% TODO Сделать прослойку для доступа к глобальным данным
  getActiveCellsHelper(FeedForward, #{}, get(?InPredictedCells)).
