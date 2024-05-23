%%%-------------------------------------------------------------------
%%% @author Potap
%%% @copyright (C) 2024, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 20. май 2024 0:43
%%%-------------------------------------------------------------------
-module('ActivateOutCells').
-author("Potap").

%% API
-export([getOutWinCells/0, getActivateCells/0]).

-include("Model.hrl").

% Описание алгоритма
% Берем клетку выходного слоя. И для каждой такой клетки перебираем активные клетки входного слоя.
% По двум guid получаем синапс из FeedForward.
% Если синапс есть, то сохраняем его.
% Когда кончились активные клетки, смотрим на количество сохраненных синапсов.
% Если их достаточное количество, то сохраняем range колонки и список синапсов, приведших к активации

% В этой функции происходит формирование списка существующих синапсов между выходной клеткой и активными клетками ОДНОЙ МИНИ-КОЛОНКИ входного слоя.
% ActiveCellGuid - текущая активная клетка, для которой ищем синапс с выходной клеткой
% OutCellGuid - текущая выходная клетка, для которой ищем синапсы с активными клеткой
% Synapses - out параметр, результирующая мапа, содержащая существующие синапсы
getSynapsesListBetweenInActiveCellsFromOneMiniColumnAndOutCell([], _OutCellGuid, Synapses) ->
  Synapses;
getSynapsesListBetweenInActiveCellsFromOneMiniColumnAndOutCell([ActiveCellGuid | ActiveCells], OutCellGuid, Synapses) ->
  case 'CommonFunctions':existSynapseInFeedForwardMap(ActiveCellGuid, OutCellGuid, 'GlobalDataService':getFeedForward()) of
    {1, _Value} ->
      getSynapsesListBetweenInActiveCellsFromOneMiniColumnAndOutCell(ActiveCells, OutCellGuid, lists:append(Synapses, [{ActiveCellGuid, OutCellGuid}]));
    0 ->
      getSynapsesListBetweenInActiveCellsFromOneMiniColumnAndOutCell(ActiveCells, OutCellGuid, Synapses)
  end.



% В этой функции происходит формирование списка существующих синапсов между выходной клеткой и активными клетками входного слоя
% Вместе с этим проверяется, а достаточно ли этих синапсов для активации
getSynapsesListBetweenInActiveCellsAndOutCell(ActiveCellsIterator, OutCellGuid, Synapses) ->
  case maps:next(ActiveCellsIterator) of
    % Проверяем - а достаточно ли синапсов у текущей выходной клетки
    none ->
      case 'CommonFunctions':getListSize(Synapses) > ?THETA_OUT_P of
        true -> Synapses;
        false -> []
      end;
    {_Range, ActiveCellsList, NewCellIterator} ->
      getSynapsesListBetweenInActiveCellsAndOutCell(NewCellIterator, OutCellGuid, lists:append(Synapses, getSynapsesListBetweenInActiveCellsFromOneMiniColumnAndOutCell(ActiveCellsList, OutCellGuid, Synapses)))
  end.



% В этой функции формируется список выигрышных клеток выходного слоя (тех клеток, у которых достаточно синапсов с активными кдетками входного слоя)
getOutWinCellsHelper(OutCellsIterator, OutWin) ->
  case maps:next(OutCellsIterator) of
    none -> OutWin;
    {Range, {OutCellGuid, _}, NewCellIterator} ->
      case getSynapsesListBetweenInActiveCellsAndOutCell(maps:iterator('GlobalDataService':getInActiveCells()), OutCellGuid, []) of
        % Если связь не прочная или ее вообще нет
        [] -> getOutWinCellsHelper(NewCellIterator, OutWin);
        % Если есть достаточно синапсов, то объявляем клетку выигрышной
        Synapses -> getOutWinCellsHelper(NewCellIterator, maps:put(Range, Synapses, OutWin))
      end
  end.


% Получение выигрышных клеток выходного слоя
getOutWinCells() ->
  getOutWinCellsHelper(maps:iterator('GlobalDataService':getOutLayer()), #{}).



% Выборка активных клеток с учетом латерального сигнала
getActiveCellsWithKsi(WinOutIterator, Ksi, ActiveList) ->
  case maps:next(WinOutIterator) of
    none -> ActiveList;
    {Range, List, NewWinOutIterator} ->
      case 'CommonFunctions':getListSize(List) of
        Size -> case Size >= Ksi of
                  true -> getActiveCellsWithKsi(NewWinOutIterator, Ksi, lists:append(ActiveList, [Range]));
                  false -> getActiveCellsWithKsi(NewWinOutIterator, Ksi, ActiveList)
                end
      end
  end.

getActiveCellsWithKsi(WinOut, Ksi) ->
  getActiveCellsWithKsi(maps:iterator(WinOut), Ksi, []).



% Функция возвращает максимальное количество активных базальных дендритов
% TODO Узнать, как записать значение вызова в функцию и использовать его дальше. При этом не потеряв функциональный стиль
getKsi(PredictedCellsIterator, Ksi) ->
  case maps:next(PredictedCellsIterator) of
    none -> Ksi;
    {_Range, List, NewCellIterator} ->
      case 'CommonFunctions':getListSize(List) of
        Size -> case Size > Ksi of
                  true -> getKsi(NewCellIterator, Size);
                  false -> getKsi(NewCellIterator, Ksi)
                end
      end
  end.

getKsi(PredictedCells) ->
  getKsi(maps:iterator(PredictedCells), 0).



% Получение активных клетов выходного слоя.
% С учетом прямого и латерального сигнала
getActivateCells([]) ->
  getActiveCellsWithKsi(getOutWinCells(), 0);
getActivateCells(undefined) ->
  getActiveCellsWithKsi(getOutWinCells(), 0);
getActivateCells(OutPreviousActivation) ->
  case 'PredictCells':getPredictedCellsInOutputLayer(OutPreviousActivation, maps:iterator('GlobalDataService':getOutLayer()), #{}) of
    PredictedCells ->
      % Количество клеток с боковой поддержкой больше, чем минимальное количество активных клеток
      case maps:size(PredictedCells) >= ?S of
        true -> getActiveCellsWithKsi(getOutWinCells(), getKsi(PredictedCells));
        false -> getActiveCellsWithKsi(getOutWinCells(), 0)
      end
  end.

% TODO Реализовать сохранение активации выходного слоя
getActivateCells() -> getActivateCells(get(?OutPreviousActivation)).
