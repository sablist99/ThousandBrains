%%%-------------------------------------------------------------------
%%% @author Potap
%%% @copyright (C) 2024, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 20. май 2024 0:43
%%%-------------------------------------------------------------------
-module('ActivateCellsInOutputLayer').
-author("Potap").

%% API
-export([getOutWinCells/0]).

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
getSynapsesListBetweenOutCellAndActiveCellsFromOneMiniColumn([], _OutCellGuid, Synapses) ->
  Synapses;
getSynapsesListBetweenOutCellAndActiveCellsFromOneMiniColumn([ActiveCellGuid | ActiveCells], OutCellGuid, Synapses) ->
  case 'CommonFunctions':existSynapseInFeedForwardMap(ActiveCellGuid, OutCellGuid, get(feedForward)) of
    {1, Value} ->
      getSynapsesListBetweenOutCellAndActiveCellsFromOneMiniColumn(ActiveCells, OutCellGuid, lists:append(Synapses, [Value]));
    0 ->
      getSynapsesListBetweenOutCellAndActiveCellsFromOneMiniColumn(ActiveCells, OutCellGuid, Synapses)
  end.



% В этой функции происходит формирование списка существующих синапсов между выходной клеткой и активными клетками входного слоя
% Вместе с этим проверяется, а достаточно ли этих синапсов для активации
getSynapsesListBetweenOutCellAndActiveCells(ActiveCellsIterator, OutCellGuid, Synapses) ->
  case maps:next(ActiveCellsIterator) of
    % Проверяем - а достаточно ли синапсов у текущей выходной клетки
    none ->
      case iolist_size(Synapses) > ?THETA_OUT_P of
        true -> Synapses;
        false -> []
      end;
    {_Range, ActiveCellsList, NewCellIterator} ->
      getSynapsesListBetweenOutCellAndActiveCells(NewCellIterator, OutCellGuid, lists:append(Synapses, getSynapsesListBetweenOutCellAndActiveCellsFromOneMiniColumn(ActiveCellsList, OutCellGuid, Synapses)))
  end.



% В этой функции формируется список выигрышных клеток выходного слоя (тех клеток, у которых достаточно синапсов с активными кдетками входного слоя)
getOutWinCellsHelper(OutCellsIterator, OutWin) ->
  case maps:next(OutCellsIterator) of
    none -> OutWin;
    {Range, {OutCellGuid, _}, NewCellIterator} ->
      case getSynapsesListBetweenOutCellAndActiveCells(maps:iterator(get(inActiveCells)), OutCellGuid, []) of
        % Если связь не прочная или ее вообще нет
        [] -> getOutWinCellsHelper(NewCellIterator, OutWin);
        % Если есть достаточно синапсов, то объявляем клетку выигрышной
        Synapses -> getOutWinCellsHelper(NewCellIterator, maps:put(Range, Synapses, OutWin))
      end
  end.


% Получение выигрышных клеток выходного слоя
getOutWinCells() ->
  getOutWinCellsHelper(maps:iterator(get(outLayer)), #{}).