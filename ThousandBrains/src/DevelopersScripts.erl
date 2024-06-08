%%%-------------------------------------------------------------------
%%% @author Potap
%%% @copyright (C) 2024, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 23. май 2024 17:47
%%%-------------------------------------------------------------------
-module('DevelopersScripts').
-author("Potap").

%% API
-export([script_0/0, script_1/1, script_1_withHardCode/0, script_2/1, printToFileThousandSynapses/0, script_3/0]).

% Инициализация синапсов во всех структурах
script_0() ->
  'BrainService':initializeBrain().

% Отправка сигнала местоположения, получение предсказанных клеток
script_1(Signal) ->
  'BrainService':sendExternalSignal(Signal),
  'GlobalDataService':getInPredictedCells().

% Отправка сигнала местоположения, получение предсказанных клеток
script_1_withHardCode() ->
  'BrainService':sendExternalSignal([11, 12, 23, 14, 35, 36, 37, 30, 39, 40]),
  'GlobalDataService':getInPredictedCells().

% Отправка прямого сигнала, получение активных клеток выходного слоя
script_2(List) ->
  'BrainService':sendFeedForwardSignal(List),
  'GlobalDataService':getOutActiveCells().

% Вывод министолбцов входного слоя с активными апикальными дендритами
script_3() ->
  'CommonFunctions':getMiniColumnsWithActiveApicalDendrite().




% Вывод на экран 1000 синапсов
printToFileThousandSynapses(Count, Synapses) when Count == 1000 ->
  Synapses;
printToFileThousandSynapses(Count, Synapses) ->
  printToFileThousandSynapses(Count + 1, lists:append(Synapses, ['BrainInit':getSynapse()])).

printToFileThousandSynapses() ->
  'HelpFunctions':listWriteToFile('ThousandSynapses.tb', printToFileThousandSynapses(0, [])).
