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
-export([script_0/0, script_1/1, script_1_withHardCode/0, script_2/1, printGlobalData/0]).

script_0() ->
  'BrainService':initializeBrain().

script_1(Signal) ->
  'BrainService':sendExternalSignal(Signal),
  'GlobalDataService':getInPredictedCells().

script_1_withHardCode() ->
  'BrainService':sendExternalSignal([11, 12, 23, 14, 35, 36, 37, 30, 39, 40]),
  'GlobalDataService':getInPredictedCells().

script_2(List) ->
  'BrainService':sendFeedForwardSignal(List),
  'GlobalDataService':getOutActiveCells().



% Функция выводит в файлы все глобальные данные
printGlobalData() ->
  'HelpFunctions':mapWriteToFile('InLayer.tb', 'GlobalDataService':getInLayer()),
  'HelpFunctions':mapWriteToFile('OutLayer.tb', 'GlobalDataService':getOutLayer()),
  'HelpFunctions':mapWriteToFile('FeedForward.tb', 'GlobalDataService':getFeedForward()),
  'HelpFunctions':mapWriteToFile('FeedBack.tb', 'GlobalDataService':getFeedBack()),
  'HelpFunctions':mapWriteToFile('InActiveLayer.tb', 'GlobalDataService':getInActiveCells()),
  'HelpFunctions':mapWriteToFile('InPredictedCells.tb', 'GlobalDataService':getInPredictedCells()),
  'HelpFunctions':listWriteToFile('OutActiveLayer.tb', 'GlobalDataService':getOutActiveCells()).
