%%%-------------------------------------------------------------------
%%% @author Potap
%%% @copyright (C) 2024, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 23. май 2024 17:35
%%%-------------------------------------------------------------------
-module('BrainService').
-author("Potap").

%% API
-export([initializeBrain/0, sendExternalSignal/1, sendFeedForwardSignal/1, getOutActiveCells/0]).

-include("Model.hrl").

% Заполнение слоев синапсами, создание связей между синапсами
initializeBrain() ->
  'BrainInit':initializeGlobalData().

% Отправка сигнала местонахождения (L)
sendExternalSignal(Signal) ->
  'GlobalDataService':putInPredictedCells('PredictCells':getPredictedCellsInInputLayer(Signal)),
  ok.

% Отправка сенсорного (прямого) сигнала
%
sendFeedForwardSignal(Signal) ->
  'GlobalDataService':putInActiveCells('ActivateInCells':getActiveCells(Signal)),
  'GlobalDataService':putOutPreviousActivation('GlobalDataService':getOutActiveCells()),
  'GlobalDataService':putOutActiveCells('ActivateOutCells':getActivateCells()),
  ok.

getOutActiveCells() ->
  'GlobalDataService':getOutActiveCells().
