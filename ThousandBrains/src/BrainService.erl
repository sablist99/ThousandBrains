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

initializeBrain() ->
  'BrainInit':initializeGlobalData().

sendExternalSignal(Signal) ->
  'GlobalDataService':putInPredictedCells('PredictCells':getPredictedCellsInInputLayer(Signal)),
  ok.

sendFeedForwardSignal(Signal) ->
  'GlobalDataService':putInActiveCells('ActivateInCells':getActiveCells(Signal)),
  'GlobalDataService':putOutActiveCells('ActivateOutCells':getActivateCells()),
  ok.

getOutActiveCells() ->
  'GlobalDataService':getOutActiveCells().
