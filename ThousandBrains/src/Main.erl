%%%-------------------------------------------------------------------
%%% @author Potap
%%% @copyright (C) 2024, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 14. май 2024 2:00
%%%-------------------------------------------------------------------
-module('Main').
-author("Potap").

%% API
-export([main/0, sendExternalSignal/1, sendFeedForwardSignal/1, getWinOut/0]).

-include("Model.hrl").

main() ->
  'BrainInit':initializeGlobalData().

% TODO Сделать прослойку для доступа к глобальным данным
sendExternalSignal(Signal) ->
  put(?InPredictedCells, 'PredictCells':getPredictedCellsInInputLayer(Signal)), ok.

sendFeedForwardSignal(Signal) ->
  put(?InActiveCells, 'ActivateCellsInInputLayer':getActiveCells(Signal)), ok.

getWinOut() ->
  put(?OutWin, 'ActivateCellsInOutputLayer':getOutWinCells()), ok.

