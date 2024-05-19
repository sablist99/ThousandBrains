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
-export([main/0, sendExternalSignal/1]).

-include("Model.hrl").

main() ->
  'BrainInit':initializeGlobalData().

sendExternalSignal(Signal) ->
  'PredictCellsInInputLayer':getPredictedCells(Signal).