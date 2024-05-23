%%%-------------------------------------------------------------------
%%% @author Potap
%%% @copyright (C) 2024, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 23. май 2024 17:50
%%%-------------------------------------------------------------------
-module('GlobalDataService').
-author("Potap").

%% API
-export([putInLayer/1, getInLayer/0, putOutLayer/1, getOutLayer/0, putAllInCells/1, getAllInCells/0,
  putAllOutCells/1, getAllOutCells/0, putFeedForward/1, getFeedForward/0, putFeedBack/1, getFeedBack/0,
  putInPredictedCells/1, getInPredictedCells/0, putOutActiveCells/1, getOutActiveCells/0, putInActiveCells/1, getInActiveCells/0]).

-include("Model.hrl").



putInLayer(Layer) ->
  put(?InLayer, Layer).

getInLayer() ->
  get(?InLayer).



putOutLayer(Layer) ->
  put(?OutLayer, Layer).

getOutLayer() ->
  get(?OutLayer).



putAllInCells(Cells) ->
  put(?AllInCells, Cells).

getAllInCells()->
  get(?AllInCells).



putAllOutCells(Cells) ->
  put(?AllOutCells, Cells).

getAllOutCells()->
  get(?AllOutCells).



putFeedForward(Synapses) ->
  put(?FeedForward, Synapses).

getFeedForward()->
  get(?FeedForward).



putFeedBack(Synapses) ->
  put(?FeedBack, Synapses).

getFeedBack()->
  get(?FeedBack).



putInPredictedCells(Cells) ->
  put(?InPredictedCells, Cells).

getInPredictedCells()->
  get(?InPredictedCells).



putInActiveCells(Cells) ->
  put(?InActiveCells, Cells).

getInActiveCells()->
  get(?InActiveCells).



putOutActiveCells(Cells) ->
  put(?OutActiveCells, Cells).

getOutActiveCells()->
  get(?OutActiveCells).
