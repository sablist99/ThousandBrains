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
-export([
  putInLayer/1, getInLayer/0, putOutLayer/1, getOutLayer/0, putAllInCells/1, getAllInCells/0,
  putAllOutCells/1, getAllOutCells/0, putFeedForward/1, getFeedForward/0, putFeedBack/1, getFeedBack/0,
  putInPredictedCells/1, getInPredictedCells/0, putOutActiveCells/1, getOutActiveCells/0,
  putInActiveCells/1, getInActiveCells/0, putOutPreviousActivation/1, getOutPreviousActivation/0,
  putSenderMode/1, getSenderMode/0,
  putLocationSignal/1, getLocationSignal/0, appendLocationSignal/1,
  putSensorySignal/1, getSensorySignal/0, appendSensorySignal/1]).

-include("Model/Model.hrl").
-include("Model/SenderMode.hrl").

% TODO Добавить общую проверку на undefined

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



putOutPreviousActivation(OutActiveCells) ->
  put(?OutPreviousActivation, OutActiveCells).

getOutPreviousActivation()->
  get(?OutPreviousActivation).



putSenderMode(Mode) ->
  put(?SenderMode, Mode).

getSenderMode()->
  get(?SenderMode).



putLocationSignal(Signal) ->
  put(?LocationSignal, Signal).

getLocationSignal() ->
  get(?LocationSignal).

appendLocationSignal(Signal) ->
  put(?LocationSignal, lists:append(getLocationSignal(), [Signal])).



putSensorySignal(Signal) ->
  put(?SensorySignal, Signal).

getSensorySignal() ->
  get(?SensorySignal).

appendSensorySignal(Signal) ->
  put(?SensorySignal, lists:append(getSensorySignal(), [Signal])).