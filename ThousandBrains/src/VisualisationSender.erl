%%%-------------------------------------------------------------------
%%% @author Potap
%%% @copyright (C) 2024, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 02. июнь 2024 13:29
%%%-------------------------------------------------------------------
-module('VisualisationSender').
-author("Potap").

%% API
-export([sendDataToVisualization/0, initializeSocket/0, sendInLayer/0]).

-include("Model.hrl").

% TODO на каждую структуру создать отдельную функцию. Определиться с сохранением сокета

sendDataToVisualization() ->
  {ok, Socket} = gen_tcp:connect(?IP_ADDRESS, ?PORT, [binary, {active, true}]),
  'VisualisationClient':sendInformMessage(Socket, ?StructureName_InLayer),
  sendData(Socket, 'GlobalDataService':getInLayer()),
  'VisualisationClient':sendInformMessage(Socket, ?StructureEnd),
  'VisualisationClient':sendInformMessage(Socket, ?StructureName_PredictInLayer),
  sendData(Socket, 'GlobalDataService':getInPredictedCells()),
  'VisualisationClient':sendInformMessage(Socket, ?StructureEnd),
  'VisualisationClient':sendInformMessage(Socket, ?StructureName_ActiveInLayer),
  sendData(Socket, 'GlobalDataService':getInActiveCells()),
  'VisualisationClient':sendInformMessage(Socket, ?StructureEnd),
  'VisualisationClient':sendInformMessage(Socket, ?StructureName_OutLayer),
  sendData(Socket, 'GlobalDataService':getOutLayer()),
  'VisualisationClient':sendInformMessage(Socket, ?StructureEnd),
  'VisualisationClient':sendInformMessage(Socket, ?StructureName_ActiveOutLayer),
  sendData(Socket, 'GlobalDataService':getOutActiveCells()),
  'VisualisationClient':sendInformMessage(Socket, ?StructureEnd),
  'VisualisationClient':sendInformMessage(Socket, ?StructureName_FeedForward),
  sendData(Socket, 'GlobalDataService':getFeedForward()),
  'VisualisationClient':sendInformMessage(Socket, ?StructureEnd),
  'VisualisationClient':sendInformMessage(Socket, ?StructureName_FeedBack),
  sendData(Socket, 'GlobalDataService':getFeedBack()),
  'VisualisationClient':sendInformMessage(Socket, ?StructureEnd).

% TODO Написать прослойку для доступа к сокету + проверку на его существование
initializeSocket() ->
  {ok, Socket} = gen_tcp:connect(?IP_ADDRESS, ?PORT, [binary, {active, true}]),
  put(socket, Socket).

sendInLayer() ->
  'VisualisationClient':sendInformMessage(get(socket), ?StructureName_InLayer),
  sendData(get(socket), 'GlobalDataService':getInLayer()),
  'VisualisationClient':sendInformMessage(get(socket), ?StructureEnd).

sendData(Socket, Structure) ->
  if
    is_map(Structure) -> sendMap(Socket, Structure);
    is_list(Structure) -> sendList(Socket, Structure);
    is_record(Structure, synapse) -> sendSynapse(Socket, Structure);
    is_integer(Structure) -> 'VisualisationClient':sendIntegerValue(Socket, Structure);
    true -> sendSimpleData(Socket, Structure)
  end.



sendMap(Socket, Map) ->
  'VisualisationClient':sendInformMessage(Socket, ?MarkerMapBegin),
  maps:foreach(
    fun(Key, Value) ->
      sendData(Socket, Key),
      sendData(Socket, Value)
    end, Map),
  'VisualisationClient':sendInformMessage(Socket, ?MarkerMapEnd).



sendList(Socket, List) ->
  'VisualisationClient':sendInformMessage(Socket, ?MarkerListBegin),
  lists:foreach(
    fun(Value) ->
      sendData(Socket, Value)
    end, List),
  'VisualisationClient':sendInformMessage(Socket, ?MarkerListEnd).



sendSynapse(Socket, Synapse) ->
  'VisualisationClient':sendInformMessage(Socket, ?MarkerSynapseBegin),
  'VisualisationClient':sendIntegerValue(Socket, Synapse#synapse.guid),
  'VisualisationClient':sendFloatValue(Socket, Synapse#synapse.permanenceValue),
  'VisualisationClient':sendBoolValue(Socket, Synapse#synapse.permanenceWeight),
  'VisualisationClient':sendInformMessage(Socket, ?MarkerSynapseEnd).



sendSimpleData(Socket, SimpleData) ->
  case SimpleData of
    {noActiveApicalDendrite, List} ->
      'VisualisationClient':sendInformMessage(Socket, ?MarkerDendriteBegin),
      'VisualisationClient':sendInformMessage(Socket, ?MarkerFalse),
      sendList(Socket, List),
      'VisualisationClient':sendInformMessage(Socket, ?MarkerDendriteEnd);

    {ApicalDendrite, [H | T]} ->
      'VisualisationClient':sendInformMessage(Socket, ?MarkerDendriteBegin),
      'VisualisationClient':sendIntegerValue(Socket, ApicalDendrite),
      sendList(Socket, [H | T]),
      'VisualisationClient':sendInformMessage(Socket, ?MarkerDendriteEnd);

    {{Key1, Key2}, {Key3, Key4}} ->
      'VisualisationClient':sendInformMessage(Socket, ?MarkerFeedBegin),
      'VisualisationClient':sendIntegerValue(Socket, Key1),
      'VisualisationClient':sendIntegerValue(Socket, Key2),
      'VisualisationClient':sendIntegerValue(Socket, Key3),
      'VisualisationClient':sendIntegerValue(Socket, Key4),
      'VisualisationClient':sendInformMessage(Socket, ?MarkerFeedEnd);

    {CellGuid, Map} ->
      'VisualisationClient':sendInformMessage(Socket, ?MarkerOutColumnBegin),
      'VisualisationClient':sendIntegerValue(Socket, CellGuid),
      sendMap(Socket, Map),
      'VisualisationClient':sendInformMessage(Socket, ?MarkerOutColumnEnd);

    _ -> 'VisualisationClient':sendInformMessage(Socket, ?MarkerUndefined)
  end.
