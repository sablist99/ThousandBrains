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
-export([handleCommand/1]).

-include("Model/Commands.hrl").
-include("Model/Model.hrl").
-include("Model/SenderMode.hrl").
-include("Model/ServerSettings.hrl").

% TODO на каждую структуру создать отдельную функцию
% TODO Написать проверку на существование сокета

% Обработка сообщений, полученных от модуля визуализации
handleCommand(Command) ->
  % Если нам прислали число, то определяем для чего оно пришло. Иначе это буквенная команда
  case 'HelpFunctions':getIntegerFromString(Command) of
    false ->
      case Command of
        ?NeedBrainInitialize ->
          'BrainService':initializeBrain(),
          sendInLayer(),
          sendFeedBack(),
          sendFeedForward(),
          sendOutLayer();

        ?NeedBrainPrint ->
          'HelpFunctions':printGlobalData();

        ?LocationSignalBegin ->
          % Устанавливаем режим для распознавания сигнала местоположения и обнуляем старый сигнал
          'GlobalDataService':putSenderMode(?LocationSignalMode),
          'GlobalDataService':putLocationSignal([]);
        ?LocationSignalEnd ->
          % Сбрасываем режим, отправляем сигнал мозгу и данные в модуль визуализации
          'GlobalDataService':putSenderMode(?NoneSenderMode),
          'BrainService':sendExternalSignal('GlobalDataService':getLocationSignal()),
          sendPredictInLayer();

        ?SensorySignalBegin ->
          % Устанавливаем режим для распознавания сенсорного сигнала и обнуляем старый сигнал
          'GlobalDataService':putSenderMode(?SensorySignalMode),
          'GlobalDataService':putSensorySignal([]);
        ?SensorySignalEnd ->
          % Сбрасываем режим, отправляем сигнал мозгу и данные в модуль визуализации
          'GlobalDataService':putSenderMode(?NoneSenderMode),
          'BrainService':sendFeedForwardSignal('GlobalDataService':getSensorySignal()),
          sendActiveInLayer(),
          sendActiveOutLayer();

        _ ->
          io:format("Поступила неизвестная команда: ~p", [Command])

      end;
    % У нас число
    _ ->
      case 'GlobalDataService':getSenderMode() of
        % Заполняем сигнал местоположения
        ?LocationSignalMode ->
          'GlobalDataService':appendLocationSignal('HelpFunctions':getIntegerFromString(Command));
        % Заполняем сенсорный сигнал
        ?SensorySignalMode ->
          'GlobalDataService':appendSensorySignal('HelpFunctions':getIntegerFromString(Command));
        _ -> error
      end
  end.



sendInLayer() ->
  'VisualisationClient':sendInformMessage(get(socket), ?StructureName_InLayer),
  sendData(get(socket), 'GlobalDataService':getInLayer()),
  'VisualisationClient':sendInformMessage(get(socket), ?StructureEnd).

sendOutLayer() ->
  'VisualisationClient':sendInformMessage(get(socket), ?StructureName_OutLayer),
  sendData(get(socket), 'GlobalDataService':getOutLayer()),
  'VisualisationClient':sendInformMessage(get(socket), ?StructureEnd).

sendPredictInLayer() ->
  'VisualisationClient':sendInformMessage(get(socket), ?StructureName_PredictInLayer),
  sendData(get(socket), 'GlobalDataService':getInPredictedCells()),
  'VisualisationClient':sendInformMessage(get(socket), ?StructureEnd).

sendActiveInLayer() ->
  'VisualisationClient':sendInformMessage(get(socket), ?StructureName_ActiveInLayer),
  sendData(get(socket), 'GlobalDataService':getInActiveCells()),
  'VisualisationClient':sendInformMessage(get(socket), ?StructureEnd).

sendActiveOutLayer() ->
'VisualisationClient':sendInformMessage(get(socket), ?StructureName_ActiveOutLayer),
sendData(get(socket), 'GlobalDataService':getOutActiveCells()),
'VisualisationClient':sendInformMessage(get(socket), ?StructureEnd).

sendFeedForward() ->
  'VisualisationClient':sendInformMessage(get(socket), ?StructureName_FeedForward),
  sendData(get(socket), 'GlobalDataService':getFeedForward()),
  'VisualisationClient':sendInformMessage(get(socket), ?StructureEnd).

sendFeedBack() ->
  'VisualisationClient':sendInformMessage(get(socket), ?StructureName_FeedBack),
  sendData(get(socket), 'GlobalDataService':getFeedBack()),
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
