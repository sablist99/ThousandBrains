%%%-------------------------------------------------------------------
%%% @author Potap
%%% @copyright (C) 2024, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 18. май 2024 19:00
%%%-------------------------------------------------------------------
-module('HelpFunctions').
-author("Potap").

%% API
-export([mapWriteToFile/2, listWriteToFile/2, sendDataToVisualization/0, sendMap/2, sendList/2]).

-include("Model.hrl").

% Функция вывода Map в файл с заданным именем
% TODO Рассмотреть идею - сначала формировать большую строку, а затем разом писать ее в файл
mapWriteToFile(File, Map) ->
  {ok, S} = file:open(?FileDirectory ++ File, write),
  maps:foreach(
    fun(Key, Value) ->
      if
        Value#synapse.permanenceWeight == false -> none;
        true -> file:write_file(?FileDirectory ++ File, io_lib:fwrite("~p -> ~p \n", [Key, Value]), [append])
      end
    end, Map),
  file:close(S).

% Функция вывода List в файл
listWriteToFile(File, List) ->
  {ok, S} = file:open(?FileDirectory ++ File, write),
  lists:foreach(
    fun(Value) ->
      file:write_file(?FileDirectory ++ File, io_lib:fwrite("~p\n", [Value]), [append])
    end, List),
  file:close(S).




sendDataToVisualization() ->
  {ok, Socket} = gen_tcp:connect({127, 0, 0, 1}, 8888, [binary, {active, true}]),
  sendInPredictCells(Socket).

sendData(Socket, Structure) ->
  if
    is_map(Structure) -> sendMap(Socket, Structure);
    is_list(Structure) -> sendList(Socket, Structure);
    is_record(Structure, synapse) -> 'VisualisationClient':sendInformMessage(Socket, "UNDEFINED");
    is_integer(Structure) -> 'VisualisationClient':sendSingleValue(Socket, Structure);
    true -> sendSimpleData(Socket, Structure)
  end.

sendMap(Socket, Map) ->
  'VisualisationClient':sendInformMessage(Socket, "MapBegin"),
  maps:foreach(
    fun(Key, Value) ->
      'VisualisationClient':sendSingleValue(Socket, Key),
      sendData(Socket, Value)
    end, Map),
  'VisualisationClient':sendInformMessage(Socket, "MapEnd").

sendList(Socket, List) ->
  'VisualisationClient':sendInformMessage(Socket, "ListBegin"),
  lists:foreach(
    fun(Value) ->
      sendData(Socket, Value)
    end, List),
  'VisualisationClient':sendInformMessage(Socket, "ListEnd").

sendSimpleData(Socket, Synapse) ->
  case Synapse of
    {noActiveApicalDendrite, List} -> 'VisualisationClient':sendInformMessage(Socket, "false"), sendList(Socket, List);
    {ApicalDendrite, [H|T]} -> 'VisualisationClient':sendSingleValue(Socket, ApicalDendrite), sendList(Socket, [H|T]);
    _ -> 'VisualisationClient':sendInformMessage(Socket, "UNDEFINED")
  end.


sendInPredictCells(Socket) ->
  'VisualisationClient':sendInformMessage(Socket, "InPredict"),
  sendData(Socket, 'GlobalDataService':getInPredictedCells()),
  'VisualisationClient':sendInformMessage(Socket, "END").



