%%%-------------------------------------------------------------------
%%% @author Potap
%%% @copyright (C) 2024, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 28. май 2024 22:05
%%%-------------------------------------------------------------------
-module('VisualisationClient').
-author("Potap").

%% API
-export([sendInformMessage/2, sendIntegerValue/2, sendFloatValue/2, sendBoolValue/2, initializeSocket/0, runReceiver/0]).

-include("Model/ServerSettings.hrl").
-include("Model/Commands.hrl").

% Команды отправки разных типов данных
sendInformMessage(Socket, Message) ->
  gen_tcp:send(Socket, Message ++ "\n").

sendIntegerValue(Socket, Value) ->
  gen_tcp:send(Socket, integer_to_list(Value) ++ "\n").

sendFloatValue(Socket, Value) ->
  gen_tcp:send(Socket, float_to_list(Value, [{decimals, 4}]) ++ "\n").

sendBoolValue(Socket, Value) ->
  case Value of
    false -> gen_tcp:send(Socket, "false" ++ "\n");
    true -> gen_tcp:send(Socket, "true" ++ "\n")
  end.



% Создание сокета {ok, Socket} = gen_tcp:connect({127, 0, 0, 1}, 8888, [binary, {active, false}]).
initializeSocket() ->
  {ok, Socket} = gen_tcp:connect(?IP_ADDRESS, ?PORT, [binary, {active, false}, {packet, line}]),
  put(socket, Socket).



% Бесконечный прием сообщений
runReceiver() ->
  case gen_tcp:recv(get(socket), 0) of
    {ok, Command} ->
      'VisualisationSender':handleCommand(lists:delete($\n, lists:delete($\r, binary_to_list(Command))));
    {error, Reason} ->
      Reason
  end,
  runReceiver().
