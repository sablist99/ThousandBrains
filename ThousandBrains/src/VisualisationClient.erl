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
-export([sendInformMessage/2, sendSingleValue/2, sendFloatValue/2, sendBoolValue/2]).
-define(Port, 8888).

% Создание сокета {ok, Socket} = gen_tcp:connect({127, 0, 0, 1}, 8888, [binary, {active,true}]).

sendInformMessage(Socket, Message) ->
  gen_tcp:send(Socket, Message ++ "\n").

sendSingleValue(Socket, Value) ->
  gen_tcp:send(Socket, integer_to_list(Value) ++ "\n").

sendFloatValue(Socket, Value) ->
  gen_tcp:send(Socket, float_to_list(Value, [{decimals, 4}]) ++ "\n").

sendBoolValue(Socket, Value) ->
  case Value of
    false -> gen_tcp:send(Socket, "false" ++ "\n");
    true -> gen_tcp:send(Socket, "true" ++ "\n")
  end.
