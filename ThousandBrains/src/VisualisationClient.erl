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
-export([sendInformMessage/2, sendSingleValue/2]).
-define(Port, 8888).

% Создание сокета {ok, Socket} = gen_tcp:connect({127, 0, 0, 1}, 8888, [binary, {active,true}]).

sendInformMessage(Socket, Message) ->
  gen_tcp:send(Socket, Message ++ "\n").

sendSingleValue(Socket, Value) ->
  gen_tcp:send(Socket, integer_to_list(Value) ++ "\n").
