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
-export([mapWriteToFile/2, listWriteToFile/2]).

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