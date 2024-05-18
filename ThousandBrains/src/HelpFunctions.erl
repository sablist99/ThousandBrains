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
-export([mapWriteToFile/2]).

% Функция вывода Map в файл с заданным именем
mapWriteToFile(File, Map) ->
  {ok, S} = file:open(File, write),
  maps:foreach(
    fun(Key, Value) ->
      file:write_file(File, io_lib:fwrite("~p -> ~p \n", [Key, Value]), [append])
    end, Map),
  file:close(S).