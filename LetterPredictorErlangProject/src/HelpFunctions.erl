-module('HelpFunctions').

-include("Model.hrl").

%% API
-export([getStat/1, printMap/1, print100PoissonValue/0]).

-define(STEP, 1).

% Инициализация карты со стастистикой
getEmptyStatMapHelper(Map, CurrentValue, TargetValue, _Step) when CurrentValue > TargetValue ->
  Map;
getEmptyStatMapHelper(Map, CurrentValue, TargetValue, Step) ->
  getEmptyStatMapHelper(Map#{CurrentValue => 0}, CurrentValue + Step, TargetValue, Step).

getEmptyStatMap() ->
  getEmptyStatMapHelper(#{}, 0, 100, ?STEP).


% Обновление статистики
incrementStatMap(Map, TargetPart) ->
  Map#{TargetPart := maps:get(TargetPart, Map) + 1}.


% Определение диапазона, к которому принадлежит значение
generateValueForStat(Map) ->
  incrementStatMap(Map, (round('MyMath':getPoissonValue() * 100) div ?STEP) * ?STEP).


% Получение карты со статистикой распределения значений
getStatHelper(Map, CurrentTest, TestCount) when CurrentTest == TestCount ->
  Map;
getStatHelper(Map, CurrentTest, TestCount) ->
  getStatHelper(generateValueForStat(Map), CurrentTest + 1, TestCount).

getStat(TestCount) -> getStatHelper(getEmptyStatMap(), 0, TestCount).


% Печать статистики распределения значений
printMapHelper(_Map, CurrentKey, TestCount, _Step) when CurrentKey > TestCount ->
  exit;
printMapHelper(Map, CurrentKey, TestCount, Step) ->
  io:fwrite("("),
  io:write(CurrentKey),
  io:fwrite(";"),
  io:write(maps:get(CurrentKey, Map)),
  io:fwrite(") ~n"),
  printMapHelper(Map, CurrentKey + Step, TestCount, Step).

printMap(TestCount) ->
  printMapHelper(getStat(TestCount), 0, 100, ?STEP).


% Вывод сотни значений полученных распределением пуассона
print100PoissonValue(100) -> exit;
print100PoissonValue(N) ->
  io:fwrite("("),
  io:write('MyMath':getPoissonValue()),
  io:fwrite(";0) ~n"),
  print100PoissonValue(N + 1).

print100PoissonValue() -> print100PoissonValue(0).

