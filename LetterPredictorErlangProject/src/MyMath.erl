-module('MyMath').

%% API
-export([roundRandomNumber/2, getPoissonValue/0]).

factorial(0) -> 1;
factorial(N) when N > 0 -> N * factorial(N - 1).

poisson(Lambda, K) ->
  (math:pow(Lambda, K) * math:exp(-Lambda)) / factorial(K).

% Четвертый аргумент - признак направления. 0 - умножаем на 10, 1 - делим на 10
% Условие возврата целевого числа
roundRandomNumberHelper(Number, Eps, Counter, 1) when Counter == Eps ->
  Number;
% Условие смены направления
roundRandomNumberHelper(Number, _Eps, 0, 0) -> io:write(round(Number)),
  roundRandomNumberHelper(round(Number), _Eps, 0, 1);
% Условие продолжения увеличения числа
roundRandomNumberHelper(Number, Eps, Counter, 0) ->
  roundRandomNumberHelper(Number * 10, Eps, Counter - 1, 0);
% Условие продолжения уменьшения числа
roundRandomNumberHelper(Number, Eps, Counter, 1) ->
  roundRandomNumberHelper(Number / 10, Eps, Counter + 1, 1).

roundRandomNumber(Number, Eps) -> roundRandomNumberHelper(Number, Eps, Eps, 0).


% Коэффициенты подобраны опытным путем
getPoissonValue() -> poisson(rand:uniform() * 5, 1).