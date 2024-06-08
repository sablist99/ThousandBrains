-module('BrainLearn').

-include("Prediction.erl").

%% API
-export([learn/2]).

-define(LEARNING_ITERATIONS, 20).

learnSymbol(Brain, Char, ActiveCells, PredictedCells, CurrentLayer, IsFirstFilter)


learnString(Brain, [], ActiveCells, PredictedCells, CurrentLayer, IsFirstFilter) ->
  Brain;
learnString(Brain, [Char | String_T], ActiveCells, PredictedCells, CurrentLayer, IsFirstFilter) ->
  learnString(
    learnSymbol(
      Brain, Char,
    ),
    String_T,

  ).


% Изучение конкретной строки
learnStringByIterations(Brain, _String, CurrentIteration) when CurrentIteration == ?LEARNING_ITERATIONS ->
  Brain;
learnStringByIterations(Brain, String, CurrentIteration) ->
  learnStringByIterations(
    learnString(Brain, String, [], [], 0, 1),
    String, CurrentIteration + 1
  ).


% Изучение массива строк
learn(Brain, []) ->
  Brain;
learn(Brain, [CurrentString | OtherString]) ->
  learn(OtherString, learnStringByIterations(Brain, CurrentString, 0)).

