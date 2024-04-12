-module('Main').

-include("Model.hrl").
%% API
-export([main/0]).

main() ->
  _ = #brain{
    layers = 'BrainInit':getBrain(16, 6, 5, 1)
  }.
