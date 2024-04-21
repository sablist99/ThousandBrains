%%%-------------------------------------------------------------------
%%% @author Daniil
%%% @copyright (C) 2024, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 10. апр. 2024 23:39
%%%-------------------------------------------------------------------
-author("Daniil").

-define(PERMANENCE_WEIGHT_BORDER, 0.35).
-define(THETA, 3).
-define(PERMANENCE_THRESHOLD, 0.3).
-define(P_PLUS, 0.1).


-record(synapse, {permanenceValue, permanenceWeight :: boolean()}).
-record(synapseRow, {synapses = [] :: synapse}).
-record(dendrite, {synapseRows = [] :: synapseRow}).
-record(cell, {dendrites = [] :: dendrite}).
-record(cellRow, {cells = [] :: cell}).
-record(layer, {cellRows = [] :: cellRow}).
-record(brain, {layers = [] :: layer}).



