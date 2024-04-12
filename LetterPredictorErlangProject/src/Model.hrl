%%%-------------------------------------------------------------------
%%% @author Daniil
%%% @copyright (C) 2024, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 10. апр. 2024 23:39
%%%-------------------------------------------------------------------
-author("Daniil").

-record(synapse, {permanenceValue, permanenceWeight :: boolean()}).
-record(synapseRow, {synapses = [] :: synapse}).
-record(dendrite, {synapseRows = [] :: synapseRow}).
-record(cell, {dendrites = [] :: dendrite}).
-record(cellRow, {cells = [] :: cell}).
-record(layer, {cellRows = [] :: cellRow}).
-record(brain, {layers = [] :: layer}).


