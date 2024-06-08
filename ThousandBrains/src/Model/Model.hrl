%%%-------------------------------------------------------------------
%%% @author Potap
%%% @copyright (C) 2024, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 16. май 2024 0:50
%%%-------------------------------------------------------------------
-author("Potap").

-record(synapse, {guid, permanenceValue, permanenceWeight :: boolean()}).

-define(InActiveCells, inActiveCells).
-define(InPredictedCells, inPredictedCells).
-define(InLayer, inLayer).

-define(OutActiveCells, outActiveCells).
-define(OutPreviousActivation, outPreviousActivation).
-define(OutLayer, outLayer).

-define(FeedForward, feedForward).
-define(FeedBack, feedBack).

-define(AllInCells, allInCells).
-define(AllOutCells, allOutCells).

-define(LocationSignal, locationSignal).

-define(NoActiveApicalDendrite, noActiveApicalDendrite).
-define(ID, id).
