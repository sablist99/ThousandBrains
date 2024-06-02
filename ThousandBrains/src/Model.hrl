%%%-------------------------------------------------------------------
%%% @author Potap
%%% @copyright (C) 2024, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 16. май 2024 0:50
%%%-------------------------------------------------------------------
-author("Potap").

% TODO Разнести define по разным файлам

-record(synapse, {guid, permanenceValue, permanenceWeight :: boolean()}).

-define(N_C, 1).
-define(N_IN, 50). % По статье - 150
-define(M, 16). % По статье - 16
-define(D, 10). %
-define(N_OUT, 512). % По статье - 4096
-define(N_EXT, 240). % По статье - 2400
-define(S, 4). % По статье - 40
-define(PERMANENCE_WEIGHT_BORDER, 0.35).
-define(POTENTIAL_SYNAPSE_BORDER, 0.17).
-define(PERMANENCE_THRESHOLD, 0.3).
-define(ROUND_EPSILON, 10000).

-define(THETA_IN_B_MIN, 4). % В статье значение параметра не уточняется
-define(THETA_IN_B_MAX, 6).
-define(THETA_OUT_B, 18).
-define(THETA_OUT_C, 1).
-define(THETA_OUT_P, 3).

-define(P_IN_PLUS, 0.5).
-define(P_IN_MINUS, 0.1).
-define(P_FF_PLUS, 0.5).
-define(P_FF_MINUS, 0.1).
-define(P_OUT_PLUS, 0.5).
-define(P_OUT_MINUS, 0.1).

-define(IP_ADDRESS, {127, 0, 0, 1}).
-define(PORT, 8888).


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

-define(NoActiveApicalDendrite, noActiveApicalDendrite).
-define(ID, id).


-define(FileDirectory, "tmp/").


-define(StructureName_InLayer, "InLayer").
-define(StructureName_PredictInLayer, "PredictInLayer").
-define(StructureName_ActiveInLayer, "ActiveInLayer").
-define(StructureName_OutLayer, "OutLayer").
-define(StructureName_ActiveOutLayer, "ActiveOutLayer").
-define(StructureName_FeedForward, "FeedForward").
-define(StructureName_FeedBack, "FeedBack").
-define(StructureEnd, "END").

-define(MarkerMapBegin, "MapBegin").
-define(MarkerMapEnd, "MapEnd").
-define(MarkerListBegin, "ListBegin").
-define(MarkerListEnd, "ListEnd").
-define(MarkerDendriteBegin, "DendriteBegin").
-define(MarkerDendriteEnd, "DendriteEnd").
-define(MarkerSynapseBegin, "SynapseBegin").
-define(MarkerSynapseEnd, "SynapseEnd").
-define(MarkerFeedBegin, "FeedBegin").
-define(MarkerFeedEnd, "FeedEnd").
-define(MarkerOutColumnBegin, "OutColumnBegin").
-define(MarkerOutColumnEnd, "OutColumnEnd").
-define(MarkerUndefined, "UNDEFINED").
-define(MarkerFalse, "false").

-define(TrueString, "true").
-define(FalseString, "false").

