-module('Main').
-author("Daniil").

%% API
-export([start/0, main/0, getBrain/4]).
-record(synapse, {permanenceValue, permanenceWeight}).
-record(synapseRow, {synapses = [] :: synapse}).
-record(dendrite, {synapseRows = [] :: synapseRow}).
-record(cell, {dendrites = [] :: dendrite}).
-record(cellRow, {cells = [] :: cell}).
-record(layer, {cellRows = [] :: cellRow}).
-record(brain, {layers = [] :: layer}).

% Строки синапсов
getSynapseRowHelper(LayerWidth, List, CurrentWidth) when CurrentWidth < LayerWidth ->
  getSynapseRowHelper(LayerWidth, lists:append(List, [#synapse{permanenceValue = 0.6,permanenceWeight = 0.99}]), CurrentWidth + 1);
getSynapseRowHelper(LayerWidth, List, CurrentWidth) when CurrentWidth == LayerWidth ->
  List.

getSynapseRow(LayerWidth) ->
  getSynapseRowHelper(LayerWidth, [], 0).

% Матрица синапсов (Дендрит)
getDendriteHelper(LayerWidth, LayerHeight, List, CurrentHeight) when CurrentHeight < LayerHeight ->
  getDendriteHelper(LayerWidth, LayerHeight, lists:append(List, [getSynapseRow(LayerWidth)]), CurrentHeight + 1);
getDendriteHelper(_LayerWidth, LayerHeight, List, CurrentHeight) when CurrentHeight == LayerHeight->
  List.

getDendrite(LayerWidth, Height) ->
  getDendriteHelper(LayerWidth, Height, [], 0).

% Клетка
getCellHelper(LayerWidth, LayerHeight, DendriteCount, List, CurrentCount) when CurrentCount < DendriteCount ->
  getCellHelper(LayerWidth, LayerHeight, DendriteCount, lists:append(List, [getDendrite(LayerWidth, LayerHeight)]), CurrentCount + 1);
getCellHelper(_LayerWidth, _LayerHeight, DendriteCount, List, CurrentCount) when CurrentCount == DendriteCount->
  List.

getCell(LayerWidth, LayerHeight, DendriteCount) ->
  getCellHelper(LayerWidth, LayerHeight, DendriteCount, [], 0).

% Строка клеток
getCellRowHelper(LayerWidth, LayerHeight, DendriteCount, List, CurrentWidth) when CurrentWidth < LayerWidth ->
  getCellRowHelper(LayerWidth, LayerHeight, DendriteCount, lists:append(List, lists:append(List, [getCell(LayerWidth, LayerHeight, DendriteCount)])), CurrentWidth + 1);
getCellRowHelper(LayerWidth, _LayerHeight, _DendriteCount, List, CurrentWidth) when CurrentWidth == LayerWidth ->
  List.

getCellRow(LayerWidth, LayerHeight, DendriteCount) ->
  getCellRowHelper(LayerWidth, LayerHeight, DendriteCount, [], 0).

% Матрица клеток (Слой)
getLayerHelper(LayerWidth, LayerHeight, DendriteCount, List, CurrentHeight) when CurrentHeight < LayerHeight ->
  getLayerHelper(LayerWidth, LayerHeight, DendriteCount, lists:append(List, [getCellRow(LayerWidth, LayerHeight, DendriteCount)]), CurrentHeight + 1);
getLayerHelper(_LayerWidth, LayerHeight, _DendriteCount, List, CurrentHeight) when CurrentHeight == LayerHeight->
  List.

getLayer(LayerWidth, Height, DendriteCount) ->
  getLayerHelper(LayerWidth, Height, DendriteCount, [], 0).

% Слои (мозг)
getBrainHelper(LayerWidth, LayerHeight, DendriteCount, LayerCount, List, CurrentLayer) when CurrentLayer < LayerCount ->
  getBrainHelper(LayerWidth, LayerHeight, DendriteCount, LayerCount, lists:append(List, [getLayer(LayerWidth, LayerHeight, DendriteCount)]), CurrentLayer + 1);
getBrainHelper(_LayerWidth, _LayerHeight, _DendriteCount, LayerCount, List, CurrentLayer) when CurrentLayer == LayerCount->
  List.

getBrain(LayerWidth, Height, DendriteCount, LayerCount) ->
  getBrainHelper(LayerWidth, Height, DendriteCount, LayerCount, [], 0).

main() ->
  _ = #brain{
    layers = getBrain(32, 12, 10, 1)
  }.


start() ->
  _ = #brain{
    layers = getBrain(10, 4, 10, 8)
  },

  _ = #layer{
    cellRows = getLayer(3, 3, 3)
  },

  _ = #cellRow{
    cells = getCellRow(3, 3, 3)
  },

  _ = #cell{
    dendrites = getCell(3, 3, 3)
  },

  _ = #dendrite{
    synapseRows = getDendrite(3, 3)
  },

  _ = #synapseRow{
    synapses = getSynapseRow(3)
  }.


