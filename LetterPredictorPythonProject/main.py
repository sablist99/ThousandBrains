import numpy as np
import matplotlib.pyplot as plt

#Synapse  = (permanence_value, permanence_weight)       #tuple of permanency value and its binary representation

#Segment == dendrit
#Segment = [[(), ()],
#           [(), ()]]                                   #matrix of synapses
#          [[Synapse, Synapse, ..., Synapse],
#           [Synapse, Synapse, ..., Synapse],
#           ...
#           [Synapse, Synapse, ..., Synapse]

#Cell = [[[(), ()], [(), ()]], [[(), ()], [(), ()]]]    #array of segments (aka dendrits)
#       [Segment, Segment, ..., Segment]

#Layer = [[[[(), ()], [(), ()]], [[(), ()], [(), ()]]],
#         [[[(), ()], [(), ()]], [[(), ()], [(), ()]]]  #matrix of cells
#        [[Cell, Cell, ..., Cell],
#         [Cell, Cell, ..., Cell],
#         ...
#         [Cell, Cell, ..., Cell]

#Brain = [Layer]                                        #array of layers
#LayerState [(x,y),(x,y)] - должен

#How does inhibitory process work:
#захардкодить весь входной алфавит, лол

Alphabet = ['A', 'B', 'C', 'D', 'E', 'F', 'G', 'H',
            'I', 'J', 'K', 'L', 'M', 'N', 'O', 'P',
            'Q', 'R', 'S', 'T', 'U', 'V', 'W', 'X',
            'Y', 'Z', ' ', ',', '.', '!', '?', '-']

Labels =   [' ', 'A', 'B', 'C', 'D', 'E', 'F', 'G', 'H',
            'I', 'J', 'K', 'L', 'M', 'N', 'O', 'P',
            'Q', 'R', 'S', 'T', 'U', 'V', 'W', 'X',
            'Y', 'Z', ' ', ',', '.', '!', '?', '-']

# so in layer 0-th column is responsible for A, 1st for B etc

Brain = []

#N = 2048    # width of layer
N = 32     # width of layer
#M = 32      # height of layer
M = 12      # height of layer
#k = 40
k = 10

theta = 3 # для 40 синапсов в клетке theta = 15.
p_plus = 0.1
p_minus = 0.02
p_minus_minus = 0.005

permanence_threshold = 0.3
target_init_value = 0.12
_lambda = 5
lambda_divisor = _lambda / target_init_value
#dendrit_count = 128
dendrit_count = 20
#layer_count = 8
layer_count = 1



def initSegment(): #init one segment of a cell
    filled_segment = []
    for i in range(M):
        one_line = []
        for j in range(N):
            perm_val = round(np.random.poisson(_lambda) / lambda_divisor, 2)
            synapse = [perm_val, 1 if perm_val >= permanence_threshold else 0]
            one_line.append(synapse)
        filled_segment.append(one_line)
    return filled_segment

def initCell(): #init one cell
    filled_cell = []
    for i in range(dendrit_count):
        filled_cell.append(initSegment())
    return filled_cell

def initLayer():
    filled_layer = []
    for i in range(M):
        one_line = []
        for j in range(N):
            one_line.append(initCell())
        filled_layer.append(one_line)
    return filled_layer

def initBrain():
    filled_brain = []
    for i in range(layer_count):
        filled_brain.append(initLayer())
    return filled_brain


def drawSegmentByBinary(segment):
    i = 0
    for row in segment:
        j = 0
        i += 1
        for element in row:
            j += 1
            if element[1] == 1:
                plt.plot(i, j, marker='.', color="black")
    plt.xlim(0, M)
    plt.ylim(0, N)
    plt.show()
    return

def drawActiveStateLayer(layer):
    i = 0
    for row in range(M):
        j = 0
        i += 1
        for element in range(N):
            j += 1
            if [i - 1, j - 1] in layer:
                plt.plot(j, i, marker='.', color="black")
            else:
                plt.plot(j, i, marker='.', color="gainsboro")

    plt.xlim(0, N + 1)
    plt.ylim(0, M + 1)
    ax = plt.gca()
    ax.set_aspect('equal', adjustable='box')
    plt.show()
    return

def drawActiveStateLayerWithPrediction(layer, prediction):
    i = 0
    for row in range(M):
        j = 0
        i += 1
        for element in range(N):
            j += 1
            if [i - 1, j - 1] in prediction:
                plt.plot(j, i, marker='.', color="red")
            elif [i - 1, j - 1] in layer:
                plt.plot(j, i, marker='.', color="black")
            else:
                plt.plot(j, i, marker='.', color="gainsboro")

    plt.xlim(0, N + 1)
    plt.ylim(0, M + 1)
    ax = plt.gca()
    ax.set_aspect('equal', adjustable='box')
    plt.xticks(range(N + 1), Labels)
    plt.show()
    return

def drawSegment(segment):
    i = 0
    for row in segment:
        j = 0
        i += 1
        for element in row:
            j += 1
            plt.plot(i, j, marker='.', color=(element[0], 0, 0))
    plt.xlim(0, M)
    plt.ylim(0, N)
    plt.show()
    return


def selectWinColumns(inputSymbol):
    W = []
    for symbol in inputSymbol:
        index = Alphabet.index(symbol)
        if index not in W:
            W.append(index)
    return W

#формула (2)

def computeOneCellState(cell_coords, prediction_layer_state, W): # cell = (x,y), prev_brain_state = predict_all_cell_states(t-1), W = набор столбцов
    if cell_coords[1] in W: #we check if our cell is in column that was chose by inhibitory algorithm
        count = 0
        for current_cell in prediction_layer_state:
            if current_cell == cell_coords:
                return 1
            if current_cell[1] == cell_coords[1]:
                count += 1
        if count == 0:
            return 1
    return 0

def selectActiveCells(prev_prediction_state, w):
    #layel - full matrix
    #prev_prediction_state - разряженная матрица предсказания. На первой итерации будет пустой
    #w - список с номерами столбцов
    layer_state = []
    x = 0
    for i in range(M):
        y = 0
        for j in range(N):
            coord = [x, y]
            cell_state = computeOneCellState(coord, prev_prediction_state, w)
            if cell_state == 1:
                layer_state.append(coord)
            y += 1
        x += 1
    return layer_state

#формула (3)

def computeOneCellPrediction(coord, layer, active_layer_state):
    count = 0
    for active_cell in active_layer_state:
        for current_segment in layer[active_cell[0]][active_cell[1]]:
            if current_segment[coord[0]][coord[1]][1] == 1:
                count += 1
    if count >= theta:
        return 1
    return 0

def selectPredictedCells(layer, active_layer_state):
    # предсказанная клетка это та,на которую ссылаются (не меньше theta) клетки из матрицы активности
    predictionMatrix = []

    x = 0
    for i in range(M):
        y = 0
        for j in range(N):
            coord = [x, y]
            predict = computeOneCellPrediction(coord, layer, active_layer_state)
            if predict == 1:
                predictionMatrix.append(coord)
            y += 1
        x += 1
    return predictionMatrix

#формула (4)

# возвращает список индексов сегментов клетки, которые следует поощерить

def chooseCellsForReinforcement(coord, layer, prev_layer_state):
    count = 0
    for active_cell in prev_layer_state:
        for current_segment in layer[active_cell[0]][active_cell[1]]:
            if current_segment[coord[0]][coord[1]][1] == 1:
                count += 1
    if count >= theta:
        return 1
    return 0
'''             (cell_coords, prev_predict_state, cell, prev_layer_state): #cell = [segments], cell_coords = (x, y), prev_layer_state = [active_cells], prev_predict_state = (x, y)
    segment_list = []
    segment_index = 0
    for segment in cell:
        count = 0
        enough_synapses = 0
        for active_cell in prev_layer_state:
            if(segment[active_cell[0], active_cell[1]][1] == 1): # если клетка "выстрелила" и с ней есть синапс
                count += 1
        if count >= theta:
            enough_synapses = 1
        if enough_synapses > 0 and cell_coords in prev_predict_state:
            segment_list.append(segment_index)
        segment_index += 1
    return segment_list
'''
def searchCorrectlyPredictedCells(active_layer, layer, prev_active_layer, prev_prediction_layer):
    reinforcement_cells = []
    for new_active_cell in active_layer:
        if new_active_cell in prev_prediction_layer:
            choosen = chooseCellsForReinforcement(new_active_cell, layer, prev_active_layer)
            if choosen == 1:
                reinforcement_cells.append(new_active_cell)
    return reinforcement_cells

#формула (5)

#возвращает координаты клетки, которая будет соотноситься с текущим паттерном и номер сегмента этой клетки, который был ближе всего вы "выстрелу"

def selectCellAndSegmentForPattern(layer, w, prev_active_layer_state):
    stolb = w[0]
    max_cell_coords = [-1, -1]
    target_coords = []
    max_count = -1
    max_segment_index = -1
    max_value = -1.

    for prev_active_cell in prev_active_layer_state:
        segment_index = 0
        for current_segment in layer[prev_active_cell[0]][prev_active_cell[1]]:
            for i in range(M):
                if current_segment[i][stolb][0] > max_value:
                    max_value = current_segment[i][stolb][0]
                    max_segment_index = segment_index
                    max_cell_coords = prev_active_cell
                    target_coords = [i, stolb]
            segment_index += 1
    for segment in layer[max_cell_coords[0]][max_cell_coords[1]]:
        segment[target_coords[0]][target_coords[1]][0] = permanence_threshold
        segment[target_coords[0]][target_coords[1]][1] = 1
    return


#формула (6) и (7)

def rewardSynapses(layer, prev_active_layer_state, correctly_predirected_cells):
    # Перебираем все клетки, которые были предсказаны и стали активными
    for current_correctly_cell in correctly_predirected_cells:
        # Для каждой из них ищем те клетки, которые их предсказали
        for current_active_cell in prev_active_layer_state:
            # Затем ищем сегменты, которые сделали предсказание
            for current_segment in layer[current_active_cell[0]][current_active_cell[1]]:
                if current_segment[current_correctly_cell[0]][current_correctly_cell[1]][1] == 1:
                    # Поощеряем все синапсы этого сегмента
                    current_segment[current_correctly_cell[0]][current_correctly_cell[1]][0] += p_plus


if __name__ == '__main__':
    input_raw_data_file = open("input_raw_data.dat", "r")
    input_strings = input_raw_data_file.read().splitlines()

    input_test_strings_file = open("input_test_strings.dat", "r")
    input_test_strings = input_test_strings_file.read().splitlines()


    brain = initBrain()

    learning_iterations = 20


    predicted_cells = []
    for i in range(layer_count):
        predicted_cells.append([])

    # Обучение
    for current_string in input_strings:
        print("Изучаем строку - ", current_string)
        for i in range(learning_iterations):
            active_cells = []
            predicted_cells = []
            current_layer = 0
            isFirstIter = 1
            for symbol in current_string:
                #В win_columns будут лежать номера столбцов, соответствубщие строке
                win_columns = selectWinColumns(symbol)

                prev_active_cells = active_cells
                prev_predicted_cells = predicted_cells

                active_cells = selectActiveCells(prev_predicted_cells, win_columns)
                predicted_cells = selectPredictedCells(brain[current_layer], active_cells)

                #drawActiveStateLayerWithPrediction(active_cells, predicted_cells)

                correctly_predicted_cells = searchCorrectlyPredictedCells(active_cells, brain[current_layer], prev_active_cells, prev_predicted_cells)

                if isFirstIter == 1:
                    isFirstIter = 0
                    continue
                if correctly_predicted_cells == []:
                    # Если никто не предсказывал, а столб выстрелил
                    selectCellAndSegmentForPattern(brain[current_layer], win_columns, prev_active_cells)
                else:
                    rewardSynapses(brain[current_layer], prev_active_cells, correctly_predicted_cells)


    for test_string in input_test_strings:
        active_cells = []
        predicted_cells = []

        # Тестирование
        for symbol in test_string:
            win_columns = selectWinColumns(symbol)

            prev_active_cells = active_cells
            prev_predicted_cells = predicted_cells

            active_cells = selectActiveCells(prev_predicted_cells, win_columns)
            predicted_cells = selectPredictedCells(brain[current_layer], active_cells)

            drawActiveStateLayerWithPrediction(active_cells, predicted_cells)

            correctly_predicted_cells = searchCorrectlyPredictedCells(active_cells, brain[current_layer], prev_active_cells, prev_predicted_cells)

