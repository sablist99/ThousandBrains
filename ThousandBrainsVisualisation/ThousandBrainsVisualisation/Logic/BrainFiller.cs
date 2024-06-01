using System.Globalization;
using ThousandBrainsVisualisation.Model;
using ThousandBrainsVisualisation.ViewModel;

namespace ThousandBrainsVisualisation.BrainFiller
{
    public partial class BrainFiller(MainWindowViewModel mainWindowViewModel)
    {
        /*
         * Сделать универсальный механизм по заполнению любой структуры данных не получится.
         * Потому что у эрланга динамическая типизация, мы заранее не знаем, что лежит в данных и какая вложенность.
         * При заполнении на шарпах нам это нужно знать.
         * Поэтому будем отслеживать уровень вложенности и в зависимости от заполняемой структуры данных (InLayer, PredictCells и т.п.), будем 
         * класть данные в определенные словари
         */

        private BrainModel Brain = new();
        private MainWindowViewModel MainWindowViewModel = mainWindowViewModel;
        private BrainFillerMode BrainFillerMode = BrainFillerMode.Wait;
        private DataStructureMode DataMode = DataStructureMode.None;
        private Stack<DataStructureMode> DataStructureStack = new();

        private int Map_CurrentLevel = 0;

        // Структура InLayer
        private int MapKey_InLayer_Level_1;
        private int MapKey_InLayer_Level_2;
        private int MapKey_InLayer_Level_3;
        private int MapKey_InLayer_Level_4;
        private Dictionary<int, Dictionary<int, Dictionary<int, Dictionary<int, Synapse>>>> Map_InLayer_Level_1; // Колонки
        private Dictionary<int, Dictionary<int, Dictionary<int, Synapse>>> Map_InLayer_Level_2; // Клетки в колонке
        private Dictionary<int, Dictionary<int, Synapse>> Map_InLayer_Level_3; // Дендриты
        private Dictionary<int, Synapse> Map_InLayer_Level_4; // Синапсы
        private Synapse Synapse;

        // Структура PredictInLayer
        private int MapKey_PredictInLayer_Level_1;
        private int MapKey_PredictInLayer_Level_2;
        private Dictionary<int, Dictionary<int, Dendrites>> Map_PredictInLayer_Level_1;
        private Dictionary<int, Dendrites> Map_PredictInLayer_Level_2;
        private List<int> ActiveLateralDendrites;
        private Dendrites ActiveDendrites;

        public void SendData(string? text)
        {
            // Отладочная строка для отображения полученных данных
            //MainWindowViewModel.SynchronizedText += text + "\n";
            switch (BrainFillerMode)
            {
                // Проверяем какую структуру предметной области заполняем
                case BrainFillerMode.Wait:
                    // До сих пор ничего не заполняли. Укахываем только сейчас.
                    SetMode(text);
                    break;
                default:
                    // Нам изместно, какую структуру предметной области заполняем
                    switch (DataMode)
                    {
                        // Проверяем, какую логическую структуру данных заполняем
                        case DataStructureMode.None:
                            // Ожидаем указания заполняемой логической структуры данных
                            SetDataStructure(text);
                            break;

                        case DataStructureMode.MapMode:
                            FillMap(text);
                            break;

                        case DataStructureMode.ListMode:
                            FillList(text);
                            break;

                        case DataStructureMode.SynapseMode:
                            FillSynapse(text);
                            break;

                        case DataStructureMode.DendriteMode:
                            FillDendrite(text);
                            break;

                        default:
                            break;
                    }
                    break;
            }
        }

        private void FillList(string? text)
        {
            switch (BrainFillerMode)
            {
                case BrainFillerMode.FillPredictInLayer:
                    bool isNumber = int.TryParse(text, out var number);
                    if (isNumber)
                    {
                        ActiveLateralDendrites.Add(number);
                    }
                    else
                    {
                        SetDataStructure(text);
                    }
                    break;
            }
        }

        private void FillDendrite(string? text)
        {
            bool isApicalDendrite = int.TryParse(text, out var apicalDendrite);
            if (isApicalDendrite)
            {
                ActiveDendrites.ApicalDendrite = apicalDendrite;
            }
            else
            {
                if (text == False)
                {
                    ActiveDendrites.ApicalDendrite = null;
                }
                else
                {
                    SetDataStructure(text);
                }
            }
        }

        private void FillMap(string? text)
        {
            // К нам пришли данные. Если распарсились - ключ.
            // Иначе - вложенная структура
            bool isNumber = int.TryParse(text, out var number);
            if (isNumber)
            {
                SaveMapKey(number);
            }
            else
            {
                SetDataStructure(text);
            }
        }

        private void FillSynapse(string? text)
        {
            bool isSynapseId = int.TryParse(text, out var synapseId);
            if (isSynapseId)
            {
                Synapse.Id = synapseId;
            }
            else
            {
                bool isPermanenceValue = float.TryParse(text, NumberStyles.Any, CultureInfo.GetCultureInfo("fr-FR"), out var permanenceValue);
                if (isPermanenceValue)
                {
                    Synapse.PermanenceValue = permanenceValue;
                }
                else
                {
                    bool isWeight = bool.TryParse(text, out var weight);
                    if (isWeight)
                    {
                        Synapse.Weight = weight;
                    }
                    else
                    {
                        SetDataStructure(text);
                    }
                }
            }
        }

        private void SaveMapKey(int number)
        {
            switch (BrainFillerMode)
            {
                case BrainFillerMode.FillInLayer:
                    switch (Map_CurrentLevel)
                    {
                        case 1:
                            MapKey_InLayer_Level_1 = number;
                            break;
                        case 2:
                            MapKey_InLayer_Level_2 = number;
                            break;
                        case 3:
                            MapKey_InLayer_Level_3 = number;
                            break;
                        case 4:
                            MapKey_InLayer_Level_4 = number;
                            break;
                        default:
                            break;
                    }
                    break;
                case BrainFillerMode.FillPredictInLayer:
                    switch (Map_CurrentLevel)
                    {
                        case 1:
                            MapKey_PredictInLayer_Level_1 = number;
                            break;
                        case 2:
                            MapKey_PredictInLayer_Level_2 = number;
                            break;
                        default:
                            break;
                    }
                    break;
            }
        }

        private void SetDataStructure(string? text)
        {
            switch (text)
            {
                case MapBegin:
                    // Заполняем словарь/map/dictionary
                    // Это может быть не первая логическая структура, поэтому надо запомнить, что заполняли ранее
                    DataStructureStack.Push(DataMode);
                    DataMode = DataStructureMode.MapMode;
                    Map_CurrentLevel++;
                    InitializeMap(Map_CurrentLevel);
                    break;

                case MapEnd:
                    SaveMapValue();
                    DataMode = DataStructureStack.Pop();
                    Map_CurrentLevel--;
                    break;


                case ListBegin:
                    DataStructureStack.Push(DataMode);
                    DataMode = DataStructureMode.ListMode;
                    InitializeList();
                    break;
                
                case ListEnd:
                    SaveList();
                    DataMode = DataStructureStack.Pop();
                    break;

                
                case SynapseBegin:
                    DataStructureStack.Push(DataMode);
                    DataMode = DataStructureMode.SynapseMode;
                    Synapse = new();
                    break;
                
                case SynapseEnd:
                    SaveSynapse();
                    DataMode = DataStructureStack.Pop();
                    break;

                
                case DendriteBegin:
                    DataStructureStack.Push(DataMode);
                    DataMode = DataStructureMode.DendriteMode;
                    ActiveDendrites = new();
                    break;

                case DendriteEnd:
                    SaveDendrite();
                    DataMode = DataStructureStack.Pop();
                    break;


                case End:
                    BrainFillerMode = BrainFillerMode.Wait;
                    break;

                default:
                    break;
            }
        }

        private void SaveDendrite()
        {
            switch (BrainFillerMode)
            {
                case BrainFillerMode.FillPredictInLayer:
                    Map_PredictInLayer_Level_2.Add(MapKey_PredictInLayer_Level_2, ActiveDendrites);
                    break;
            }
        }

        private void SaveSynapse()
        {
            switch (BrainFillerMode)
            {
                case BrainFillerMode.FillInLayer:
                    Map_InLayer_Level_4.Add(MapKey_InLayer_Level_4, Synapse);
                    break;
            }
        }

        private void SaveList()
        {
            switch (BrainFillerMode)
            {
                case BrainFillerMode.FillPredictInLayer:
                    ActiveDendrites.ActiveLateralDendrites = ActiveLateralDendrites;
                    break;
            }
        }

        private void SaveMapValue()
        {
            switch (BrainFillerMode)
            {
                case BrainFillerMode.FillInLayer:
                    switch (Map_CurrentLevel)
                    {
                        case 2:
                            Map_InLayer_Level_1.Add(MapKey_InLayer_Level_1, Map_InLayer_Level_2);
                            break;
                        case 3:
                            Map_InLayer_Level_2.Add(MapKey_InLayer_Level_2, Map_InLayer_Level_3);
                            break;
                        case 4:
                            Map_InLayer_Level_3.Add(MapKey_InLayer_Level_3, Map_InLayer_Level_4);
                            break;
                        default:
                            break;
                    }
                    break;
                case BrainFillerMode.FillPredictInLayer:
                    switch (Map_CurrentLevel)
                    {
                        case 2:
                            Map_PredictInLayer_Level_1.Add(MapKey_PredictInLayer_Level_1, Map_PredictInLayer_Level_2);
                            break;
                        default:
                            break;
                    }
                    break;
            }
        }

        private void InitializeList()
        {
            switch (BrainFillerMode)
            {
                case BrainFillerMode.FillPredictInLayer:
                    ActiveLateralDendrites = [];
                    break;
            }
        }

        private void InitializeMap(int mapCurrentLevel)
        {
            switch (BrainFillerMode)
            {
                case BrainFillerMode.FillInLayer:
                    switch (Map_CurrentLevel)
                    {
                        case 1:
                            Map_InLayer_Level_1 = [];
                            break;
                        case 2:
                            Map_InLayer_Level_2 = [];
                            break;
                        case 3:
                            Map_InLayer_Level_3 = [];
                            break;
                        case 4:
                            Map_InLayer_Level_4 = [];
                            break;
                        default:
                            break;
                    }
                    break;
                case BrainFillerMode.FillPredictInLayer:
                    switch (Map_CurrentLevel)
                    {
                        case 1:
                            Map_PredictInLayer_Level_1 = [];
                            break;
                        case 2:
                            Map_PredictInLayer_Level_2 = [];
                            break;
                        default:
                            break;
                    }
                    break;
            }
        }

        private void SetMode(string? text)
        {
            switch (text)
            {
                case InLayer:
                    // Заполняем входной слой
                    BrainFillerMode = BrainFillerMode.FillInLayer;
                    break;
                case OutLayer:
                    // Заполняем вЫходной слой
                    BrainFillerMode = BrainFillerMode.FillOutLayer;
                    break;
                case ActiveInLayer:
                    // Заполняем активные клетки входного слоя
                    BrainFillerMode = BrainFillerMode.FillActiveInLayer;
                    break;
                case ActiveOutLayer:
                    // Заполняем активные клетки вЫходного слоя
                    BrainFillerMode = BrainFillerMode.FillActiveOutLayer;
                    break;
                case PredictInLayer:
                    // Заполняем предсказанные клетки входного слоя
                    BrainFillerMode = BrainFillerMode.FillPredictInLayer;
                    break;
                default:
                    break;
            }
        }

    }
}
