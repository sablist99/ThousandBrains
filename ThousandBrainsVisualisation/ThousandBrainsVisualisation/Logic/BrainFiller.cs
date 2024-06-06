using System.Globalization;
using ThousandBrainsVisualisation.Model;

namespace ThousandBrainsVisualisation.BrainFillerNS
{
    public partial class BrainFiller(BrainModel brain)
    {
        // TODO Добавить в каждом switch сообщение об ошибке, если попали в default 

        /*
         * Сделать универсальный механизм по заполнению любой структуры данных не получится.
         * Потому что у эрланга динамическая типизация, мы заранее не знаем, что лежит в данных и какая вложенность.
         * При заполнении на шарпах нам это нужно знать.
         * Поэтому будем отслеживать уровень вложенности и в зависимости от заполняемой структуры данных (InLayer, PredictCells и т.п.), будем 
         * класть данные в определенные словари
         */

        private BrainModel brain = brain;
        private BrainFillerMode BrainFillerMode = BrainFillerMode.Wait;
        private DataStructureMode DataMode = DataStructureMode.None;
        private Stack<DataStructureMode> DataStructureStack = new();

        private int Map_CurrentLevel = 0;
        private Synapse Synapse = new();
        private int? FeedKey_1;
        private int? FeedKey_2;
        private int? FeedKey_3;
        private int? FeedKey_4;

        // Структура InLayer
        private int MapKey_InLayer_Level_1;
        private int MapKey_InLayer_Level_2;
        private int MapKey_InLayer_Level_3;
        private int MapKey_InLayer_Level_4;
        private Dictionary<int, Dictionary<int, Dictionary<int, Dictionary<int, Synapse>>>> Map_InLayer_Level_1 = []; // Колонки
        private Dictionary<int, Dictionary<int, Dictionary<int, Synapse>>> Map_InLayer_Level_2 = []; // Клетки в колонке
        private Dictionary<int, Dictionary<int, Synapse>> Map_InLayer_Level_3 = []; // Дендриты
        private Dictionary<int, Synapse> Map_InLayer_Level_4 = []; // Синапсы

        // Структура PredictInLayer
        private int MapKey_PredictInLayer_Level_1;
        private int MapKey_PredictInLayer_Level_2;
        private Dictionary<int, Dictionary<int, Dendrites>> Map_PredictInLayer_Level_1 = [];
        private Dictionary<int, Dendrites> Map_PredictInLayer_Level_2 = [];
        private List<int> ActiveLateralDendrites = [];
        private Dendrites ActiveDendrites = new();

        // Структура ActiveInLayer
        private int MapKey_ActiveInLayer_Level_1;
        private Dictionary<int, List<int>> Map_ActiveInLayer_Level_1 = [];
        private List<int> ActiveCellsIds = [];

        // Структура OutLayer
        private int MapKey_OutLayer_Level_1;
        private int MapKey_OutLayer_Level_2;
        private int MapKey_OutLayer_Level_3;
        private Dictionary<int, (int, Dictionary<int, Dictionary<int, Synapse>>)> Map_OutLayer_Level_1 = [];
        private int CellGuid_OutLayer;
        private Dictionary<int, Dictionary<int, Synapse>> Map_OutLayer_Level_2 = [];
        private Dictionary<int, Synapse> Map_OutLayer_Level_3 = [];

        // Структура ActiveOutLayer
        private List<int> ActiveCellOutLayer = [];

        // Структура FeedForward
        private Dictionary<((int?, int?), (int?, int?)), Synapse> FeedForwardSynapses = [];

        // Структура FeedBack
        private Dictionary<((int?, int?), (int?, int?)), Synapse> FeedBackSynapses = [];


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

                        case DataStructureMode.OutColumnMode:
                            FillOutColumn(text);
                            break;

                        case DataStructureMode.FeedMode:
                            FillFeed(text);
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

                case FeedForward:
                    // Заполняем связь входного слоя с выходным
                    BrainFillerMode = BrainFillerMode.FillFeedForward;
                    break;

                case FeedBack:
                    // Заполняем связь выходного слоя с входным
                    BrainFillerMode = BrainFillerMode.FillFeedBack;
                    break;

                default:
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


                case OutColumnBegin:
                    DataStructureStack.Push(DataMode);
                    DataMode = DataStructureMode.OutColumnMode;
                    break;

                case OutColumnEnd:
                    DataMode = DataStructureStack.Pop();
                    break;


                case FeedBegin:
                    DataStructureStack.Push(DataMode);
                    DataMode = DataStructureMode.FeedMode;
                    InitializeFeed();
                    break;

                case FeedEnd:
                    DataMode = DataStructureStack.Pop();
                    break;


                case End:
                    SendFilledStructure();
                    BrainFillerMode = BrainFillerMode.Wait;
                    break;

                default:
                    break;
            }
        }

        private void SendFilledStructure()
        {
            switch (BrainFillerMode)
            {
                case BrainFillerMode.FillInLayer:
                    brain.InLayer = Map_InLayer_Level_1;
                    break;

                case BrainFillerMode.FillPredictInLayer:
                    brain.PredictInLayer = Map_PredictInLayer_Level_1;
                    break;

                case BrainFillerMode.FillActiveInLayer:
                    brain.ActiveInLayer = Map_ActiveInLayer_Level_1;
                    break;

                case BrainFillerMode.FillOutLayer:
                    brain.OutLayer = Map_OutLayer_Level_1;
                    break;

                case BrainFillerMode.FillActiveOutLayer:
                    brain.ActiveOutLayer = ActiveCellOutLayer;
                    break;

                case BrainFillerMode.FillFeedForward:
                    brain.FeedForwardSynapses = FeedForwardSynapses;
                    break;

                case BrainFillerMode.FillFeedBack:
                    brain.FeedBackSynapses = FeedBackSynapses;
                    break;

                default:
                    break;
            }
        }

        #region Initialize 

        private void InitializeFeed()
        {
            FeedKey_1 = FeedKey_2 = FeedKey_3 = FeedKey_4 = null;
        }

        private void InitializeList()
        {
            switch (BrainFillerMode)
            {
                case BrainFillerMode.FillPredictInLayer:
                    ActiveLateralDendrites = [];
                    break;

                case BrainFillerMode.FillActiveInLayer:
                    ActiveCellsIds = [];
                    break;

                case BrainFillerMode.FillActiveOutLayer:
                    ActiveCellOutLayer = [];
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

                case BrainFillerMode.FillActiveInLayer:
                    switch (Map_CurrentLevel)
                    {
                        case 1:
                            Map_ActiveInLayer_Level_1 = [];
                            break;
                        default:
                            break;
                    }
                    break;

                case BrainFillerMode.FillOutLayer:
                    switch (Map_CurrentLevel)
                    {
                        case 1:
                            Map_OutLayer_Level_1 = [];
                            break;
                        case 2:
                            Map_OutLayer_Level_2 = [];
                            break;
                        case 3:
                            Map_OutLayer_Level_3 = [];
                            break;
                        default:
                            break;
                    }
                    break;

                case BrainFillerMode.FillFeedForward:
                    FeedForwardSynapses = [];
                    break;

                case BrainFillerMode.FillFeedBack:
                    FeedBackSynapses = [];
                    break;

                default:
                    break;
            }
        }

        #endregion

        #region Fill

        private void FillFeed(string? text)
        {
            bool isNumber = int.TryParse(text, out var number);
            if (isNumber)
            {
                if (FeedKey_1 == null)
                {
                    FeedKey_1 = number;
                }
                else
                {
                    if (FeedKey_2 == null)
                    {
                        FeedKey_2 = number;
                    }
                    else
                    {
                        if (FeedKey_3 == null)
                        {
                            FeedKey_3 = number;
                        }
                        else
                        {
                            if (FeedKey_4 == null)
                            {
                                FeedKey_4 = number;
                            }

                        }
                    }
                }
            }
            else
            {
                SetDataStructure(text);
            }
        }

        private void FillOutColumn(string? text)
        {
            bool isCellGuid = int.TryParse(text, out var guid);
            if (isCellGuid)
            {
                CellGuid_OutLayer = guid;
            }
            else
            {
                SetDataStructure(text);
            }
        }

        private void FillList(string? text)
        {
            switch (BrainFillerMode)
            {
                case BrainFillerMode.FillPredictInLayer:
                    bool isGuid = int.TryParse(text, out var guid);
                    if (isGuid)
                    {
                        ActiveLateralDendrites.Add(guid);
                    }
                    else
                    {
                        SetDataStructure(text);
                    }
                    break;

                case BrainFillerMode.FillActiveInLayer:
                    bool isCellid = int.TryParse(text, out var cellId);
                    if (isCellid)
                    {
                        ActiveCellsIds.Add(cellId);
                    }
                    else
                    {
                        SetDataStructure(text);
                    }
                    break;

                case BrainFillerMode.FillActiveOutLayer:
                    bool isActiveCellid = int.TryParse(text, out var activeCellId);
                    if (isActiveCellid)
                    {
                        ActiveCellOutLayer.Add(activeCellId);
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
                // TODO Не кастится значение
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

        #endregion

        #region Save
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

                case BrainFillerMode.FillOutLayer:
                    Map_OutLayer_Level_3.Add(MapKey_OutLayer_Level_3, Synapse);
                    break;

                case BrainFillerMode.FillFeedForward:
                    FeedForwardSynapses.Add(((FeedKey_1, FeedKey_2), (FeedKey_3, FeedKey_4)), Synapse);
                    break;

                case BrainFillerMode.FillFeedBack:
                    FeedBackSynapses.Add(((FeedKey_1, FeedKey_2), (FeedKey_3, FeedKey_4)), Synapse);
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

                case BrainFillerMode.FillActiveInLayer:
                    Map_ActiveInLayer_Level_1.Add(MapKey_ActiveInLayer_Level_1, ActiveCellsIds);
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

                case BrainFillerMode.FillOutLayer:
                    switch (Map_CurrentLevel)
                    {
                        case 2:
                            Map_OutLayer_Level_1.Add(MapKey_OutLayer_Level_1, (CellGuid_OutLayer, Map_OutLayer_Level_2));
                            break;
                        case 3:
                            Map_OutLayer_Level_2.Add(MapKey_OutLayer_Level_2, Map_OutLayer_Level_3);
                            break;
                        default:
                            break;
                    }
                    break;

                default:
                    break;
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

                case BrainFillerMode.FillActiveInLayer:
                    switch (Map_CurrentLevel)
                    {
                        case 1:
                            MapKey_ActiveInLayer_Level_1 = number;
                            break;
                        default:
                            break;
                    }
                    break;

                case BrainFillerMode.FillOutLayer:
                    switch (Map_CurrentLevel)
                    {
                        case 1:
                            MapKey_OutLayer_Level_1 = number;
                            break;
                        case 2:
                            MapKey_OutLayer_Level_2 = number;
                            break;
                        case 3:
                            MapKey_OutLayer_Level_3 = number;
                            break;
                        default:
                            break;
                    }
                    break;

                default:
                    break;
            }
        }


        #endregion
    }
}
