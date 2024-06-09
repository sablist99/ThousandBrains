namespace ThousandBrainsVisualisation.Model
{
    public delegate void UpdateInCellsEventHandler();
    public delegate void UpdateOutCellsEventHandler();
    public delegate void SendLocationSignalEventHandler();
    public delegate void SendSensorySignalEventHandler();
    public delegate void SetNeedBrainInitializeEventHandler();
    public delegate void SetNeedBrainPrintEventHandler();

    public class BrainModel
    {
        #region Model
        private Dictionary<int, Dictionary<int, Dictionary<int, Dictionary<int, Synapse>>>> inLayer;
        public Dictionary<int, Dictionary<int, Dictionary<int, Dictionary<int, Synapse>>>> InLayer
        {
            get => inLayer;
            set
            {
                if (value != null)
                {
                    inLayer = value;
                    UpdateInCells();
                };
            }
        }


        private Dictionary<int, Dictionary<int, Dendrites>> predictInLayer;
        public Dictionary<int, Dictionary<int, Dendrites>> PredictInLayer
        {
            get => predictInLayer;
            set
            {
                if (value != null)
                {
                    predictInLayer = value;
                    UpdateInCells();
                };
            }
        }


        private Dictionary<int, List<int>> activeInLayer;
        public Dictionary<int, List<int>> ActiveInLayer
        {
            get => activeInLayer;
            set
            {
                if (value != null)
                {
                    activeInLayer = value;
                    UpdateInCells();
                };
            }
        }


        private Dictionary<int, (int, Dictionary<int, Dictionary<int, Synapse>>)> outLayer;
        public Dictionary<int, (int, Dictionary<int, Dictionary<int, Synapse>>)> OutLayer
        {
            get => outLayer;
            set
            {
                if (value != null)
                {
                    outLayer = value;
                    UpdateOutCells();
                };
            }
        }


        private List<int> activeOutLayer;
        public List<int> ActiveOutLayer
        {
            get => activeOutLayer;
            set
            {
                if (value != null)
                {
                    activeOutLayer = value;
                    UpdateOutCells();
                };
            }
        }


        private Dictionary<((int?, int?), (int?, int?)), Synapse> feedForwardSynapses;
        public Dictionary<((int?, int?), (int?, int?)), Synapse> FeedForwardSynapses
        {
            get => feedForwardSynapses;
            set
            {
                if (value != null)
                {
                    feedForwardSynapses = value;
                };
            }
        }


        private Dictionary<((int?, int?), (int?, int?)), Synapse> feedBackSynapses;
        public Dictionary<((int?, int?), (int?, int?)), Synapse> FeedBackSynapses
        {
            get => feedBackSynapses;
            set
            {
                if (value != null)
                {
                    feedBackSynapses = value;
                };
            }
        }
        #endregion Data

        #region ExternalData
        private List<int> locationSignal = [];
        public List<int> LocationSignal
        {
            get => locationSignal;
            set
            {
                if (value != null)
                {
                    locationSignal = value;
                    SendLocationSignal();
                };
            }
        }

        private List<int> sensorySignal = [];
        public List<int> SensorySignal
        {
            get => sensorySignal;
            set
            {
                if (value != null)
                {
                    sensorySignal = value;
                    SendSensorySignal();
                };
            }
        }
        #endregion

        #region VM data
        private bool needBrainInitialize = false;
        public bool NeedBrainInitialize
        {
            get => needBrainInitialize;
            set
            {
                needBrainInitialize = value;
                if (value == true)
                {
                    SetNeedBrainInitialize();
                }
            }
        }

        private bool needBrainPrint = false;
        public bool NeedBrainPrint
        {
            get => needBrainPrint;
            set
            {
                needBrainPrint = value;
                if (value == true)
                {
                    SetNeedBrainPrint();
                }
            }
        }
        #endregion

        #region Checkers
        public bool HasActiveLateralDendrite(int miniColumnKey, int cellKey, int dendriteKey) => PredictInLayer.ContainsKey(miniColumnKey) 
                                                                                            && PredictInLayer[miniColumnKey].ContainsKey(cellKey)
                                                                                            && PredictInLayer[miniColumnKey][cellKey].ActiveLateralDendrites.Contains(dendriteKey);

        //TODO Добавить условия
        public int GetExistSynapseCountInDendrite(int miniColumnKey, int cellKey, int dendriteKey) => InLayer[miniColumnKey][cellKey][dendriteKey].Where(s => s.Value.Weight == true).Count();
        public int GetActiveSynapseCountInDendrite(int miniColumnKey, int cellKey, int dendriteKey) => InLayer[miniColumnKey][cellKey][dendriteKey].Where(s => s.Value.Weight == true && LocationSignal.Contains(s.Key)).Count();

        public bool IsActiveCellInLayer(int miniColumnKey, int cellKey) => ActiveInLayer.ContainsKey(miniColumnKey) && ActiveInLayer[miniColumnKey].Contains(cellKey);
        public bool IsPredictCellInLayer(int miniColumnKey, int cellKey) => PredictInLayer.ContainsKey(miniColumnKey) && PredictInLayer[miniColumnKey].ContainsKey(cellKey);
        public bool IsActiveCellOutLayer(int miniColumnKey) => ActiveOutLayer.Contains(miniColumnKey);
        #endregion

        //TODO Реализовать передачу настроек мозга по TCP
        public const int LocationSignalSize = 200;

        public event UpdateInCellsEventHandler UpdateInCells = delegate { };
        public event UpdateOutCellsEventHandler UpdateOutCells = delegate { };
        public event SendLocationSignalEventHandler SendLocationSignal = delegate { };
        public event SendSensorySignalEventHandler SendSensorySignal = delegate { };
        public event SetNeedBrainInitializeEventHandler SetNeedBrainInitialize = delegate { };
        public event SetNeedBrainInitializeEventHandler SetNeedBrainPrint = delegate { };

        public BrainModel()
        {
            inLayer = [];
            predictInLayer = [];
            activeInLayer = [];
            outLayer = [];
            activeOutLayer = [];
            feedForwardSynapses = [];
            feedBackSynapses = [];
            LocationSignal = [];
        }
    }
}
