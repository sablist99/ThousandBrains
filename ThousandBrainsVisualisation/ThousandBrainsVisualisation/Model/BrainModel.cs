namespace ThousandBrainsVisualisation.Model
{
    public delegate void UpdateInCellsEventHandler();
    public delegate void UpdateOutCellsEventHandler();
    public delegate void UpdateLocationSignalEventHandler();
    public delegate void UpdateNeedBrainInitializeEventHandler();

    public class BrainModel
    {
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

        public Dictionary<((int?, int?), (int?, int?)), Synapse> FeedForwardSynapses;

        public Dictionary<((int?, int?), (int?, int?)), Synapse> FeedBackSynapses;

        private List<int> locationSignal = [];
        public List<int> LocationSignal
        {
            get => locationSignal;
            set
            {
                if (value != null)
                {
                    locationSignal = value;
                    UpdateLocationSignal();
                };
            }
        }

        private bool needBrainInitialize = false;
        public bool NeedBrainInitialize 
        { 
            get => needBrainInitialize; 
            set 
            {
                needBrainInitialize = value;
                if (value == true) 
                { 
                    UpdateNeedBrainInitialize(); 
                }
            } 
        }

        //TODO Реализовать передачу настроек мозга по TCP
        public const int LocationSignalSize = 100;

        public event UpdateInCellsEventHandler UpdateInCells = delegate { };
        public event UpdateOutCellsEventHandler UpdateOutCells = delegate { };
        public event UpdateLocationSignalEventHandler UpdateLocationSignal = delegate { };
        public event UpdateNeedBrainInitializeEventHandler UpdateNeedBrainInitialize = delegate { };

        public BrainModel()
        {
            inLayer = [];
            predictInLayer = [];
            activeInLayer = [];
            outLayer = [];
            activeOutLayer = [];
            FeedForwardSynapses = [];
            FeedBackSynapses = [];
            LocationSignal = [];
        }
    }
}
