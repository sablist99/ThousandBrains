namespace ThousandBrainsVisualisation.Model
{
    public class BrainModel
    {
        public Dictionary<int,
                  Dictionary<int,
                    Dictionary<int,
                        Dictionary<int, Synapse>>>> InLayer;

        public Dictionary<int,
                  Dictionary<int, Dendrites>> PredictInLayer;

        public Dictionary<int,
                    List<int>> ActiveInLayer;

        public Dictionary<int,
                    (int, Dictionary<int,
                             Dictionary<int, Synapse>>)> OutLayer;

        public List<int> ActiveOutLayer;

        public Dictionary<((int?, int?), (int?, int?)), Synapse> FeedForwardSynapses;

        public Dictionary<((int?, int?), (int?, int?)), Synapse> FeedBackSynapses;

        public List<int> LocationSignal;

        //TODO Реализовать передачу настроек мозга по TCP
        public const int LocationSignalSize = 100;

        public BrainModel()
        {
            InLayer = [];
            PredictInLayer = [];
            ActiveInLayer = [];
            OutLayer = [];
            ActiveOutLayer = [];
            FeedForwardSynapses = [];
            FeedBackSynapses = [];
            LocationSignal = [];
        }
    }
}
