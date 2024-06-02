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

        public BrainModel()
        {
            InLayer = [];
            PredictInLayer = [];
            ActiveInLayer = [];
            OutLayer = [];
            ActiveOutLayer = [];
            FeedForwardSynapses = [];
            FeedBackSynapses = [];
        }
    }
}
