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

        private Dictionary<int,
                    List<int>> ActiveInLayer;

        private Dictionary<int,
                    (int, Dictionary<int,
                             Dictionary<int, Synapse>>)> OutLayer;

        private List<int> ActiveOutLayer;

        private Dictionary<((int?, int?), (int?, int?)), Synapse> FeedForwardSynapses;

        private Dictionary<((int?, int?), (int?, int?)), Synapse> FeedBackSynapses;

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
