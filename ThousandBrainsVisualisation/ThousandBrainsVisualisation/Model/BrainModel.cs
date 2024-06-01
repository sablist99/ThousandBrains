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

        public BrainModel()
        {
            InLayer = [];
            PredictInLayer = [];
        }
    }
}
