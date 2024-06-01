namespace ThousandBrainsVisualisation.Model
{
    public class Dendrites
    {
        public Dendrites()
        {
            ActiveLateralDendrites = [];
        }

        public int? ApicalDendrite { get; set; }    
        public List<int> ActiveLateralDendrites { get; set; }    
    }
}
