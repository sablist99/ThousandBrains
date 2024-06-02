using System.Collections.ObjectModel;
using System.Windows;
using System.Windows.Media;
using System.Windows.Shapes;
using ThousandBrainsVisualisation.Model;

namespace ThousandBrainsVisualisation.ViewModel
{
    public class MainWindowViewModel : BaseViewModel
    {
        private readonly BrainModel Brain = new();

        #region Brain OnPropertyChanged
        public Dictionary<int, Dictionary<int, Dictionary<int, Dictionary<int, Synapse>>>> InLayer
        {
            get => Brain.InLayer;
            set
            {
                if (value != null)
                {
                    Brain.InLayer = value;
                    OnPropertyChanged();
                };
            }
        }

        public Dictionary<int, Dictionary<int, Dendrites>> PredictInLayer
        {
            get => Brain.PredictInLayer;
            set
            {
                if (value != null)
                {
                    Brain.PredictInLayer = value;
                    OnPropertyChanged();
                };
            }
        }

        public Dictionary<int, List<int>> ActiveInLayer
        {
            get => Brain.ActiveInLayer;
            set
            {
                if (value != null)
                {
                    Brain.ActiveInLayer = value;
                    OnPropertyChanged();
                };
            }
        }

        public Dictionary<int, (int, Dictionary<int, Dictionary<int, Synapse>>)> OutLayer
        {
            get => Brain.OutLayer;
            set
            {
                if (value != null)
                {
                    Brain.OutLayer = value;
                    OnPropertyChanged();
                };
            }
        }

        public List<int> ActiveOutLayer
        {
            get => Brain.ActiveOutLayer;
            set
            {
                if (value != null)
                {
                    Brain.ActiveOutLayer = value;
                    OnPropertyChanged();
                };
            }
        }

        public Dictionary<((int?, int?), (int?, int?)), Synapse> FeedForwardSynapses
        {
            get => Brain.FeedForwardSynapses;
            set
            {
                if (value != null)
                {
                    Brain.FeedForwardSynapses = value;
                    OnPropertyChanged();
                };
            }
        }

        public Dictionary<((int?, int?), (int?, int?)), Synapse> FeedBackSynapses
        {
            get => Brain.FeedBackSynapses;
            set
            {
                if (value != null)
                {
                    Brain.FeedBackSynapses = value;
                    OnPropertyChanged();
                };
            }
        }

        #endregion


        public ObservableCollection<Ellipse> InLayerCells { get; set; } = [];


        public MainWindowViewModel()
        {

        }
    }
}
