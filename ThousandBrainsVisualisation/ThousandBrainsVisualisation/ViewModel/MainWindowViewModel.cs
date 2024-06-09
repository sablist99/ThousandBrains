using System.Windows.Media.Imaging;
using ThousandBrainsVisualisation.Logic;
using ThousandBrainsVisualisation.Model;

namespace ThousandBrainsVisualisation.ViewModel
{
    public class MainWindowViewModel(BrainModel brain, Painter painter) : BaseViewModel
    {
        public readonly BrainModel Brain = brain;
        private readonly Painter Painter = painter;

        private bool isBusy = false;
        public bool IsBusy
        {
            get => isBusy;
            set
            {
                isBusy = value;
                OnPropertyChanged();
            }
        }

        private bool hasActiveConnection = false;
        public bool HasActiveConnection
        {
            get => hasActiveConnection;
            set
            {
                hasActiveConnection = value;
                OnPropertyChanged();
            }
        }

        #region Model
        public Dictionary<int, Dictionary<int, Dictionary<int, Dictionary<int, Synapse>>>> InLayer
        {
            get => Brain.InLayer;
        }

        public Dictionary<int, Dictionary<int, Dendrites>> PredictInLayer
        {
            get => Brain.PredictInLayer;
        }

        public Dictionary<int, List<int>> ActiveInLayer
        {
            get => Brain.ActiveInLayer;
        }

        public Dictionary<int, (int, Dictionary<int, Dictionary<int, Synapse>>)> OutLayer
        {
            get => Brain.OutLayer;
        }

        public List<int> ActiveOutLayer
        {
            get => Brain.ActiveOutLayer;
        }

        public Dictionary<((int?, int?), (int?, int?)), Synapse> FeedForwardSynapses
        {
            get => Brain.FeedForwardSynapses;
        }

        public Dictionary<((int?, int?), (int?, int?)), Synapse> FeedBackSynapses
        {
            get => Brain.FeedBackSynapses;
        }

        public List<int> LocationSignal
        {
            get => Brain.LocationSignal;
            set
            {
                if (value != null)
                {
                    Brain.LocationSignal = value;
                    Painter.DrawOutCells();
                };
            }
        }
        #endregion

        #region Bitmaps
        private BitmapImage inLayerCells = new();
        public BitmapImage InLayerCells
        {
            get => inLayerCells;
            set
            {
                if (value != null)
                {
                    inLayerCells = value;
                    OnPropertyChanged();
                }
            }
        }

        private BitmapImage outLayerCells = new();
        public BitmapImage OutLayerCells
        {
            get => outLayerCells;
            set
            {
                if (value != null)
                {
                    outLayerCells = value;
                    OnPropertyChanged();
                }
            }
        }

        private BitmapImage feedBackSynapsesBitmap = new();
        public BitmapImage FeedBackSynapsesBitmap
        {
            get => feedBackSynapsesBitmap;
            set
            {
                if (value != null)
                {
                    feedBackSynapsesBitmap = value;
                    OnPropertyChanged();
                }
            }
        }

        private BitmapImage feedForwardSynapsesBitmap = new();
        public BitmapImage FeedForwardSynapsesBitmap
        {
            get => feedForwardSynapsesBitmap;
            set
            {
                if (value != null)
                {
                    feedForwardSynapsesBitmap = value;
                    OnPropertyChanged();
                }
            }
        }

        private List<DendriteView> basalDendrites { get; set; } = [];
        public List<DendriteView> BasalDendrites
        {
            get => basalDendrites;
            set
            {
                if (value != null)
                {
                    basalDendrites = value;
                    OnPropertyChanged();
                }
            }
        }
        #endregion

        public void SelectInCell(int x, int y)
        {
            Painter.SelectInCell(x, y);
        }

        public void SelectOutCell(int x, int y)
        {
            Painter.SelectOutCell(x, y);
        }

        private void SendLocationSignal()
        {
            IsBusy = true;
            Brain.LocationSignal = [1, 4, 7, 11, 14, 19, 21, 22, 46, 58, 69, 90, 104, 139, 150, 167, 179, 182, 187, 192, 195, 199];
        }

        private void SendSensorySignal()
        {
            IsBusy = true;
            Brain.SensorySignal = [1, 4, 7, 11, 15];
        }

        #region Commands
        private RelayCommand? brainInitializeCommand;
        public RelayCommand BrainInitializeCommand
        {
            get
            {
                return brainInitializeCommand ??= new RelayCommand(obj =>
                {
                    IsBusy = true;
                    Brain.NeedBrainInitialize = true;
                });
            }
        }


        private RelayCommand? brainPrintCommand;
        public RelayCommand BrainPrintCommand
        {
            get
            {
                return brainPrintCommand ??= new RelayCommand(obj =>
                {
                    //IsBusy = true;
                    Brain.NeedBrainPrint = true;
                });
            }
        }


        private RelayCommand? sendLocationSignasCommand;
        public RelayCommand SendLocationSignalCommand
        {
            get
            {
                return sendLocationSignasCommand ??= new RelayCommand(obj =>
                {
                    SendLocationSignal();
                });
            }
        }
        

        private RelayCommand? sendSensorySignasCommand;
        public RelayCommand SendSensorySignalCommand
        {
            get
            {
                return sendSensorySignasCommand ??= new RelayCommand(obj =>
                {
                    SendSensorySignal();
                });
            }
        }
        #endregion
    }
}
