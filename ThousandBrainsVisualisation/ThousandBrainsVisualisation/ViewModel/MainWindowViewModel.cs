using System.Windows.Media.Imaging;
using ThousandBrainsVisualisation.Logic;
using ThousandBrainsVisualisation.Model;
using ThousandBrainsVisualisation.View;

namespace ThousandBrainsVisualisation.ViewModel
{
    public class MainWindowViewModel : BaseViewModel
    {
        public readonly BrainModel Brain;
        private readonly Painter Painter;

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
            Brain.NeedSendLocationSignal = true;
        }

        private void SendSensorySignal()
        {
            IsBusy = true;
            Brain.NeedSendSensorySignal = true;
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


        private RelayCommand? editLocationSignalCommand;
        public RelayCommand EditLocationSignalCommand
        {
            get
            {
                return editLocationSignalCommand ??= new RelayCommand(obj =>
                {
                    EditSignalViewModel editSignalViewModel = new(Brain, EditSignalMode.LocationSignal);
                    EditSignalWindow editSignal = new()
                    {
                        DataContext = editSignalViewModel
                    };
                    editSignal.ShowDialog();
                });
            }
        }


        private RelayCommand? editSensorySignalCommand;
        public RelayCommand EditSensorySignalCommand
        {
            get
            {
                return editSensorySignalCommand ??= new RelayCommand(obj =>
                {
                    EditSignalViewModel editSignalViewModel = new(Brain, EditSignalMode.SensorySignal);
                    EditSignalWindow editSignal = new()
                    {
                        DataContext = editSignalViewModel
                    };
                    editSignal.ShowDialog();
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

        public MainWindowViewModel(BrainModel brain)
        {
            Brain = brain;
            Painter = new(brain);

            Brain.UpdateInCells += Painter.DrawInCells;
            Brain.UpdateOutCells += Painter.DrawOutCells;

            Painter.InCellsPainted += () => InLayerCells = Painter.InLayerCells;
            Painter.OutCellsPainted += () => OutLayerCells = Painter.OutLayerCells;
            Painter.FeedBackSynapsesPainted += () => FeedBackSynapsesBitmap = Painter.FeedBackSynapsesBitmap;
            Painter.FeedForwardSynapsesPainted += () => FeedForwardSynapsesBitmap = Painter.FeedForwardSynapsesBitmap;
            Painter.BasalDendritesPainted += () => BasalDendrites = Painter.BasalDendrites;
        }
    }
}
