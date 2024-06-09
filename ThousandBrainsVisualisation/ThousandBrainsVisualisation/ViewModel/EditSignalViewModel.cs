using System.Windows.Media.Imaging;
using ThousandBrainsVisualisation.Logic;
using ThousandBrainsVisualisation.Model;

namespace ThousandBrainsVisualisation.ViewModel
{
    class EditSignalViewModel : BaseViewModel
    {
        public readonly BrainModel Brain;
        private readonly Painter Painter;

        public EditSignalMode Mode { get; set; }

        private BitmapImage signalBitmap = new();
        public BitmapImage SignalBitmap
        {
            get => signalBitmap;
            set
            {
                if (value != null)
                {
                    signalBitmap = value;
                    OnPropertyChanged();
                }
            }
        }

        private List<int> signal = [];
        public List<int> Signal
        {
            get => signal;
            set
            {
                if (value != null)
                {
                    signal = value;
                }
            }
        }

        public void SelectSignalSynapse(int x, int y)
        {
            switch (Mode) 
            {
                case EditSignalMode.LocationSignal:
                    Painter.SelectLocationSignalSynapse(x, y);
                    break;

                case EditSignalMode.SensorySignal:
                    Painter.SelectSensorySignalMiniColumn(x, y);
                    break;
            }
        }

        public EditSignalViewModel(BrainModel brain, EditSignalMode mode)
        {
            Brain = brain;
            Painter = new(brain);

            Mode = mode;

            switch (Mode)
            {
                case EditSignalMode.LocationSignal:
                    Painter.LocationSignalPainted += () => SignalBitmap = Painter.LocationSignalBitmap;
                    Painter.DrawLocationSignal();
                    break;

                case EditSignalMode.SensorySignal:
                    Painter.SensorySignalPainted += () => SignalBitmap = Painter.SensorySignalBitmap;
                    Painter.DrawSensorySignal();
                    break;
            }
        }
    }
}
