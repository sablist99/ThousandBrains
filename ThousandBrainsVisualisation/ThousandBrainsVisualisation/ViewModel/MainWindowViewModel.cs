using System.Drawing;
using System.Windows;
using System.Windows.Media.Imaging;
using ThousandBrainsVisualisation.Model;

namespace ThousandBrainsVisualisation.ViewModel
{
    public class MainWindowViewModel : BaseViewModel
    {
        private readonly BrainModel Brain = new();

        private const int CellSize = 10;
        private const int CellMargin = 10;
        private readonly SolidBrush UsuallyCellColor = new(Color.Gray);
        //private readonly SolidBrush PredictedCellColor = new(Color.Gray);

        #region Brain OnPropertyChanged
        public Dictionary<int, Dictionary<int, Dictionary<int, Dictionary<int, Synapse>>>> InLayer
        {
            get => Brain.InLayer;
            set
            {
                if (value != null)
                {
                    Brain.InLayer = value;
                    AddCells();
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


        private BitmapImage _inLayerCells = new();
        public BitmapImage InLayerCells
        {
            get => _inLayerCells;
            set
            {
                if (value != null)
                {
                    _inLayerCells = value;
                    OnPropertyChanged();
                }
            }
        }

        private void AddCells()
        {
            Application.Current.Dispatcher.Invoke(() =>
            {
                using var bmp = new Bitmap(
                    InLayer.Count * CellSize + CellMargin * 2,
                    InLayer.FirstOrDefault().Value.Count * CellSize + CellMargin * 2);
                using var gfx = Graphics.FromImage(bmp);

                gfx.Clear(Color.White);

                int i = 0;
                foreach (var miniColumn in InLayer.OrderBy(c => c.Key))
                {
                    int j = 0;
                    foreach (var cell in miniColumn.Value.OrderBy(c => c.Key))
                    {
                        gfx.FillEllipse(UsuallyCellColor, 
                            (CellSize * i) + CellMargin, 
                            (CellSize * j) + CellMargin, 
                            CellSize, 
                            CellSize);
                        j++;
                    }
                    i++;
                }

                InLayerCells = BmpImageFromBmp(bmp);
            });
        }

        private BitmapImage BmpImageFromBmp(Bitmap bmp)
        {
            using (var memory = new System.IO.MemoryStream())
            {
                bmp.Save(memory, System.Drawing.Imaging.ImageFormat.Png);
                memory.Position = 0;

                var bitmapImage = new BitmapImage();
                bitmapImage.BeginInit();
                bitmapImage.StreamSource = memory;
                bitmapImage.CacheOption = BitmapCacheOption.OnLoad;
                bitmapImage.EndInit();
                bitmapImage.Freeze();

                return bitmapImage;
            }
        }

        public MainWindowViewModel()
        {

        }
    }
}
