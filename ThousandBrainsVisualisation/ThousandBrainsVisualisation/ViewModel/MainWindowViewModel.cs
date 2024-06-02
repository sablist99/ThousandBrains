using System.Drawing;
using System.Windows;
using System.Windows.Media.Imaging;
using ThousandBrainsVisualisation.Model;

namespace ThousandBrainsVisualisation.ViewModel
{
    public class MainWindowViewModel : BaseViewModel
    {
        private readonly BrainModel Brain = new();

        private const int CellSize = 15;
        private const int CellMargin = 12;
        private readonly SolidBrush UsuallyCellBrush = new(Color.Gray);
        private readonly SolidBrush PredictedCellBrush = new(Color.LightBlue);
        private readonly SolidBrush ActiveCellBrush = new(Color.Coral);

        #region Brain OnPropertyChanged
        public Dictionary<int, Dictionary<int, Dictionary<int, Dictionary<int, Synapse>>>> InLayer
        {
            get => Brain.InLayer;
            set
            {
                if (value != null)
                {
                    Brain.InLayer = value;
                    DrawInCells();
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
                    DrawInCells();
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
                    DrawInCells();
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

        private void DrawInCells()
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
                        gfx.FillEllipse(
                            SelectBrushForInLayer(miniColumn.Key, cell.Key),
                            (CellSize * i) + CellMargin,
                            (CellSize * j) + CellMargin,
                            CellSize,
                            CellSize);
                        j++;
                    }
                    i++;
                }

                InLayerCells = BitmapImageImageFromBitmap(bmp);
            });
        }

        private SolidBrush SelectBrushForInLayer(int miniColumnKey, int cellKey)
        {
            if (PredictInLayer.TryGetValue(miniColumnKey, out Dictionary<int, Dendrites>? value) && value.ContainsKey(cellKey))
            {
                if (ActiveInLayer.ContainsKey(miniColumnKey))
                {
                    return ActiveCellBrush;
                }
                else
                {
                    return PredictedCellBrush;
                }
            }
            else
            {
                return UsuallyCellBrush;
            }
        }


        private BitmapImage BitmapImageImageFromBitmap(Bitmap bmp)
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
    }
}
