using System.Drawing;
using System.Windows;
using System.Windows.Media.Imaging;
using ThousandBrainsVisualisation.Model;

namespace ThousandBrainsVisualisation.ViewModel
{
    public class MainWindowViewModel : BaseViewModel
    {
        private readonly BrainModel Brain = new();

        private int ImageWidth;
        private readonly int CellSize = 15;
        private readonly int CellMargin = 12;
        private readonly SolidBrush UsuallyCellBrush = new(Color.Gray);
        private readonly SolidBrush PredictedCellBrush = new(Color.LightBlue);
        private readonly SolidBrush ActiveCellBrush = new(Color.Coral);

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
                    DrawOutCells();
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
                    DrawOutCells();
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

        private BitmapImage _outLayerCells = new();
        public BitmapImage OutLayerCells
        {
            get => _outLayerCells;
            set
            {
                if (value != null)
                {
                    _outLayerCells = value;
                    OnPropertyChanged();
                }
            }
        }

        // TODO На текущий момент механизм отрисовки не универсальный. Значения подобраны так, чтобы корректно отображались 50 мини-колонок на экране 1920x1080
        private void DrawInCells()
        {
            Application.Current.Dispatcher.Invoke(() =>
            {
                // Сохраняем ширину, чтобы остальные изображения были такими же
                ImageWidth = InLayer.Count * CellSize + CellMargin * 2;
                using var bmp = new Bitmap(
                    ImageWidth,
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

        private void DrawOutCells()
        {
            Application.Current.Dispatcher.Invoke(() =>
            {
                int cellsCount = OutLayer.Count;
                int cellsInOneLineCount = (ImageWidth - CellMargin * 2) / CellSize;
                int imageHeight = (cellsCount / cellsInOneLineCount) * CellSize + CellMargin * 2;
                using var bmp = new Bitmap(
                    ImageWidth,
                    imageHeight + CellMargin * 2);
                using var gfx = Graphics.FromImage(bmp);

                gfx.Clear(Color.White);

                int i = 0;
                int j = 0;
                foreach (var miniColumn in OutLayer.OrderBy(c => c.Key))
                {
                    int cellId = miniColumn.Value.Item1;
                    if (i == cellsInOneLineCount)
                    {
                        i = 0;
                        j++;
                    }

                    gfx.FillEllipse(
                        SelectBrushForOutLayer(miniColumn.Key),
                        (CellSize * i) + CellMargin,
                        (CellSize * j) + CellMargin,
                        CellSize,
                        CellSize);
                    i++;
                }

                OutLayerCells = BitmapImageImageFromBitmap(bmp);
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

        private SolidBrush SelectBrushForOutLayer(int miniColumnKey)
        {
            if (ActiveOutLayer.Contains(miniColumnKey))
            {
                return ActiveCellBrush;
            }
            else
            {
                return UsuallyCellBrush;
            }
        }

        private static BitmapImage BitmapImageImageFromBitmap(Bitmap bmp)
        {
            using var memory = new System.IO.MemoryStream();

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
