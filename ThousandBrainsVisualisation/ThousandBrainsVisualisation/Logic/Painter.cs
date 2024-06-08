using System.Drawing;
using System.Linq;
using System.Windows;
using System.Windows.Media.Imaging;
using ThousandBrainsVisualisation.Model;
using ThousandBrainsVisualisation.ViewModel;

namespace ThousandBrainsVisualisation.Logic
{
    public class Painter()
    {
        public MainWindowViewModel MainWindowViewModel { get; set; }

        private int ImageWidth;
        private int ImageInLayerHeight;
        private int ImageOutLayerHeight;
        private int CellsInOneLineCount = 1;
        private readonly int CellSize = 15;
        private readonly int CellMargin = 12;
        private readonly SolidBrush UsuallyCellBrush = new(Color.Gray); // Тут есть синапс, но он не активен
        private readonly SolidBrush PotentialSynapseBrush = new(Color.LightGray); // Тут может быть синапс
        private readonly SolidBrush EmptyCellBrush = new(Color.WhiteSmoke); // Тут мтнапса никогда возникнуть не сможет
        private readonly SolidBrush PredictedCellBrush = new(Color.LightBlue);
        private readonly SolidBrush ActiveCellBrush = new(Color.Coral);

        private Tuple<int, int>? selectedInCell;
        private Tuple<int, int>? selectedOutCell;

        public Dictionary<int, Dictionary<int, Dictionary<int, Dictionary<int, Synapse>>>> InLayer
        {
            get => MainWindowViewModel.InLayer;
        }

        public Dictionary<int, Dictionary<int, Dendrites>> PredictInLayer
        {
            get => MainWindowViewModel.PredictInLayer;
        }

        public Dictionary<int, List<int>> ActiveInLayer
        {
            get => MainWindowViewModel.ActiveInLayer;
        }

        public Dictionary<int, (int, Dictionary<int, Dictionary<int, Synapse>>)> OutLayer
        {
            get => MainWindowViewModel.OutLayer;
        }

        public List<int> ActiveOutLayer
        {
            get => MainWindowViewModel.ActiveOutLayer;
        }

        public Dictionary<((int?, int?), (int?, int?)), Synapse> FeedForwardSynapses
        {
            get => MainWindowViewModel.FeedForwardSynapses;
        }

        public Dictionary<((int?, int?), (int?, int?)), Synapse> FeedBackSynapses
        {
            get => MainWindowViewModel.FeedBackSynapses;
        }

        public List<int> LocationSignal
        {
            get => MainWindowViewModel.LocationSignal;
        }

        public void SelectInCell(int x, int y)
        {
            int miniColumnKey = (x - CellMargin) / CellSize;
            int cellIndex = (y - CellMargin) / CellSize;
            MainWindowViewModel.BasalDendrites = [];
            if (InLayer.ContainsKey(miniColumnKey) && InLayer[miniColumnKey].Count >= cellIndex)
            {
                // Этот замут нужен для того, чтобы обратиться к клетке не по ID, а по порядковому номеру, который соответствует выводу на картинке
                // TODO не обрабатывается ситуация, если промазать и выйти за клетки
                var cell = InLayer[miniColumnKey].OrderBy(c => c.Key).ToList().ElementAt(cellIndex);
                selectedInCell = new Tuple<int, int>(miniColumnKey, cellIndex);
                DrawInCells();
                DrawFeedForwardSynapses(miniColumnKey, cell.Key);
                foreach (var dendrite in cell.Value.OrderBy(d => d.Key))
                {
                    MainWindowViewModel.BasalDendrites.Add(
                        new DendriteView()
                        {
                            Info = GetDendriteInfo(miniColumnKey, cell.Key, dendrite.Key),
                            Image = GetDentriteImage(miniColumnKey, cell.Key, dendrite.Key, dendrite.Value)
                        });
                }
            }
        }

        public void SelectOutCell(int x, int y)
        {
            int miniColumnKey = (x - CellMargin) / CellSize;
            int cellIndex = (y - CellMargin) / CellSize;
            int range = cellIndex * CellsInOneLineCount + miniColumnKey;
            if (OutLayer.ContainsKey(range))
            {
                selectedOutCell = new Tuple<int, int>(miniColumnKey, cellIndex);
                DrawOutCells();
                DrawFeedBackSynapses(range);
            }
        }

        #region Draw
        // TODO На текущий момент механизм отрисовки не универсальный. Значения подобраны так, чтобы корректно отображались 50 мини-колонок на экране 1920x1080
        // TODO Обернуть в try/catch, потому что при неверной последовательности команд erlang клеток может не быть
        public void DrawInCells()
        {
            Application.Current.Dispatcher.Invoke(() =>
            {
                // Сохраняем ширину, чтобы остальные изображения были такими же
                ImageWidth = InLayer.Count * CellSize + CellMargin * 2;
                ImageInLayerHeight = InLayer.FirstOrDefault().Value.Count * CellSize + CellMargin * 2;
                using var bmp = new Bitmap(ImageWidth, ImageInLayerHeight);
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

                if (selectedInCell != null)
                {
                    gfx.DrawEllipse(
                        new Pen(Color.Black, 1),
                        (CellSize * selectedInCell.Item1) + CellMargin,
                        (CellSize * selectedInCell.Item2) + CellMargin,
                        CellSize,
                        CellSize);
                }

                MainWindowViewModel.InLayerCells = BitmapImageImageFromBitmap(bmp);
            });
        }

        public void DrawOutCells()
        {
            Application.Current.Dispatcher.Invoke(() =>
            {
                int cellsCount = OutLayer.Count;
                CellsInOneLineCount = (ImageWidth - CellMargin * 2) / CellSize;
                ImageOutLayerHeight = (int)Math.Ceiling((decimal)cellsCount / CellsInOneLineCount) * CellSize + CellMargin * 2;
                using var bmp = new Bitmap(ImageWidth, ImageOutLayerHeight);
                using var gfx = Graphics.FromImage(bmp);

                gfx.Clear(Color.White);

                int i = 0;
                int j = 0;
                foreach (var miniColumn in OutLayer.OrderBy(c => c.Key))
                {
                    if (i == CellsInOneLineCount)
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

                if (selectedOutCell != null)
                {
                    gfx.DrawEllipse(
                        new Pen(Color.Black, 1),
                        (CellSize * selectedOutCell.Item1) + CellMargin,
                        (CellSize * selectedOutCell.Item2) + CellMargin,
                        CellSize,
                        CellSize);
                }

                MainWindowViewModel.OutLayerCells = BitmapImageImageFromBitmap(bmp);
            });
        }

        private void DrawFeedBackSynapses(int range)
        {
            Application.Current.Dispatcher.Invoke(() =>
            {
                using var bmp = new Bitmap(ImageWidth, ImageInLayerHeight);
                using var gfx = Graphics.FromImage(bmp);

                gfx.Clear(Color.White);

                var synapses = FeedBackSynapses.Where(f => f.Key.Item1.Item1 == range);

                int i = 0;
                foreach (var miniColumn in InLayer.OrderBy(c => c.Key))
                {
                    int j = 0;
                    foreach (var cell in miniColumn.Value.OrderBy(c => c.Key))
                    {
                        gfx.FillEllipse(
                            SelectBrushForFeed(synapses.Where(v => v.Key.Item2.Item1 == miniColumn.Key && v.Key.Item2.Item2 == cell.Key).FirstOrDefault().Value),
                            (CellSize * i) + CellMargin,
                            (CellSize * j) + CellMargin,
                            CellSize,
                            CellSize);
                        j++;
                    }
                    i++;
                }

                MainWindowViewModel.FeedBackSynapsesBitmap = BitmapImageImageFromBitmap(bmp);
            });
        }

        private void DrawFeedForwardSynapses(int miniColumnKey, int cellKey)
        {
            Application.Current.Dispatcher.Invoke(() =>
            {
                int cellsCount = OutLayer.Count;
                using var bmp = new Bitmap(ImageWidth, ImageOutLayerHeight);
                using var gfx = Graphics.FromImage(bmp);

                gfx.Clear(Color.White);

                var synapses = FeedForwardSynapses.Where(f => f.Key.Item1.Item1 == miniColumnKey && f.Key.Item1.Item2 == cellKey);

                int i = 0;
                int j = 0;
                for (int s = 0; s < cellsCount; s++)
                {
                    if (i == CellsInOneLineCount)
                    {
                        i = 0;
                        j++;
                    }

                    gfx.FillEllipse(
                        SelectBrushForFeed(synapses.Where(v => v.Key.Item2.Item1 == s).FirstOrDefault().Value),
                        (CellSize * i) + CellMargin,
                        (CellSize * j) + CellMargin,
                        CellSize,
                        CellSize);
                    i++;
                }

                MainWindowViewModel.FeedForwardSynapsesBitmap = BitmapImageImageFromBitmap(bmp);
            });
        }
        #endregion

        #region Dendrite Handle
        private BitmapImage GetDentriteImage(int miniColumnKey, int cellKey, int dendriteKey, Dictionary<int, Synapse> dendrite)
        {
            int ImageDendriteHeight = (int)Math.Ceiling((decimal)BrainModel.LocationSignalSize / CellsInOneLineCount) * CellSize + CellMargin * 2;
            using var bmp = new Bitmap(ImageWidth, ImageDendriteHeight);

            using var gfx = Graphics.FromImage(bmp);

            gfx.Clear(Color.White);

            int i = 0;
            int j = 0;
            for (int s = 0; s < BrainModel.LocationSignalSize; s++)
            {
                if (i == CellsInOneLineCount)
                {
                    i = 0;
                    j++;
                }

                dendrite.TryGetValue(s, out Synapse? synapse);
                gfx.FillEllipse(
                    SelectBrushForDendrite(synapse, s),
                    (CellSize * i) + CellMargin,
                    (CellSize * j) + CellMargin,
                    CellSize,
                    CellSize);
                i++;
            }

            // Todo Разобраться с повторяющимся вызовом
            if (MainWindowViewModel.Brain.HasActiveLateralDendrite(miniColumnKey, cellKey, dendriteKey))
            {
                gfx.DrawRectangle(
                    new Pen(Color.Green, 3),
                    1, 1, ImageWidth - 3, ImageDendriteHeight - 3);
            }

            return BitmapImageImageFromBitmap(bmp);
        }

        private string GetDendriteInfo(int miniColumnKey, int cellKey, int dendriteKey)
        {
            string result = string.Empty;

            result += "Активность: " + (MainWindowViewModel.Brain.HasActiveLateralDendrite(miniColumnKey, cellKey, dendriteKey) ? "Да " : "Нет ");
            result += "Количество существующих синапсов: " + MainWindowViewModel.Brain.GetExistSynapseCountInDendrite(miniColumnKey, cellKey, dendriteKey) + " ";
            result += "Количество активных синапсов: " + MainWindowViewModel.Brain.GetActiveSynapseCountInDendrite(miniColumnKey, cellKey, dendriteKey) + " ";

            return result;
        }
        #endregion

        #region SelectBrush
        private SolidBrush SelectBrushForFeed(Synapse? synapse)
        {
            if (synapse == null)
            {
                return EmptyCellBrush;
            }
            else
            {
                if (synapse.Weight)
                {
                    return UsuallyCellBrush;
                }
                else
                {
                    return PotentialSynapseBrush;
                }
            }
        }

        private SolidBrush SelectBrushForInLayer(int miniColumnKey, int cellKey)
        {
            if (MainWindowViewModel.Brain.IsActiveCellInLayer(miniColumnKey, cellKey))
            {
                return ActiveCellBrush;
            }
            else
            {
                if (MainWindowViewModel.Brain.IsPredictCellInLayer(miniColumnKey, cellKey))
                {
                    return PredictedCellBrush;
                }
                else
                {
                    return UsuallyCellBrush;
                }
            }
        }

        private SolidBrush SelectBrushForDendrite(Synapse? synapse, int indexInLocation)
        {
            if (synapse == null)
            {
                return EmptyCellBrush;
            }
            if (synapse.Weight)
            {
                if (LocationSignal.Contains(indexInLocation))
                {
                    return ActiveCellBrush;
                }
                else
                {
                    return UsuallyCellBrush;
                }
            }
            else
            {
                return PotentialSynapseBrush;
            }
        }

        private SolidBrush SelectBrushForOutLayer(int miniColumnKey)
        {
            if (MainWindowViewModel.Brain.IsActiveCellOutLayer(miniColumnKey))
            {
                return ActiveCellBrush;
            }
            else
            {
                return UsuallyCellBrush;
            }
        }
        #endregion

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
