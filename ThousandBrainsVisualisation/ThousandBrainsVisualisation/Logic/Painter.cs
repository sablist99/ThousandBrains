using System.Drawing;
using System.Windows;
using System.Windows.Media.Imaging;
using ThousandBrainsVisualisation.Model;
using ThousandBrainsVisualisation.ViewModel;

namespace ThousandBrainsVisualisation.Logic
{
    public delegate void InCellsPaintedEventHandler();
    public delegate void OutCellsPaintedEventHandler();
    public delegate void FeedBackSynapsesPaintedEventHandler();
    public delegate void FeedForwardSynapsesPaintedEventHandler();
    public delegate void BasalDendritesPaintedEventHandler();
    public delegate void LocationSignalPaintedEventHandler();
    public delegate void SensorySignalPaintedEventHandler();

    public class Painter() : BaseViewModel
    {
        private static readonly SolidBrush UsuallyCellBrush = new(Color.Gray); // Тут есть синапс, но он не активен
        private static readonly SolidBrush PotentialSynapseBrush = new(Color.LightGray); // Тут может быть синапс
        private static readonly SolidBrush EmptyCellBrush = new(Color.WhiteSmoke); // Тут мтнапса никогда возникнуть не сможет
        private static readonly SolidBrush PredictedCellBrush = new(Color.LightBlue);
        private static readonly SolidBrush ActiveCellBrush = new(Color.Coral);
        private static readonly int CellSize = 15;
        private static readonly int CellMargin = 12;
        private int ImageWidth => Brain.InLayer.Count * CellSize + CellMargin * 2;
        private int CellsInOneLineCount => (ImageWidth - CellMargin * 2) / CellSize;
        private int ImageDendriteHeight => (int)Math.Ceiling((decimal)Brain.LocationSignalSize / CellsInOneLineCount) * CellSize + CellMargin * 2;
        private int ImageInLayerHeight => Brain.InLayer.FirstOrDefault().Value.Count * CellSize + CellMargin * 2;
        private int ImageOutLayerHeight => (int)Math.Ceiling((decimal)Brain.OutLayer.Count / CellsInOneLineCount) * CellSize + CellMargin * 2;

        private Tuple<int, int>? selectedInCell;
        private Tuple<int, int>? selectedOutCell;

        private readonly BrainModel brain;
        public BrainModel Brain => brain;

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
                    InCellsPainted();
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
                    OutCellsPainted();
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
                    FeedBackSynapsesPainted();
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
                    FeedForwardSynapsesPainted();
                }
            }
        }

        private BitmapImage feedForwardSignalBitmap = new();
        public BitmapImage FeedForwardSignalBitmap
        {
            get => feedForwardSignalBitmap;
            set
            {
                if (value != null)
                {
                    feedForwardSignalBitmap = value;
                    SensorySignalPainted();
                }
            }
        }

        private BitmapImage locationSignalBitmap = new();
        public BitmapImage LocationSignalBitmap
        {
            get => locationSignalBitmap;
            set
            {
                if (value != null)
                {
                    locationSignalBitmap = value;
                    LocationSignalPainted();
                }
            }
        }

        private BitmapImage sensorySignalBitmap = new();
        public BitmapImage SensorySignalBitmap
        {
            get => sensorySignalBitmap;
            set
            {
                if (value != null)
                {
                    sensorySignalBitmap = value;
                    SensorySignalPainted();
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
                }
            }
        }
        #endregion

        #region ClickOnElipse
        public void SelectLocationSignalSynapse(int x, int y)
        {
            int miniColumnKey = (x - CellMargin) / CellSize;
            int cellIndex = (y - CellMargin) / CellSize;
            int range = cellIndex * CellsInOneLineCount + miniColumnKey;

            // Если не получилось удалить, значит синапса там и не было. Поэтому добавляем
            if (!Brain.LocationSignal.Remove(range))
            {
                Brain.LocationSignal.Add(range);
            }
            DrawLocationSignal();
        }

        // TODO Написать функцию, преобразующую координаты в порядковый номер клетки
        public void SelectSensorySignalMiniColumn(int x, int y)
        {
            int miniColumnKey = (x - CellMargin) / CellSize;

            // Если не получилось удалить, значит синапса там и не было. Поэтому добавляем
            if (!Brain.SensorySignal.Remove(miniColumnKey))
            {
                Brain.SensorySignal.Add(miniColumnKey);
            }
            DrawSensorySignal();
        }

        public void SelectInCell(int x, int y)
        {
            int miniColumnKey = (x - CellMargin) / CellSize;
            int cellIndex = (y - CellMargin) / CellSize;
            BasalDendrites = [];
            if (Brain.InLayer.ContainsKey(miniColumnKey) && Brain.InLayer[miniColumnKey].Count >= cellIndex)
            {
                // Этот замут нужен для того, чтобы обратиться к клетке не по ID, а по порядковому номеру, который соответствует выводу на картинке
                // TODO не обрабатывается ситуация, если промазать и выйти за клетки
                var cell = Brain.InLayer[miniColumnKey].OrderBy(c => c.Key).ToList().ElementAt(cellIndex);
                selectedInCell = new Tuple<int, int>(miniColumnKey, cellIndex);
                DrawInCells();
                DrawFeedForwardSynapses(miniColumnKey, cell.Key);
                foreach (var dendrite in cell.Value.OrderBy(d => d.Key))
                {
                    BasalDendrites.Add(
                        new DendriteView()
                        {
                            Info = GetDendriteInfo(miniColumnKey, cell.Key, dendrite.Key),
                            Image = GetDentriteImage(miniColumnKey, cell.Key, dendrite.Key, dendrite.Value)
                        });
                }
                BasalDendritesPainted();
            }
        }

        public void SelectOutCell(int x, int y)
        {
            int miniColumnKey = (x - CellMargin) / CellSize;
            int cellIndex = (y - CellMargin) / CellSize;
            int range = cellIndex * CellsInOneLineCount + miniColumnKey;
            if (Brain.OutLayer.ContainsKey(range))
            {
                selectedOutCell = new Tuple<int, int>(miniColumnKey, cellIndex);
                DrawOutCells();
                DrawFeedBackSynapses(range);
            }
        }
        #endregion

        #region Draw
        // TODO На текущий момент механизм отрисовки не универсальный. Значения подобраны так, чтобы корректно отображались 50 мини-колонок на экране 1920x1080
        // TODO Обернуть в try/catch, потому что при неверной последовательности команд erlang клеток может не быть
        public void DrawInCells()
        {
            Application.Current.Dispatcher.Invoke(() =>
            {
                using var bmp = new Bitmap(ImageWidth, ImageInLayerHeight);
                using var gfx = Graphics.FromImage(bmp);

                gfx.Clear(Color.White);

                int i = 0;
                foreach (var miniColumn in Brain.InLayer.OrderBy(c => c.Key))
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
                DrawRisk(gfx);

                if (selectedInCell != null)
                {
                    gfx.DrawEllipse(
                        new Pen(Color.Black, 1),
                        (CellSize * selectedInCell.Item1) + CellMargin,
                        (CellSize * selectedInCell.Item2) + CellMargin,
                        CellSize,
                        CellSize);
                }

                InLayerCells = BitmapImageImageFromBitmap(bmp);
            });
        }

        public void DrawOutCells()
        {
            Application.Current.Dispatcher.Invoke(() =>
            {
                using var bmp = new Bitmap(ImageWidth, ImageOutLayerHeight);
                using var gfx = Graphics.FromImage(bmp);

                gfx.Clear(Color.White);

                int i = 0;
                int j = 0;
                foreach (var miniColumn in Brain.OutLayer.OrderBy(c => c.Key))
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
                DrawRisk(gfx);

                if (selectedOutCell != null)
                {
                    gfx.DrawEllipse(
                        new Pen(Color.Black, 1),
                        (CellSize * selectedOutCell.Item1) + CellMargin,
                        (CellSize * selectedOutCell.Item2) + CellMargin,
                        CellSize,
                        CellSize);
                }

                OutLayerCells = BitmapImageImageFromBitmap(bmp);
            });
        }

        private void DrawFeedBackSynapses(int range)
        {
            Application.Current.Dispatcher.Invoke(() =>
            {
                // ImageInLayerHeight - исходим из того, что нелзя нарисовать FeedBack до выходного слоя
                using var bmp = new Bitmap(ImageWidth, ImageInLayerHeight);
                using var gfx = Graphics.FromImage(bmp);

                gfx.Clear(Color.White);

                var synapses = Brain.FeedBackSynapses.Where(f => f.Key.Item1.Item1 == range);

                int i = 0;
                foreach (var miniColumn in Brain.InLayer.OrderBy(c => c.Key))
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
                DrawRisk(gfx);

                FeedBackSynapsesBitmap = BitmapImageImageFromBitmap(bmp);
            });
        }

        private void DrawFeedForwardSynapses(int miniColumnKey, int cellKey)
        {
            Application.Current.Dispatcher.Invoke(() =>
            {
                int cellsCount = Brain.OutLayer.Count;
                // ImageOutLayerHeight - исходим из того, что нелзя нарисовать FeedForward до выходного слоя
                using var bmp = new Bitmap(ImageWidth, ImageOutLayerHeight);
                using var gfx = Graphics.FromImage(bmp);

                gfx.Clear(Color.White);

                var synapses = Brain.FeedForwardSynapses.Where(f => f.Key.Item1.Item1 == miniColumnKey && f.Key.Item1.Item2 == cellKey);

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
                DrawRisk(gfx);

                FeedForwardSynapsesBitmap = BitmapImageImageFromBitmap(bmp);
            });
        }

        public void DrawLocationSignal()
        {
            Application.Current.Dispatcher.Invoke(() =>
            {
                using var bmp = new Bitmap(ImageWidth, ImageDendriteHeight);
                using var gfx = Graphics.FromImage(bmp);

                gfx.Clear(Color.White);


                int i = 0;
                int j = 0;
                for (int s = 0; s < Brain.LocationSignalSize; s++)
                {
                    if (i == CellsInOneLineCount)
                    {
                        i = 0;
                        j++;
                    }

                    gfx.FillEllipse(
                        SelectBrushForLocationSignal(j * CellsInOneLineCount + i),
                        (CellSize * i) + CellMargin,
                        (CellSize * j) + CellMargin,
                        CellSize,
                        CellSize);
                    i++;
                }
                DrawRisk(gfx);

                LocationSignalBitmap = BitmapImageImageFromBitmap(bmp);
            });
        }

        public void DrawSensorySignal()
        {
            Application.Current.Dispatcher.Invoke(() =>
            {
                using var bmp = new Bitmap(ImageWidth, CellMargin * 2 + CellSize);
                using var gfx = Graphics.FromImage(bmp);

                gfx.Clear(Color.White);

                for (int s = 0; s < Brain.InLayer.Count; s++)
                {
                    gfx.FillEllipse(
                        SelectBrushForSensorySignal(s),
                        (CellSize * s) + CellMargin,
                        CellMargin,
                        CellSize,
                        CellSize);
                }
                DrawRisk(gfx);

                SensorySignalBitmap = BitmapImageImageFromBitmap(bmp);
            });
        }

        private void DrawRisk(Graphics g)
        {
            for (int i = 1; i < CellsInOneLineCount / 5; i++)
            {
                g.DrawLine(new Pen(Color.Gray, 1), CellMargin + CellSize * 5 * i, 8, CellMargin + CellSize * 5 * i, 13);
            }
        }
        #endregion

        #region Dendrite Handle
        private BitmapImage GetDentriteImage(int miniColumnKey, int cellKey, int dendriteKey, Dictionary<int, Synapse> dendrite)
        {
            ;
            using var bmp = new Bitmap(ImageWidth, ImageDendriteHeight);
            using var gfx = Graphics.FromImage(bmp);

            gfx.Clear(Color.White);

            int i = 0;
            int j = 0;
            for (int s = 0; s < Brain.LocationSignalSize; s++)
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
            DrawRisk(gfx);

            // Todo Разобраться с повторяющимся вызовом
            if (Brain.HasActiveLateralDendrite(miniColumnKey, cellKey, dendriteKey))
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

            result += "Активность: " + (Brain.HasActiveLateralDendrite(miniColumnKey, cellKey, dendriteKey) ? "Да " : "Нет ");
            result += "Количество существующих синапсов: " + Brain.GetExistSynapseCountInDendrite(miniColumnKey, cellKey, dendriteKey) + " ";
            result += "Количество активных синапсов: " + Brain.GetActiveSynapseCountInDendrite(miniColumnKey, cellKey, dendriteKey) + " ";

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
            if (Brain.IsActiveCellInLayer(miniColumnKey, cellKey))
            {
                return ActiveCellBrush;
            }
            else
            {
                if (Brain.IsPredictCellInLayer(miniColumnKey, cellKey))
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
                if (Brain.LocationSignal.Contains(indexInLocation))
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

        private SolidBrush SelectBrushForLocationSignal(int range) => Brain.LocationSignal.Contains(range) ? ActiveCellBrush : UsuallyCellBrush;

        private SolidBrush SelectBrushForSensorySignal(int range) => Brain.SensorySignal.Contains(range) ? ActiveCellBrush : UsuallyCellBrush;

        private SolidBrush SelectBrushForOutLayer(int miniColumnKey) => Brain.IsActiveCellOutLayer(miniColumnKey) ? ActiveCellBrush : UsuallyCellBrush;
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

        public event InCellsPaintedEventHandler InCellsPainted = delegate { };
        public event OutCellsPaintedEventHandler OutCellsPainted = delegate { };
        public event FeedBackSynapsesPaintedEventHandler FeedBackSynapsesPainted = delegate { };
        public event FeedForwardSynapsesPaintedEventHandler FeedForwardSynapsesPainted = delegate { };
        public event BasalDendritesPaintedEventHandler BasalDendritesPainted = delegate { };
        public event LocationSignalPaintedEventHandler LocationSignalPainted = delegate { };
        public event SensorySignalPaintedEventHandler SensorySignalPainted = delegate { };

        public Painter(BrainModel brain) : this()
        {
            this.brain = brain;
        }
    }
}
