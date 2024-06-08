using System.Drawing;
using System.Windows;
using System.Windows.Media.Imaging;
using ThousandBrainsVisualisation.Model;

namespace ThousandBrainsVisualisation.ViewModel
{
    public class MainWindowViewModel(BrainModel brain) : BaseViewModel
    {
        private readonly BrainModel Brain = brain;
        private int ImageOutLayerWidth;
        private int ImageOutLayerHeight;
        private int CellsInOneLineCount = 1;

        private readonly int CellSize = 15;
        private readonly int CellMargin = 12;
        private readonly SolidBrush UsuallyCellBrush = new(Color.Gray); // Тут есть синапс, но он не активен
        private readonly SolidBrush PotentialSynapseBrush = new(Color.LightGray); // Тут может быть синапс
        private readonly SolidBrush EmptyCellBrush = new(Color.WhiteSmoke); // Тут мтнапса никогда возникнуть не сможет
        private readonly SolidBrush PredictedCellBrush = new(Color.LightBlue);
        private readonly SolidBrush ActiveCellBrush = new(Color.Coral);

        private bool _isBusy = false;
        public bool IsBusy
        {
            get => _isBusy;
            set
            {
                _isBusy = value;
                OnPropertyChanged();
            }
        }

        private bool _hasActiveConnection = false;
        public bool HasActiveConnection
        {
            get => _hasActiveConnection;
            set
            {
                _hasActiveConnection = value;
                OnPropertyChanged();
            }
        }

        private Tuple<int, int>? selectedCell;

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

        public List<int> LocationSignal
        {
            get => Brain.LocationSignal;
            set
            {
                if (value != null)
                {
                    Brain.LocationSignal = value;
                    DrawOutCells();
                };
            }
        }
        #endregion

        #region Bitmaps
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

        private BitmapImage _apicalDendrite = new();
        public BitmapImage ApicalDendrite
        {
            get => _apicalDendrite;
            set
            {
                if (value != null)
                {
                    _apicalDendrite = value;
                    OnPropertyChanged();
                }
            }
        }

        private List<DendriteView> _basalDendrites { get; set; } = [];
        public List<DendriteView> BasalDendrites
        {
            get => _basalDendrites;
            set
            {
                if (value != null)
                {
                    _basalDendrites = value;
                    OnPropertyChanged();
                }
            }
        }
        #endregion

        // TODO На текущий момент механизм отрисовки не универсальный. Значения подобраны так, чтобы корректно отображались 50 мини-колонок на экране 1920x1080
        // TODO Обернуть в try/catch, потому что при неверной последовательности команд erlang клеток может не быть
        public void DrawInCells()
        {
            Application.Current.Dispatcher.Invoke(() =>
            {
                // Сохраняем ширину, чтобы остальные изображения были такими же
                ImageOutLayerWidth = InLayer.Count * CellSize + CellMargin * 2;
                using var bmp = new Bitmap(
                    ImageOutLayerWidth,
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

                if (selectedCell != null)
                {
                    gfx.DrawEllipse(
                        new Pen(Color.Black, 1),
                        (CellSize * selectedCell.Item1) + CellMargin,
                        (CellSize * selectedCell.Item2) + CellMargin,
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
                int cellsCount = OutLayer.Count;
                CellsInOneLineCount = (ImageOutLayerWidth - CellMargin * 2) / CellSize;
                ImageOutLayerHeight = (int)Math.Ceiling((decimal)cellsCount / CellsInOneLineCount) * CellSize + CellMargin * 2;
                using var bmp = new Bitmap(ImageOutLayerWidth, ImageOutLayerHeight);
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

                OutLayerCells = BitmapImageImageFromBitmap(bmp);
            });
        }

        public void SelectCell(int x, int y)
        {
            int miniColumnKey = (x - CellMargin) / CellSize;
            int cellIndex = (y - CellMargin) / CellSize;
            BasalDendrites = [];
            if (InLayer.ContainsKey(miniColumnKey) && InLayer[miniColumnKey].Count >= cellIndex)
            {
                // Этот замут нужен для того, чтобы обратиться к клетке не по ID, а по порядковому номеру, который соответствует выводу на картинке
                var cell = InLayer[miniColumnKey].OrderBy(c => c.Key).ToList().ElementAt(cellIndex);
                selectedCell = new Tuple<int, int>(miniColumnKey, cellIndex);
                DrawInCells();
                foreach (var dendrite in cell.Value.OrderBy(d => d.Key))
                {
                    BasalDendrites.Add(
                        new DendriteView()
                        {
                            Info = GetDendriteInfo(miniColumnKey, cell.Key, dendrite.Key),
                            Image = GetDentriteImage(miniColumnKey, cell.Key, dendrite.Key, dendrite.Value)
                        });
                }
            }
        }

        private string GetDendriteInfo(int miniColumnKey, int cellKey, int dendriteKey)
        {
            string result = string.Empty;

            result += "Активность: " + (Brain.HasActiveLateralDendrite(miniColumnKey, cellKey, dendriteKey) ? "Да " : "Нет ");
            result += "Количество существующих синапсов: " + Brain.GetExistSynapseCountInDendrite(miniColumnKey, cellKey, dendriteKey) + " ";
            result += "Количество активных синапсов: " + Brain.GetActiveSynapseCountInDendrite(miniColumnKey, cellKey, dendriteKey) + " ";

            return result;
        }

        private BitmapImage GetDentriteImage(int miniColumnKey, int cellKey, int dendriteKey, Dictionary<int, Synapse> dendrite)
        {
            int ImageDendriteHeight = (int)Math.Ceiling((decimal)BrainModel.LocationSignalSize / CellsInOneLineCount) * CellSize + CellMargin * 2;
            using var bmp = new Bitmap(ImageOutLayerWidth, ImageDendriteHeight);

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
            if (Brain.HasActiveLateralDendrite(miniColumnKey, cellKey, dendriteKey))
            {
                gfx.DrawRectangle(
                    new Pen(Color.Green, 3),
                    1, 1, ImageOutLayerWidth-3, ImageDendriteHeight-3);
            }

            return BitmapImageImageFromBitmap(bmp);
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
            if (Brain.IsActiveCellOutLayer(miniColumnKey))
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
        private void SendLocationSignal()
        {
            IsBusy = true;
            Brain.LocationSignal = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16];
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

        private void SendSensorySignal()
        {
            IsBusy = true;
            Brain.SensorySignal = [6, 7, 8];
        }
    }
}
