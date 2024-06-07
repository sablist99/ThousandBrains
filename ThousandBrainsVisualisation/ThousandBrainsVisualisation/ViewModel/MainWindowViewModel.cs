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

        private bool _isBusy = true;
        public bool IsBusy
        {
            get => _isBusy;
            set
            {
                _isBusy = value;
                OnPropertyChanged();
            }
        }
        
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

        private void UpdateDendrites(int miniColumnKey, int cellKey)
        {
            BasalDendrites = [];
            // Пренебрегаем проверкой на наличие ключей
            foreach (var dendrite in InLayer.GetValueOrDefault(miniColumnKey).GetValueOrDefault(cellKey).OrderBy(d => d.Key))
            {
                BasalDendrites.Add(
                    new DendriteView()
                    {
                        Info = GetDendriteInfo(miniColumnKey, cellKey, dendrite.Key),
                        Image = GetDentriteImage(miniColumnKey, dendrite.Value)
                    });
            }
        }

        private string GetDendriteInfo(int miniColumnKey, int cellKey, int dendriteKey)
        {
            string result = string.Empty;

            Dictionary<int, Synapse> dendrite = InLayer[miniColumnKey][cellKey][dendriteKey];

            result += "Активность: " + (PredictInLayer[miniColumnKey][cellKey].ActiveLateralDendrites.Contains(dendriteKey) ? "Да " : "Нет ");
            result += "Количество активных синапсов: " + dendrite.Where(s => s.Value.Weight == true).Count();

            return result;
        }

        private BitmapImage GetDentriteImage(int miniColumnKey, Dictionary<int, Synapse> dendrite)
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
                    SelectBrushForDendrite(miniColumnKey, synapse),
                    (CellSize * i) + CellMargin,
                    (CellSize * j) + CellMargin,
                    CellSize,
                    CellSize);
                i++;
            }

            return BitmapImageImageFromBitmap(bmp);
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

        private SolidBrush SelectBrushForDendrite(int miniColumnKey, Synapse? synapse)
        {
            if (synapse == null)
            {
                return EmptyCellBrush;
            }
            if (synapse.Weight)
            {
                if (LocationSignal.Contains(miniColumnKey)) 
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

        private RelayCommand? updateDendritesCommand;
        public RelayCommand UpdateDendritesCommand
        {
            get
            {
                return updateDendritesCommand ??= new RelayCommand(obj =>
                {
                    UpdateDendrites(0, 1516);
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

        private RelayCommand? brainInitializeCommand;
        public RelayCommand BrainInitializeCommand
        {
            get
            {
                return brainInitializeCommand ??= new RelayCommand(obj =>
                {
                    Brain.NeedBrainInitialize = true;
                });
            }
        }

        private void SendLocationSignal()
        {
            Brain.LocationSignal = [1, 2, 3, 4, 5, 6, 7];
        }
    }
}
