using System.Windows;
using ThousandBrainsVisualisation.ViewModel;

namespace ThousandBrainsVisualisation
{
    /// <summary>
    /// Interaction logic for MainWindow.xaml
    /// </summary>
    public partial class MainWindow : Window
    {
        public MainWindow()
        {
            InitializeComponent();
            MainWindowViewModel mainWindowViewModel = new();

            DataContext = mainWindowViewModel;

            Server server = new(mainWindowViewModel);
            Task.Run(() => server.ListenAsync());
        }
    }
}