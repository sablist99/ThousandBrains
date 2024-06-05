using System.Windows;
using ThousandBrainsVisualisation.Logic;
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

            BrainFiller.BrainFiller brainFiller = new(mainWindowViewModel);

            Server server = new(brainFiller);
            Task.Run(() => server.ListenAsync());

        }


    }
}