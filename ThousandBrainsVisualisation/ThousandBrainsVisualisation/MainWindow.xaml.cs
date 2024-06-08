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
        }

        private void InLayerViewMouseDown(object sender, System.Windows.Input.MouseButtonEventArgs e)
        {
            Point p = e.GetPosition(InLayerView);
            ((MainWindowViewModel)DataContext).SelectInCell((int)p.X, (int)p.Y);
        }

        private void OutLayerViewMouseDown(object sender, System.Windows.Input.MouseButtonEventArgs e)
        {
            Point p = e.GetPosition(OutLayerView);
            ((MainWindowViewModel)DataContext).SelectOutCell((int)p.X, (int)p.Y);
        }
    }
}