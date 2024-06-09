using System.Windows;
using ThousandBrainsVisualisation.ViewModel;

namespace ThousandBrainsVisualisation.View
{
    /// <summary>
    /// Логика взаимодействия для EditLocationSignal.xaml
    /// </summary>
    public partial class EditSignalWindow : Window
    {
        public EditSignalWindow()
        {
            InitializeComponent();
        }

        private void SignalViewMouseDown(object sender, System.Windows.Input.MouseButtonEventArgs e)
        {
            Point p = e.GetPosition(SignalView);
            ((EditSignalViewModel)DataContext).SelectSignalSynapse((int)p.X, (int)p.Y);
        }
    }
}
