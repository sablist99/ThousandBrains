using System.Windows;
using ThousandBrainsVisualisation.BrainFillerNS;
using ThousandBrainsVisualisation.Logic;
using ThousandBrainsVisualisation.Model;
using ThousandBrainsVisualisation.ViewModel;

namespace ThousandBrainsVisualisation
{
    /// <summary>
    /// Interaction logic for App.xaml
    /// </summary>
    public partial class App : Application
    {
        private BrainModel Brain;
        private MainWindowViewModel MainWindowViewModel;
        private BrainFiller BrainFiller;
        private Server Server;
        public App()
        {
            Brain = new();
            MainWindowViewModel = new(Brain);

            Brain.UpdateInCells += MainWindowViewModel.DrawInCells;
            Brain.UpdateOutCells += MainWindowViewModel.DrawOutCells;

            BrainFiller = new(Brain);
            Server = new();

            Server.SendData += SendData;
            
            Task.Run(() => Server.ListenAsync());
        }

        protected override void OnActivated(EventArgs e)
        {
            base.OnActivated(e);
            MainWindow.DataContext = MainWindowViewModel;
        }

        private void SendData(string? data)
        {
            BrainFiller.SendData(data);
        }
    }
}
