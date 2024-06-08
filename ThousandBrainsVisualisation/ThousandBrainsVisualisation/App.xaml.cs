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
            Brain.UpdateOutCells += MainWindowViewModel.DrawOutCells;
            Brain.UpdateLocationSignal += SendDataToClient;
            Brain.UpdateNeedBrainInitialize += UpdateNeedBrainInitialize;

            BrainFiller = new(Brain);
            Server = new();

            Server.SendDataToApplication += SendDataToApplication;

            Task.Run(() => Server.ListenAsync());
        }

        protected override void OnActivated(EventArgs e)
        {
            base.OnActivated(e);
            MainWindow.DataContext = MainWindowViewModel;
        }

        private void SendDataToApplication(string? data)
        {
            BrainFiller.SendData(data);
        }

        private void UpdateNeedBrainInitialize()
        {
            Server.SendDataToClient(BrainFiller.NeedBrainInitialize);
            Brain.NeedBrainInitialize = false;
        }

        private void SendDataToClient()
        {
            Server.SendDataToClient(BrainFiller.LocationSignalBegin);
            foreach (int i in Brain.LocationSignal)
            {
                Server.SendDataToClient(i.ToString());
            }
            Server.SendDataToClient(BrainFiller.LocationSignalEnd);
        }
    }
}
