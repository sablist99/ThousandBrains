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

            Brain.SendLocationSignal += SendLocationSignalToClient;
            Brain.SendSensorySignal += SendSensorySignalToClient;
            Brain.SetNeedBrainInitialize += SetNeedBrainInitialize;
            Brain.SetNeedBrainPrint += SetNeedBrainPrint;

            BrainFiller = new(Brain);
            BrainFiller.BeginGetData += SetIsBusy;
            BrainFiller.EndGetData += UnSetIsBusy;

            Server = new();
            Server.SendDataToApplication += SendDataToApplication;
            Server.ChangeConnectionsCount += SetHasActiveConnection;

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

        private void SetHasActiveConnection(int count)
        {
            // Todo обнулять данные, если нет колонок
            MainWindowViewModel.HasActiveConnection = count > 0;
        }

        private void SetIsBusy()
        {
            MainWindowViewModel.IsBusy = true;
        }

        private void UnSetIsBusy()
        {
            MainWindowViewModel.IsBusy = false;
        }

        // TODO Переделать на асинхронные методы
        private void SetNeedBrainInitialize()
        {
            Server.SendDataToClient(BrainCommands.NeedBrainInitialize);
            Brain.NeedBrainInitialize = false;
        }

        private void SetNeedBrainPrint()
        {
            Server.SendDataToClient(BrainCommands.NeedBrainPrint);
            Brain.NeedBrainPrint = false;
        }

        private void SendLocationSignalToClient()
        {
            Server.SendDataToClient(BrainCommands.LocationSignalBegin);
            foreach (int i in Brain.LocationSignal)
            {
                Server.SendDataToClient(i.ToString());
            }
            Server.SendDataToClient(BrainCommands.LocationSignalEnd);
            Brain.NeedSendLocationSignal = false;
        }

        private void SendSensorySignalToClient()
        {
            Server.SendDataToClient(BrainCommands.SensorySignalBegin);
            foreach (int i in Brain.SensorySignal)
            {
                Server.SendDataToClient(i.ToString());
            }
            Server.SendDataToClient(BrainCommands.SensorySignalEnd);
            Brain.NeedSendSensorySignal = false;
        }
    }
}
