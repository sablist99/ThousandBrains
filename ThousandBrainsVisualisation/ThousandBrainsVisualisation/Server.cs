using System.Net;
using System.Net.Sockets;
using ThousandBrainsVisualisation.ViewModel;

namespace ThousandBrainsVisualisation
{
    public class Server
    {
        public Server(MainWindowViewModel mainWindowViewModel)
        {
            MainWindowViewModel = mainWindowViewModel;
            tcpListener = new TcpListener(IPAddress.Any, 8888); ;
            clients = new List<ClientOnServerSide>();
        }

        protected TcpListener tcpListener { get; set; }
        protected IList<ClientOnServerSide> clients { get; set; }

        MainWindowViewModel MainWindowViewModel { get; set; }


        public void RemoveConnection(Guid id)
        {
            ClientOnServerSide? client = clients.FirstOrDefault(c => c.Id == id);
            if (client != null)
            {
                clients.Remove(client);
            }
            client?.Close();
        }

        public void Disconnect()
        {
            foreach (var client in clients)
            {
                client.Close();
            }
            tcpListener.Stop();
        }

        public async Task ListenAsync()
        {
            try
            {
                tcpListener.Start();
                Console.WriteLine("Сервер запущен. Ожидание подключений...");

                while (true)
                {
                    TcpClient tcpClient = await tcpListener.AcceptTcpClientAsync();

                    ClientOnServerSide clientObject = new(tcpClient, this);
                    clients.Add(clientObject);
                    Task.Run(clientObject.ProcessAsync);
                }
            }
            catch (Exception ex)
            {
                Console.WriteLine(ex.Message);
            }
            finally
            {
                Disconnect();
            }
        }

        public void nextSymbol(string? text)
        {
            MainWindowViewModel.SynchronizedText += text + "\n";
        }
    }
}
