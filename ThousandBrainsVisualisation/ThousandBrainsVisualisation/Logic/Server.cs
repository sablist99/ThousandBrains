using System.Net;
using System.Net.Sockets;

namespace ThousandBrainsVisualisation.Logic
{
    public class Server
    {
        public Server(BrainFiller.BrainFiller brainFiller)
        {
            TcpListener = new TcpListener(IPAddress.Any, 8888); ;
            Clients = new List<ClientOnServerSide>();
            BrainFiller = brainFiller;
        }

        protected TcpListener TcpListener { get; set; }
        protected IList<ClientOnServerSide> Clients { get; set; }

        private BrainFiller.BrainFiller BrainFiller { get; set; }

        public void RemoveConnection(Guid id)
        {
            ClientOnServerSide? client = Clients.FirstOrDefault(c => c.Id == id);
            if (client != null)
            {
                Clients.Remove(client);
            }
            client?.Close();
        }

        public void Disconnect()
        {
            foreach (var client in Clients)
            {
                client.Close();
            }
            TcpListener.Stop();
        }

        public async Task ListenAsync()
        {
            try
            {
                TcpListener.Start();
                Console.WriteLine("Сервер запущен. Ожидание подключений...");

                while (true)
                {
                    TcpClient tcpClient = await TcpListener.AcceptTcpClientAsync();

                    ClientOnServerSide clientObject = new(tcpClient, this);
                    Clients.Add(clientObject);
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

        public void NextSymbol(string? text)
        {
            BrainFiller.SendData(text);
        }
    }
}
