using System.Net;
using System.Net.Sockets;

namespace ThousandBrainsVisualisation.Logic
{
    public delegate void SendDataEventHandler(string? text);
    public class Server
    {
        public Server()
        {
            TcpListener = new TcpListener(IPAddress.Any, 8888);
            Clients = [];
        }

        public event SendDataEventHandler SendData = delegate { };

        protected TcpListener TcpListener { get; set; }
        protected IList<ClientOnServerSide> Clients { get; set; }

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
            SendData(text);
        }
    }
}
