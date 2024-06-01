using System.IO;
using System.Net.Sockets;

namespace ThousandBrainsVisualisation.Logic
{
    public class ClientOnServerSide
    {
        public Guid Id { get; }
        protected StreamWriter Writer { get; }
        protected StreamReader Reader { get; }
        protected Stream BinaryReader { get; }

        protected TcpClient Client { get; }
        protected Server Server { get; }

        public ClientOnServerSide(TcpClient tcpClient, Server serverObject)
        {
            Id = Guid.NewGuid();
            Client = tcpClient;
            Server = serverObject;
            BinaryReader = Client.GetStream();
            Reader = new StreamReader(BinaryReader);
            Writer = new StreamWriter(BinaryReader);
        }

        public async Task SendMessage(string message)
        {
            await Writer.WriteLineAsync(message);
            await Writer.FlushAsync();
        }

        public async Task ProcessAsync()
        {
            try
            {
                string? binaryMessage;
                while (true)
                {
                    try
                    {
                        byte[] buffer = new byte[10240];
                        binaryMessage = await Reader.ReadLineAsync();
                        if (string.IsNullOrEmpty(binaryMessage))
                        {
                            continue;
                        }
                        Server.NextSymbol(binaryMessage);
                        binaryMessage = null;
                    }
                    catch (Exception e)
                    {
                        Server.NextSymbol(e.Message);
                        break;
                    }
                }
            }
            catch (Exception e)
            {
                Server.NextSymbol(e.Message);
            }
            finally
            {
                Server.RemoveConnection(Id);
            }
        }
        public void Close()
        {
            Writer.Close();
            Reader.Close();
            Client.Close();
            BinaryReader.Close();
        }
    }
}
