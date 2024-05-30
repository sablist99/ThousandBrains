using System.IO;
using System.Net.Sockets;
using System.Text;

namespace ThousandBrainsVisualisation
{
    public class ClientOnServerSide
    {
        public Guid Id { get; }
        protected StreamWriter Writer { get; }
        protected StreamReader Reader { get; }
        protected Stream BinaryReader { get; }

        protected TcpClient client { get; }
        protected Server server { get; }

        public ClientOnServerSide(TcpClient tcpClient, Server serverObject)
        {
            Id = Guid.NewGuid();
            client = tcpClient;
            server = serverObject;
            BinaryReader = client.GetStream();
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
                        byte[] buffer = new byte[1024];
                        binaryMessage = await Reader.ReadLineAsync();
                        if (string.IsNullOrEmpty(binaryMessage))
                        { 
                            continue; 
                        }
                        server.nextSymbol(binaryMessage);
                        binaryMessage = null;
                    }
                    catch (Exception e)
                    {
                        server.nextSymbol(e.Message);
                    break;
                    }
                }
            }
            catch (Exception e)
            {
                server.nextSymbol(e.Message);
            }
            finally
            {
                server.RemoveConnection(Id);
            }
        }
        public void Close()
        {
            Writer.Close();
            Reader.Close();
            client.Close();
        }
    }
}
