using nanoFramework.Networking;
using System.Net.Http;

namespace Raven.Samples.NanoFramework;

public static class NetworkHttpBridge
{
    public static bool ConnectAndGet(string ssid, string password, string requestUri)
    {
        if (!WifiNetworkHelper.ConnectDhcp(ssid, password, requiresDateTime: true))
        {
            return false;
        }

        using var client = new HttpClient();
        using var response = client.Get(requestUri);
        return response.IsSuccessStatusCode;
    }
}
