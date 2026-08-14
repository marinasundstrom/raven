using System;
using System.Diagnostics;
using nanoFramework.Networking;
using System.Net.Http;
using System.Net.Security;
using System.Threading;

namespace Raven.Samples.NanoFramework;

public static class NetworkHttpBridge
{
    public const int Success = 0;
    public const int NetworkUnavailable = 1;
    public const int HttpFailureStatus = 2;
    public const int HttpRequestFailed = 3;

    public static int ConnectAndGet(string ssid, string password, string requestUri)
    {
        Debug.WriteLine("Wi-Fi: waiting for an address...");

        try
        {
            using var networkTimeout = new CancellationTokenSource(60000);
            if (!WifiNetworkHelper.ConnectDhcp(
                    ssid,
                    password,
                    requiresDateTime: false,
                    token: networkTimeout.Token))
            {
                Debug.WriteLine("Wi-Fi failed: " + WifiNetworkHelper.Status.ToString());
                if (WifiNetworkHelper.HelperException != null)
                {
                    Debug.WriteLine("Wi-Fi exception: " + WifiNetworkHelper.HelperException.Message);
                }

                return NetworkUnavailable;
            }
        }
        catch (Exception exception)
        {
            Debug.WriteLine("Wi-Fi setup threw: " + exception.Message);
            return NetworkUnavailable;
        }

        Debug.WriteLine("Wi-Fi ready. Sending GET " + requestUri);

        try
        {
            using var client = new HttpClient
            {
                Timeout = TimeSpan.FromSeconds(30),
                SslVerification = SslVerification.NoVerification,
            };

            Debug.WriteLine("WARNING: TLS certificate verification is disabled for this sample.");

            using var response = client.Get(requestUri);
            Debug.WriteLine("HTTP status: " + ((int)response.StatusCode).ToString());

            return response.IsSuccessStatusCode ? Success : HttpFailureStatus;
        }
        catch (Exception exception)
        {
            Debug.WriteLine("HTTP request failed: " + exception.Message);
            return HttpRequestFailed;
        }
    }
}
