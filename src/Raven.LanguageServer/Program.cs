using Microsoft.Extensions.DependencyInjection;
using Microsoft.Extensions.Logging;

using OmniSharp.Extensions.LanguageServer.Protocol.Models;
using OmniSharp.Extensions.LanguageServer.Server;

using Raven.CodeAnalysis;

using OmniLanguageServer = OmniSharp.Extensions.LanguageServer.Server.LanguageServer;

namespace Raven.LanguageServer;

internal static class Program
{
    static async Task Main(string[] args)
    {
        var server = await OmniLanguageServer.From(options =>
        {
            options
            .WithInput(Console.OpenStandardInput())
            .WithOutput(Console.OpenStandardOutput())
            .WithConfigurationItem(new ConfigurationItem
            {
                Section = "raven"
            })
            .ConfigureLogging(logging =>
            {
                logging.AddLanguageProtocolLogging()
                    .SetMinimumLevel(LogLevel.Debug);

            });
        }).ConfigureAwait(false);

        await server.WaitForExit.ConfigureAwait(false);
    }
}
