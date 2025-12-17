using Microsoft.Extensions.DependencyInjection;
using Microsoft.Extensions.Logging;
using OmniSharp.Extensions.LanguageServer.Server;
using Raven.CodeAnalysis;
using OmniLanguageServer = OmniSharp.Extensions.LanguageServer.Server.LanguageServer;

namespace Raven.LanguageServer;

internal static class Program
{
    public static async Task Main(string[] args)
    {
        var workspace = RavenWorkspace.Create();

        var server = await OmniLanguageServer.From(options =>
        {
            options
                .WithInput(Console.OpenStandardInput())
                .WithOutput(Console.OpenStandardOutput())
                .ConfigureLogging(b => b.AddConsole())
                .WithServices(services =>
                {
                    services.AddSingleton(workspace);
                    services.AddSingleton<DocumentStore>();
                })
                .WithHandler<RavenTextDocumentSyncHandler>()
                .WithHandler<CompletionHandler>();
        }).ConfigureAwait(false);

        await server.WaitForExit.ConfigureAwait(false);
    }
}
