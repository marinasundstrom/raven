using System.Reflection;

using Microsoft.Extensions.Logging.Abstractions;

using OmniSharp.Extensions.LanguageServer.Protocol.Client.Capabilities;
using OmniSharp.Extensions.LanguageServer.Protocol.Server.Capabilities;

using Raven.LanguageServer;

namespace Raven.Editor.Tests;

public sealed class RavenTextDocumentSyncHandlerTests
{
    [Fact]
    public void GetRegistrationOptions_UsesIncrementalSync()
    {
        var handler = new RavenTextDocumentSyncHandler(
            documents: default!,
            languageServer: default!,
            logger: NullLogger<RavenTextDocumentSyncHandler>.Instance);

        var method = typeof(RavenTextDocumentSyncHandler).GetMethod(
            "CreateRegistrationOptions",
            BindingFlags.Instance | BindingFlags.NonPublic);

        method.ShouldNotBeNull();

        var options = method!.Invoke(
            handler,
            [new TextSynchronizationCapability(), new ClientCapabilities()]);

        options.ShouldNotBeNull();
        var changeProperty = options!.GetType().GetProperty("Change");
        changeProperty.ShouldNotBeNull();
        changeProperty!.GetValue(options).ShouldBe(TextDocumentSyncKind.Incremental);
    }
}
