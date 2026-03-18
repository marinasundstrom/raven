using Microsoft.Extensions.Logging.Abstractions;

using OmniSharp.Extensions.LanguageServer.Protocol.Client.Capabilities;

using Raven.LanguageServer;

namespace Raven.Editor.Tests;

public class LanguageServerCompletionRegistrationTests
{
    [Fact]
    public void GetRegistrationOptions_DoesNotTriggerCompletionOnOpenParen()
    {
        var handler = new CompletionHandler(default!, NullLogger<CompletionHandler>.Instance);

        var options = handler.GetRegistrationOptions(new CompletionCapability(), new ClientCapabilities());

        options.TriggerCharacters.ShouldNotBeNull();
        options.TriggerCharacters!.ShouldContain(".");
        options.TriggerCharacters.ShouldContain(":");
        options.TriggerCharacters.ShouldContain("#");
        options.TriggerCharacters.ShouldContain("[");
        options.TriggerCharacters.ShouldNotContain("(");
    }
}
