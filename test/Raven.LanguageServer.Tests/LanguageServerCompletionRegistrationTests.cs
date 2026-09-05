using Microsoft.Extensions.Logging.Abstractions;

using OmniSharp.Extensions.LanguageServer.Protocol.Client.Capabilities;

using Raven.LanguageServer;

namespace Raven.LanguageServer.Tests;

public class LanguageServerCompletionRegistrationTests
{
    [Theory]
    [InlineData("(")]
    [InlineData(")")]
    [InlineData("\n")]
    [InlineData("\r")]
    [InlineData("\t")]
    [InlineData("\"")]
    [InlineData("'")]
    [InlineData(";")]
    public void GetRegistrationOptions_DoesNotTriggerForOrdinaryDelimiters(string character)
    {
        var handler = new CompletionHandler(default!, NullLogger<CompletionHandler>.Instance);
        var options = handler.GetRegistrationOptions(new CompletionCapability(), new ClientCapabilities());

        options.TriggerCharacters!.ShouldNotContain(character);
    }

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
        options.TriggerCharacters.ShouldContain("<");
        options.TriggerCharacters.ShouldContain(">");
        options.TriggerCharacters.ShouldContain("/");
        options.TriggerCharacters.ShouldContain(" ");
        options.TriggerCharacters.ShouldNotContain("(");
    }
}
