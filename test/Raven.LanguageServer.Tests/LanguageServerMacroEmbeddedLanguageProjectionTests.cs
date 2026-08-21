using OmniSharp.Extensions.JsonRpc;

using Raven.CodeAnalysis.Text;
using Raven.LanguageServer;

namespace Raven.LanguageServer.Tests;

public sealed class LanguageServerMacroEmbeddedLanguageProjectionTests
{
    [Fact]
    public void ProjectionRequest_UsesStableCustomMethodName()
    {
        var method = MethodAttribute.From(typeof(MacroEmbeddedLanguageProjectionParams));

        method.ShouldNotBeNull();
        method!.Method.ShouldBe("raven/macroEmbeddedLanguageProjection");
        method.Direction.ShouldBe(Direction.ClientToServer);
    }

    [Fact]
    public void CreateResponse_MapsAuthoredProjectionSpanToLspRange()
    {
        var sourceText = SourceText.From("let view = Markup! {\n    <div></div>\n}");
        var bodyStart = sourceText.ToString().IndexOf('\n');
        var bodyEnd = sourceText.ToString().LastIndexOf('}');
        var response = MacroEmbeddedLanguageProjectionHandler.CreateResponse(
            sourceText,
            "html",
            sourceText.ToString().Substring(bodyStart, bodyEnd - bodyStart),
            TextSpan.FromBounds(bodyStart, bodyEnd));

        response.LanguageId.ShouldBe("html");
        response.Text.ShouldBe("\n    <div></div>\n");
        response.Range.Start.Line.ShouldBe(0);
        response.Range.Start.Character.ShouldBe(20);
        response.Range.End.Line.ShouldBe(2);
        response.Range.End.Character.ShouldBe(0);
    }
}
