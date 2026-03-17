using Microsoft.Extensions.Logging.Abstractions;

using OmniSharp.Extensions.LanguageServer.Protocol;
using OmniSharp.Extensions.LanguageServer.Protocol.Models;

using Raven.CodeAnalysis;
using Raven.CodeAnalysis.Text;
using Raven.LanguageServer;

namespace Raven.Editor.Tests;

public sealed class LanguageServerSignatureHelpTests : IDisposable
{
    private readonly string _tempRoot = Path.Combine(Path.GetTempPath(), $"raven-ls-sighelp-{Guid.NewGuid():N}");

    [Fact]
    public async Task SignatureHelpHandler_IncompleteInvocation_ShowsAllOverloadsAsync()
    {
        Directory.CreateDirectory(_tempRoot);

        var workspace = RavenWorkspace.Create(targetFramework: "net10.0");
        var manager = new WorkspaceManager(workspace, NullLogger<WorkspaceManager>.Instance);
        manager.Initialize(new InitializeParams
        {
            WorkspaceFolders = new Container<WorkspaceFolder>(new WorkspaceFolder
            {
                Name = "temp",
                Uri = DocumentUri.FromFileSystemPath(_tempRoot)
            })
        });

        var store = new DocumentStore(manager, NullLogger<DocumentStore>.Instance);
        var handler = new SignatureHelpHandler(store, NullLogger<SignatureHelpHandler>.Instance);
        var documentPath = Path.Combine(_tempRoot, "main.rvn");
        var uri = DocumentUri.FromFileSystemPath(documentPath);
        const string code = """
func Foo() -> () {
}

func Foo(value: int) -> () {
}

func Main() -> () {
    Foo()
}
""";

        store.UpsertDocument(uri, code);
        var sourceText = SourceText.From(code);
        var offset = code.LastIndexOf("Foo(", StringComparison.Ordinal) + "Foo(".Length;
        offset.ShouldBeGreaterThan(0);

        var result = await handler.Handle(new SignatureHelpParams
        {
            TextDocument = new TextDocumentIdentifier(uri),
            Position = PositionHelper.ToRange(sourceText, new Raven.CodeAnalysis.Text.TextSpan(offset, 0)).Start
        }, CancellationToken.None);

        result.ShouldNotBeNull();
        result.Signatures.ShouldNotBeNull();
        result.Signatures.Select(signature => signature.Label).Count().ShouldBe(2);
        result.Signatures.Select(signature => signature.Label).ShouldContain("func Foo() -> ()");
        result.Signatures.Select(signature => signature.Label).ShouldContain("func Foo(value: int) -> ()");
    }

    public void Dispose()
    {
        if (Directory.Exists(_tempRoot))
            Directory.Delete(_tempRoot, recursive: true);
    }
}
