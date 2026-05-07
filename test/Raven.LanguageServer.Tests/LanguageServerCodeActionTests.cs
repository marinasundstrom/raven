using System.Collections.Immutable;

using Microsoft.Extensions.Logging.Abstractions;

using OmniSharp.Extensions.LanguageServer.Protocol;
using OmniSharp.Extensions.LanguageServer.Protocol.Document;
using OmniSharp.Extensions.LanguageServer.Protocol.Models;

using Raven.CodeAnalysis;
using Raven.CodeAnalysis.Text;
using Raven.LanguageServer;

namespace Raven.Editor.Tests;

using CodeFixAction = Raven.CodeAnalysis.CodeAction;
using LspDiagnostic = OmniSharp.Extensions.LanguageServer.Protocol.Models.Diagnostic;
using LspRange = OmniSharp.Extensions.LanguageServer.Protocol.Models.Range;

public sealed class LanguageServerCodeActionTests : IDisposable
{
    private readonly string _tempRoot = Path.Combine(Path.GetTempPath(), $"raven-ls-code-actions-{Guid.NewGuid():N}");

    [Fact]
    public async Task Handle_InvalidRefactoringTextChange_SkipsActionAndReturnsRemainingActionsAsync()
    {
        Directory.CreateDirectory(_tempRoot);
        var filePath = Path.Combine(_tempRoot, "main.rvn");
        var uri = DocumentUri.FromFileSystemPath(filePath);
        const string code = """
func Main() -> unit {
}
""";

        var workspace = RavenWorkspace.Create(targetFramework: "net10.0");
        var manager = new WorkspaceManager(
            workspace,
            NullLogger<WorkspaceManager>.Instance,
            ImmutableArray<CodeFixProvider>.Empty,
            ImmutableArray.Create<CodeRefactoringProvider>(new InvalidAndValidRefactoringProvider()));
        manager.Initialize(new InitializeParams
        {
            WorkspaceFolders = new Container<WorkspaceFolder>(new WorkspaceFolder
            {
                Name = "temp",
                Uri = DocumentUri.FromFileSystemPath(_tempRoot)
            })
        });

        var store = new DocumentStore(manager, NullLogger<DocumentStore>.Instance);
        _ = store.UpsertDocument(uri, code);

        var handler = new CodeActionHandler(store, manager, NullLogger<CodeActionHandler>.Instance);

        var result = await handler.Handle(
            new CodeActionParams
            {
                TextDocument = new TextDocumentIdentifier(uri),
                Range = new LspRange(new Position(0, 0), new Position(0, 0)),
                Context = new CodeActionContext
                {
                    Only = new Container<CodeActionKind>(CodeActionKind.RefactorRewrite)
                }
            },
            CancellationToken.None);

        result.ShouldNotBeNull();
        var actions = result!.ToArray();
        actions.Length.ShouldBe(2);
        actions[0].CodeAction.ShouldNotBeNull();
        actions[0].CodeAction!.Title.ShouldBe("Valid refactoring");
        actions[1].Command.ShouldNotBeNull();
        actions[1].Command!.Title.ShouldBe("Preview: Valid refactoring");
    }

    [Fact]
    public async Task Handle_QuickFix_UsesRequestDiagnosticsWithoutProjectDiagnosticPassAsync()
    {
        Directory.CreateDirectory(_tempRoot);
        var filePath = Path.Combine(_tempRoot, "main.rvn");
        var uri = DocumentUri.FromFileSystemPath(filePath);
        const string code = """
val x = 1
""";

        var workspace = RavenWorkspace.Create(targetFramework: "net10.0");
        var manager = new WorkspaceManager(
            workspace,
            NullLogger<WorkspaceManager>.Instance,
            ImmutableArray.Create<CodeFixProvider>(new RequestDiagnosticCodeFixProvider()),
            ImmutableArray<CodeRefactoringProvider>.Empty);
        manager.Initialize(new InitializeParams
        {
            WorkspaceFolders = new Container<WorkspaceFolder>(new WorkspaceFolder
            {
                Name = "temp",
                Uri = DocumentUri.FromFileSystemPath(_tempRoot)
            })
        });

        var store = new DocumentStore(manager, NullLogger<DocumentStore>.Instance);
        _ = store.UpsertDocument(uri, code);

        var handler = new CodeActionHandler(store, manager, NullLogger<CodeActionHandler>.Instance);

        using var timeout = new CancellationTokenSource(TimeSpan.FromSeconds(5));
        var result = await handler.Handle(
            new CodeActionParams
            {
                TextDocument = new TextDocumentIdentifier(uri),
                Range = new LspRange(new Position(0, 4), new Position(0, 5)),
                Context = new CodeActionContext
                {
                    Only = new Container<CodeActionKind>(CodeActionKind.QuickFix),
                    Diagnostics = new Container<LspDiagnostic>(new LspDiagnostic
                    {
                        Code = RequestDiagnosticCodeFixProvider.DiagnosticId,
                        Message = "Request supplied diagnostic",
                        Source = "raven",
                        Range = new LspRange(new Position(0, 4), new Position(0, 5))
                    })
                }
            },
            timeout.Token);

        result.ShouldNotBeNull();
        var actions = result!.ToArray();
        actions.Length.ShouldBe(2);
        actions[0].CodeAction.ShouldNotBeNull();
        actions[0].CodeAction!.Title.ShouldBe("Apply request diagnostic fix");
        actions[1].Command.ShouldNotBeNull();
        actions[1].Command!.Title.ShouldBe("Preview: Apply request diagnostic fix");
    }

    public void Dispose()
    {
        if (Directory.Exists(_tempRoot))
            Directory.Delete(_tempRoot, recursive: true);
    }

    private sealed class InvalidAndValidRefactoringProvider : CodeRefactoringProvider
    {
        public override void RegisterRefactorings(CodeRefactoringContext context)
        {
            context.RegisterRefactoring(CodeFixAction.CreateTextChange(
                "Invalid refactoring",
                context.Document.Id,
                new TextChange(new TextSpan(10_000, 0), "invalid")));
            context.RegisterRefactoring(CodeFixAction.CreateTextChange(
                "Valid refactoring",
                context.Document.Id,
                new TextChange(new TextSpan(0, 0), "// valid\n")));
        }
    }

    private sealed class RequestDiagnosticCodeFixProvider : CodeFixProvider
    {
        public const string DiagnosticId = "TEST001";

        public override IEnumerable<string> FixableDiagnosticIds => [DiagnosticId];

        public override void RegisterCodeFixes(CodeFixContext context)
        {
            if (!string.Equals(context.Diagnostic.Id, DiagnosticId, StringComparison.OrdinalIgnoreCase))
                return;

            context.RegisterCodeFix(CodeFixAction.CreateTextChange(
                "Apply request diagnostic fix",
                context.Document.Id,
                new TextChange(context.Diagnostic.Location.SourceSpan, "y")));
        }
    }
}
