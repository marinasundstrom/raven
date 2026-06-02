using System.Collections.Immutable;

using Microsoft.Extensions.Logging.Abstractions;

using OmniSharp.Extensions.LanguageServer.Protocol;
using OmniSharp.Extensions.LanguageServer.Protocol.Document;
using OmniSharp.Extensions.LanguageServer.Protocol.Models;

using Raven.CodeAnalysis;
using Raven.CodeAnalysis.Diagnostics;
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
        _ = await store.UpsertDocumentAsync(uri, code);

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
        _ = await store.UpsertDocumentAsync(uri, code);
        manager.TryGetDiagnostics(uri, out _).ShouldBeTrue();

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

    [Fact]
    public async Task Handle_QuickFix_WithRequestDiagnosticsAndNoOnly_DoesNotWaitForBusySemanticGateAsync()
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
        _ = await store.UpsertDocumentAsync(uri, code);
        using var semanticLease = await store.EnterDocumentSemanticAccessAsync(uri, CancellationToken.None, "test");

        var handler = new CodeActionHandler(store, manager, NullLogger<CodeActionHandler>.Instance);

        using var timeout = new CancellationTokenSource(TimeSpan.FromSeconds(2));
        var result = await handler.Handle(
            new CodeActionParams
            {
                TextDocument = new TextDocumentIdentifier(uri),
                Range = new LspRange(new Position(0, 4), new Position(0, 5)),
                Context = new CodeActionContext
                {
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

    [Fact]
    public async Task Handle_QuickFix_WithoutRequestDiagnostics_DoesNotComputeProjectDiagnosticsAsync()
    {
        Directory.CreateDirectory(_tempRoot);
        var filePath = Path.Combine(_tempRoot, "main.rvn");
        var uri = DocumentUri.FromFileSystemPath(filePath);
        const string code = """
missing
""";

        var workspace = RavenWorkspace.Create(targetFramework: "net10.0");
        var manager = new WorkspaceManager(
            workspace,
            NullLogger<WorkspaceManager>.Instance,
            ImmutableArray.Create<CodeFixProvider>(new NameNotFoundCodeFixProvider()),
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
        _ = await store.UpsertDocumentAsync(uri, code);
        var handler = new CodeActionHandler(store, manager, NullLogger<CodeActionHandler>.Instance);

        var result = await handler.Handle(
            new CodeActionParams
            {
                TextDocument = new TextDocumentIdentifier(uri),
                Range = new LspRange(new Position(0, 0), new Position(0, 7)),
                Context = new CodeActionContext
                {
                    Only = new Container<CodeActionKind>(CodeActionKind.QuickFix)
                }
            },
            CancellationToken.None);

        result.ShouldNotBeNull();
        result!.ToArray().ShouldBeEmpty();
    }

    [Fact]
    public async Task Handle_RefactorRewrite_ExtensionLambdaSelection_DoesNotBindColdBodiesAsync()
    {
        Directory.CreateDirectory(_tempRoot);
        var filePath = Path.Combine(_tempRoot, "main.rvn");
        var uri = DocumentUri.FromFileSystemPath(filePath);
        const string code = """
class ServiceCollection {
}

class DbContextOptionsBuilder {
}

class ConnectionFactory {
    static func GetConnectionString() -> string {
        return "Host=localhost"
    }
}

class Runner {
    func Configure(services: ServiceCollection) -> ServiceCollection {
        services.AddDbContext(func (options: DbContextOptionsBuilder) {
            options.UseProvider(ConnectionFactory.GetConnectionString())
        })
    }
}

extension ServiceCollectionExtensions for ServiceCollection {
    func AddDbContext(configure: (DbContextOptionsBuilder -> ())?) -> ServiceCollection {
        return ServiceCollection()
    }
}

extension DbContextOptionsBuilderExtensions for DbContextOptionsBuilder {
    func UseProvider(connectionString: string) -> DbContextOptionsBuilder {
        return DbContextOptionsBuilder()
    }
}
""";

        var workspace = RavenWorkspace.Create(targetFramework: "net10.0");
        var manager = new WorkspaceManager(
            workspace,
            NullLogger<WorkspaceManager>.Instance,
            ImmutableArray<CodeFixProvider>.Empty,
            ImmutableArray.Create<CodeRefactoringProvider>(new TargetTypedUnionCaseRefactoringProvider()));
        manager.Initialize(new InitializeParams
        {
            WorkspaceFolders = new Container<WorkspaceFolder>(new WorkspaceFolder
            {
                Name = "temp",
                Uri = DocumentUri.FromFileSystemPath(_tempRoot)
            })
        });

        var store = new DocumentStore(manager, NullLogger<DocumentStore>.Instance);
        _ = await store.UpsertDocumentAsync(uri, code);

        var context = await store.GetAnalysisContextAsync(uri, CancellationToken.None);
        context.ShouldNotBeNull();

        var useProviderOffset = code.IndexOf("UseProvider", StringComparison.Ordinal);
        useProviderOffset.ShouldBeGreaterThanOrEqualTo(0);
        var range = PositionHelper.ToRange(context.Value.SourceText, new TextSpan(useProviderOffset + 2, 0));
        var handler = new CodeActionHandler(store, manager, NullLogger<CodeActionHandler>.Instance);

        var before = context.Value.Compilation.PerformanceInstrumentation.SemanticQuery.CaptureSnapshot();
        var result = await handler.Handle(
            new CodeActionParams
            {
                TextDocument = new TextDocumentIdentifier(uri),
                Range = range,
                Context = new CodeActionContext
                {
                    Only = new Container<CodeActionKind>(CodeActionKind.RefactorRewrite)
                }
            },
            CancellationToken.None);
        var after = context.Value.Compilation.PerformanceInstrumentation.SemanticQuery.CaptureSnapshot();
        var delta = SemanticQueryInstrumentation.Subtract(after, before);

        result.ShouldNotBeNull();
        delta.SymbolInfoBinderFallbacks.ShouldBe(0);
        delta.TypeInfoBoundFallbacks.ShouldBe(0);
        delta.BoundNodeBindFallbacks.ShouldBe(0);
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

    private sealed class NameNotFoundCodeFixProvider : CodeFixProvider
    {
        public override IEnumerable<string> FixableDiagnosticIds => ["RAV0103"];

        public override void RegisterCodeFixes(CodeFixContext context)
        {
            context.RegisterCodeFix(CodeFixAction.CreateTextChange(
                "Replace missing name",
                context.Document.Id,
                new TextChange(context.Diagnostic.Location.SourceSpan, "replacement")));
        }
    }
}
