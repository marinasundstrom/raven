using System.Collections.Generic;
using System.Linq;

using Microsoft.Extensions.Logging;

using Newtonsoft.Json.Linq;

using OmniSharp.Extensions.LanguageServer.Protocol;
using OmniSharp.Extensions.LanguageServer.Protocol.Client.Capabilities;
using OmniSharp.Extensions.LanguageServer.Protocol.Document;
using OmniSharp.Extensions.LanguageServer.Protocol.Models;

using Raven.CodeAnalysis;
using Raven.CodeAnalysis.Syntax;
using Raven.CodeAnalysis.Text;

using LspCodeAction = OmniSharp.Extensions.LanguageServer.Protocol.Models.CodeAction;
using LspDiagnostic = OmniSharp.Extensions.LanguageServer.Protocol.Models.Diagnostic;
using LspRange = OmniSharp.Extensions.LanguageServer.Protocol.Models.Range;
using TextDocumentSelector = OmniSharp.Extensions.LanguageServer.Protocol.Models.TextDocumentSelector;

namespace Raven.LanguageServer;

internal sealed class CodeActionHandler : ICodeActionHandler
{
    private const string ShowMacroExpansionCommand = "raven.showMacroExpansion";
    private readonly DocumentStore _documents;
    private readonly WorkspaceManager _workspaceManager;
    private readonly ILogger<CodeActionHandler> _logger;

    public CodeActionHandler(DocumentStore documents, WorkspaceManager workspaceManager, ILogger<CodeActionHandler> logger)
    {
        _documents = documents;
        _workspaceManager = workspaceManager;
        _logger = logger;
    }

    public CodeActionRegistrationOptions GetRegistrationOptions(CodeActionCapability capability, ClientCapabilities clientCapabilities)
        => new()
        {
            DocumentSelector = TextDocumentSelector.ForLanguage("raven"),
            CodeActionKinds = new Container<CodeActionKind>(CodeActionKind.QuickFix, CodeActionKind.RefactorRewrite)
        };

    public void SetCapability(CodeActionCapability capability)
    {
    }

    public async Task<CommandOrCodeActionContainer?> Handle(CodeActionParams request, CancellationToken cancellationToken)
    {
        try
        {
            using var _ = await _documents.EnterCompilerAccessAsync(cancellationToken).ConfigureAwait(false);
            if (!_documents.TryGetDocument(request.TextDocument.Uri, out var document))
                return new CommandOrCodeActionContainer();

            var documentText = await document.GetTextAsync(cancellationToken).ConfigureAwait(false);
            if (!_workspaceManager.TryGetCodeFixes(request.TextDocument.Uri, out var fixes, cancellationToken: cancellationToken))
                return new CommandOrCodeActionContainer();

            var filteredFixes = fixes
                .Where(fix => IsFixInRequestedRange(fix, request.Range, documentText))
                .Where(fix => MatchesRequestedDiagnostics(fix, request.Context?.Diagnostics, documentText))
                .ToArray();

            var actions = new List<CommandOrCodeAction>(filteredFixes.Length + 1);

            var syntaxTree = await document.GetSyntaxTreeAsync(cancellationToken).ConfigureAwait(false);
            if (syntaxTree is not null &&
                _documents.TryGetCompilation(request.TextDocument.Uri, out var compilation) &&
                compilation is not null)
            {
                var semanticModel = compilation.GetSemanticModel(syntaxTree);
                var root = syntaxTree.GetRoot(cancellationToken);
                if (TryCreateMacroExpansionAction(request.TextDocument.Uri, documentText, semanticModel, root, request.Range, out var macroAction))
                    actions.Add(macroAction);
            }

            foreach (var fix in filteredFixes)
            {
                var action = await TryCreateLspCodeActionAsync(request.TextDocument.Uri, document, fix, cancellationToken).ConfigureAwait(false);
                if (action is not null)
                    actions.Add(action);
            }

            return new CommandOrCodeActionContainer(actions);
        }
        catch (OperationCanceledException) when (cancellationToken.IsCancellationRequested)
        {
            return new CommandOrCodeActionContainer();
        }
        catch (Exception ex)
        {
            _logger.LogError(
                ex,
                "CodeAction request failed for {Uri} at range {StartLine}:{StartChar}-{EndLine}:{EndChar}.",
                request.TextDocument.Uri,
                request.Range.Start.Line,
                request.Range.Start.Character,
                request.Range.End.Line,
                request.Range.End.Character);
            return new CommandOrCodeActionContainer();
        }
    }

    private static bool IsFixInRequestedRange(CodeFix fix, LspRange requestRange, SourceText text)
    {
        if (!fix.Diagnostic.Location.IsInSource)
            return false;

        var fixSpan = fix.Diagnostic.Location.SourceSpan;
        var requestStart = PositionHelper.ToOffset(text, requestRange.Start);
        var requestEnd = PositionHelper.ToOffset(text, requestRange.End);
        if (requestEnd < requestStart)
            (requestStart, requestEnd) = (requestEnd, requestStart);

        return SpansOverlap(fixSpan.Start, fixSpan.End, requestStart, requestEnd);
    }

    private static bool MatchesRequestedDiagnostics(CodeFix fix, Container<LspDiagnostic>? requestedDiagnostics, SourceText text)
    {
        if (requestedDiagnostics is null || !requestedDiagnostics.Any())
            return true;

        foreach (var diagnostic in requestedDiagnostics)
        {
            var diagnosticCode = diagnostic.Code?.String;
            var diagnosticStart = PositionHelper.ToOffset(text, diagnostic.Range.Start);
            var diagnosticEnd = PositionHelper.ToOffset(text, diagnostic.Range.End);
            if (diagnosticEnd < diagnosticStart)
                (diagnosticStart, diagnosticEnd) = (diagnosticEnd, diagnosticStart);

            var fixSpan = fix.Diagnostic.Location.SourceSpan;
            var codeMatches = string.IsNullOrWhiteSpace(diagnosticCode) ||
                string.Equals(diagnosticCode, fix.Diagnostic.Id, StringComparison.OrdinalIgnoreCase);

            if (codeMatches && SpansOverlap(fixSpan.Start, fixSpan.End, diagnosticStart, diagnosticEnd))
                return true;
        }

        return false;
    }

    private static bool SpansOverlap(int leftStart, int leftEnd, int rightStart, int rightEnd)
        => leftStart <= rightEnd && rightStart <= leftEnd;

    private static bool TryCreateMacroExpansionAction(
        DocumentUri uri,
        SourceText documentText,
        SemanticModel semanticModel,
        SyntaxNode root,
        LspRange requestRange,
        out CommandOrCodeAction action)
    {
        action = default!;

        if (!MacroExpansionDisplayService.TryCreateForRange(documentText, semanticModel, root, requestRange, out var display))
            return false;

        action = new CommandOrCodeAction(new Command
        {
            Name = ShowMacroExpansionCommand,
            Title = $"Show macro expansion for #[{display.MacroName}]",
            Arguments = new JArray(uri.ToString(), display.MacroName, display.FullText)
        });

        return true;
    }

    private static async Task<CommandOrCodeAction?> TryCreateLspCodeActionAsync(
        DocumentUri uri,
        Document currentDocument,
        CodeFix fix,
        CancellationToken cancellationToken)
    {
        var currentSolution = currentDocument.Project.Solution;
        var updatedSolution = fix.Action.GetChangedSolution(currentSolution, cancellationToken);
        if (updatedSolution.Version == currentSolution.Version)
            return null;

        var updatedDocument = updatedSolution.GetDocument(fix.DocumentId);
        if (updatedDocument is null)
            return null;

        var currentDocumentForFix = currentSolution.GetDocument(fix.DocumentId);
        if (currentDocumentForFix is null)
            return null;

        var currentText = await currentDocumentForFix.GetTextAsync(cancellationToken).ConfigureAwait(false);
        var textChanges = await updatedDocument.GetTextChangesAsync(currentDocumentForFix, cancellationToken).ConfigureAwait(false);
        if (textChanges.Count == 0)
            return null;

        var edits = textChanges
            .Select(change => new TextEdit
            {
                NewText = change.NewText,
                Range = PositionHelper.ToRange(currentText, change.Span)
            })
            .ToArray();

        var workspaceEdit = new WorkspaceEdit
        {
            Changes = new Dictionary<DocumentUri, IEnumerable<TextEdit>>
            {
                [uri] = edits
            }
        };

        var codeAction = new LspCodeAction
        {
            Title = fix.Action.Title,
            Kind = CodeActionKind.QuickFix,
            Edit = workspaceEdit,
            Diagnostics = new Container<LspDiagnostic>(
                new LspDiagnostic
                {
                    Message = fix.Diagnostic.GetMessage(),
                    Source = "raven",
                    Code = fix.Diagnostic.Id,
                    Range = PositionHelper.ToRange(currentText, fix.Diagnostic.Location.SourceSpan)
                })
        };

        return new CommandOrCodeAction(codeAction);
    }
}
