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

using CodeDiagnostic = Raven.CodeAnalysis.Diagnostic;
using CodeFixAction = Raven.CodeAnalysis.CodeAction;
using LspCodeAction = OmniSharp.Extensions.LanguageServer.Protocol.Models.CodeAction;
using LspDiagnostic = OmniSharp.Extensions.LanguageServer.Protocol.Models.Diagnostic;
using LspRange = OmniSharp.Extensions.LanguageServer.Protocol.Models.Range;
using TextDocumentSelector = OmniSharp.Extensions.LanguageServer.Protocol.Models.TextDocumentSelector;

namespace Raven.LanguageServer;

internal sealed class CodeActionHandler : ICodeActionHandler
{
    private const string ShowMacroExpansionCommand = "raven.showMacroExpansion";
    private const string ShowCodeActionPreviewCommand = "raven.showCodeActionPreview";
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
            if (!_documents.TryGetDocumentContext(request.TextDocument.Uri, out var document, out var compilation) || compilation is null)
                return new CommandOrCodeActionContainer();

            var documentText = await document.GetTextAsync(cancellationToken).ConfigureAwait(false);
            var selectionSpan = GetRequestedSpan(documentText, request.Range);
            if (!_workspaceManager.TryGetCodeFixes(request.TextDocument.Uri, out var fixes, cancellationToken: cancellationToken))
                return new CommandOrCodeActionContainer();
            if (!_workspaceManager.TryGetRefactorings(request.TextDocument.Uri, selectionSpan, out var refactorings, cancellationToken))
                return new CommandOrCodeActionContainer();

            var filteredFixes = fixes
                .Where(fix => IsFixInRequestedRange(fix, request.Range, documentText))
                .Where(fix => MatchesRequestedDiagnostics(fix, request.Context?.Diagnostics, documentText))
                .ToArray();
            var filteredRefactorings = SupportsKind(request.Context?.Only, CodeActionKind.RefactorRewrite)
                ? refactorings.ToArray()
                : [];

            var actions = new List<CommandOrCodeAction>(filteredFixes.Length + filteredRefactorings.Length + 1);

            var syntaxTree = await document.GetSyntaxTreeAsync(cancellationToken).ConfigureAwait(false);
            if (syntaxTree is not null &&
                SupportsKind(request.Context?.Only, CodeActionKind.RefactorRewrite) &&
                compilation.SyntaxTrees.Contains(syntaxTree))
            {
                var semanticModel = compilation.GetSemanticModel(syntaxTree);
                var root = syntaxTree.GetRoot(cancellationToken);
                if (TryCreateMacroExpansionAction(request.TextDocument.Uri, documentText, semanticModel, root, request.Range, out var macroAction))
                    actions.Add(macroAction);
            }

            if (SupportsKind(request.Context?.Only, CodeActionKind.QuickFix))
            {
                foreach (var fix in filteredFixes)
                {
                    var action = await TryCreateLspCodeActionAsync(
                        request.TextDocument.Uri,
                        document,
                        fix.DocumentId,
                        fix.Action,
                        CodeActionKind.QuickFix,
                        fix.Diagnostic,
                        cancellationToken).ConfigureAwait(false);
                    if (action is not null)
                        actions.Add(action);

                    var preview = await TryCreatePreviewCommandAsync(
                        request.TextDocument.Uri,
                        document,
                        fix.DocumentId,
                        fix.Action,
                        cancellationToken).ConfigureAwait(false);
                    if (preview is not null)
                        actions.Add(preview);
                }
            }

            foreach (var refactoring in filteredRefactorings)
            {
                var action = await TryCreateLspCodeActionAsync(
                    request.TextDocument.Uri,
                    document,
                    refactoring.DocumentId,
                    refactoring.Action,
                    CodeActionKind.RefactorRewrite,
                    diagnostic: null,
                    cancellationToken).ConfigureAwait(false);
                if (action is not null)
                    actions.Add(action);

                var preview = await TryCreatePreviewCommandAsync(
                    request.TextDocument.Uri,
                    document,
                    refactoring.DocumentId,
                    refactoring.Action,
                    cancellationToken).ConfigureAwait(false);
                if (preview is not null)
                    actions.Add(preview);
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

    private static TextSpan GetRequestedSpan(SourceText text, LspRange requestRange)
    {
        var start = PositionHelper.ToOffset(text, requestRange.Start);
        var end = PositionHelper.ToOffset(text, requestRange.End);
        if (end < start)
            (start, end) = (end, start);

        return new TextSpan(start, end - start);
    }

    private static bool SupportsKind(Container<CodeActionKind>? requestedKinds, CodeActionKind kind)
    {
        if (requestedKinds is null || !requestedKinds.Any())
            return true;

        var kindText = kind.ToString();
        return requestedKinds.Any(requestedKind =>
        {
            var requestedText = requestedKind.ToString();
            return kindText.StartsWith(requestedText, StringComparison.OrdinalIgnoreCase);
        });
    }

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
            Title = $"Show macro expansion for {display.InvocationDisplay}",
            Arguments = new JArray(uri.ToString(), display.MacroName, display.FullText)
        });

        return true;
    }

    private static async Task<CommandOrCodeAction?> TryCreateLspCodeActionAsync(
        DocumentUri uri,
        Document currentDocument,
        DocumentId documentId,
        CodeFixAction action,
        CodeActionKind kind,
        CodeDiagnostic? diagnostic,
        CancellationToken cancellationToken)
    {
        var currentSolution = currentDocument.Project.Solution;
        var updatedSolution = action.GetChangedSolution(currentSolution, cancellationToken);
        if (updatedSolution.Version == currentSolution.Version)
            return null;

        var updatedDocument = updatedSolution.GetDocument(documentId);
        if (updatedDocument is null)
            return null;

        var currentDocumentForFix = currentSolution.GetDocument(documentId);
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

        var codeAction = diagnostic is null
            ? new LspCodeAction
            {
                Title = action.Title,
                Kind = kind,
                Edit = workspaceEdit
            }
            : new LspCodeAction
            {
                Title = action.Title,
                Kind = kind,
                Edit = workspaceEdit,
                Diagnostics = new Container<LspDiagnostic>(
                    new LspDiagnostic
                    {
                        Message = diagnostic.GetMessage(),
                        Source = "raven",
                        Code = diagnostic.Id,
                        Range = PositionHelper.ToRange(currentText, diagnostic.Location.SourceSpan)
                    })
            };

        return new CommandOrCodeAction(codeAction);
    }

    private static async Task<CommandOrCodeAction?> TryCreatePreviewCommandAsync(
        DocumentUri uri,
        Document currentDocument,
        DocumentId documentId,
        CodeFixAction action,
        CancellationToken cancellationToken)
    {
        var currentSolution = currentDocument.Project.Solution;
        var updatedSolution = action.GetChangedSolution(currentSolution, cancellationToken);
        if (updatedSolution.Version == currentSolution.Version)
            return null;

        var updatedDocument = updatedSolution.GetDocument(documentId);
        var currentDocumentForAction = currentSolution.GetDocument(documentId);
        if (updatedDocument is null || currentDocumentForAction is null)
            return null;

        var currentText = await currentDocumentForAction.GetTextAsync(cancellationToken).ConfigureAwait(false);
        var updatedText = await updatedDocument.GetTextAsync(cancellationToken).ConfigureAwait(false);
        if (string.Equals(currentText.ToString(), updatedText.ToString(), StringComparison.Ordinal))
            return null;

        return new CommandOrCodeAction(new Command
        {
            Name = ShowCodeActionPreviewCommand,
            Title = $"Preview: {action.Title}",
            Arguments = new JArray(uri.ToString(), action.Title, currentText.ToString(), updatedText.ToString())
        });
    }
}
