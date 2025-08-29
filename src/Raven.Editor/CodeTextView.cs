using System.Collections.Generic;
using System.Linq;

using NStack;

using Raven.CodeAnalysis;
using Raven.CodeAnalysis.Diagnostics;
using Raven.CodeAnalysis.Syntax;
using Raven.CodeAnalysis.Text;

using Terminal.Gui;

using Attribute = Terminal.Gui.Attribute;

namespace Raven.Editor;

/// <summary>
/// Simple text view with Raven-based syntax highlighting.
/// </summary>
public class CodeTextView : TextView
{
    private readonly RavenWorkspace _workspace;
    private readonly ProjectId _projectId;
    private readonly DocumentId _documentId;

    private readonly Dictionary<List<Rune>, LineInfo> _lineInfos = new();
    private readonly Dictionary<string, Queue<LineInfo>> _lineInfoCache = new();

    public CodeTextView(RavenWorkspace workspace, ProjectId projectId, DocumentId documentId)
    {
        _workspace = workspace;
        _projectId = projectId;
        _documentId = documentId;
    }

    private record struct TokenSpan(int Start, int End, SemanticClassification Classification);
    private record struct DiagnosticSpan(int Start, int End, DiagnosticSeverity Severity);
    private record struct LineInfo(List<TokenSpan> Tokens, List<DiagnosticSpan> Diagnostics);


    /// <inheritdoc />
    public override void OnContentsChanged()
    {
        _lineInfos.Clear();
        _lineInfoCache.Clear();

        var text = Text?.ToString() ?? string.Empty;
        var sourceText = SourceText.From(text);
        var solution = _workspace.CurrentSolution.WithDocumentText(_documentId, sourceText);
        _workspace.TryApplyChanges(solution);

        var project = solution.GetProject(_projectId)!;
        var document = project.GetDocument(_documentId)!;
        var tree = document.GetSyntaxTreeAsync().Result!;
        var compilation = _workspace.GetCompilation(_projectId);
        var model = compilation.GetSemanticModel(tree);
        var classification = SemanticClassifier.Classify(tree.GetRoot(), model);

        var lines = text.Replace("\r\n", "\n").Replace("\r", "\n").Split('\n');
        var lineTokens = new List<TokenSpan>[lines.Length];
        var lineDiagnostics = new List<DiagnosticSpan>[lines.Length];

        foreach (var kvp in classification.Tokens)
        {
            if (kvp.Value == SemanticClassification.Default)
                continue;
            AddTokenSpan(lineTokens, lines, sourceText, kvp.Key.Span, kvp.Value);
        }

        foreach (var kvp in classification.Trivia)
        {
            if (kvp.Value == SemanticClassification.Default)
                continue;
            AddTokenSpan(lineTokens, lines, sourceText, kvp.Key.Span, kvp.Value);
        }

        foreach (var diagnostic in _workspace.GetDiagnostics(_projectId)
                     .Where(d => d.Location.SourceTree == tree))
        {
            AddDiagnosticSpan(lineDiagnostics, lines, sourceText, diagnostic.Location.SourceSpan.Start,
                diagnostic.Location.SourceSpan.End, diagnostic.Severity);
        }

        for (int i = 0; i < lines.Length; i++)
        {
            var info = new LineInfo(lineTokens[i] ?? new(), lineDiagnostics[i] ?? new());
            if (!_lineInfoCache.TryGetValue(lines[i], out var queue))
            {
                queue = new Queue<LineInfo>();
                _lineInfoCache[lines[i]] = queue;
            }
            queue.Enqueue(info);
        }

        base.OnContentsChanged();
    }

    private static void AddTokenSpan(List<TokenSpan>[] lineTokens, string[] lines, SourceText text, TextSpan span,
        SemanticClassification classification)
    {
        var (startLine1, startCol1) = text.GetLineAndColumn(span);
        var (endLine1, endCol1) = text.GetLineAndColumn(new TextSpan(span.End, 0));
        var startLine = startLine1 - 1;
        var startCol = startCol1 - 1;
        var endLine = endLine1 - 1;
        var endCol = endCol1 - 1;
        for (var line = startLine; line <= endLine; line++)
        {
            var start = line == startLine ? startCol : 0;
            var end = line == endLine ? endCol : lines[line].Length;
            var list = lineTokens[line] ??= new List<TokenSpan>();
            list.Add(new TokenSpan(start, end, classification));
        }
    }

    private static void AddDiagnosticSpan(List<DiagnosticSpan>[] lineDiagnostics, string[] lines, SourceText text,
        int startPos, int endPos, DiagnosticSeverity severity)
    {
        var (startLine1, startCol1) = text.GetLineAndColumn(new TextSpan(startPos, 0));
        var (endLine1, endCol1) = text.GetLineAndColumn(new TextSpan(endPos, 0));
        var startLine = startLine1 - 1;
        var startCol = startCol1 - 1;
        var endLine = endLine1 - 1;
        var endCol = endCol1 - 1;
        for (var line = startLine; line <= endLine; line++)
        {
            var start = line == startLine ? startCol : 0;
            var end = line == endLine ? endCol : lines[line].Length;
            var list = lineDiagnostics[line] ??= new List<DiagnosticSpan>();
            list.Add(new DiagnosticSpan(start, end, severity));
        }
    }

    /// <inheritdoc />
    protected override void SetNormalColor(List<Rune> line, int idx)
    {
        if (!_lineInfos.TryGetValue(line, out var info))
        {
            var lineText = new string(line.Select(r => (char)r).ToArray());
            if (_lineInfoCache.TryGetValue(lineText, out var queue) && queue.Count > 0)
            {
                info = queue.Dequeue();
            }
            else
            {
                info = new LineInfo(new(), new());
            }

            _lineInfos[line] = info;
        }

        foreach (var span in info.Diagnostics)
        {
            if (idx >= span.Start && idx < span.End)
            {
                var attr = GetAttribute(span.Severity);
                Driver.SetAttribute(attr);
                return;
            }
        }

        foreach (var span in info.Tokens)
        {
            if (idx >= span.Start && idx < span.End)
            {
                Driver.SetAttribute(GetAttribute(span.Classification));
                return;
            }
        }

        base.SetNormalColor(line, idx);
    }

    private Attribute GetAttribute(SemanticClassification classification)
    {
        var scheme = ColorScheme;
        var attr = HasFocus ? scheme.Focus : scheme.Normal;
        var background = attr.Background;

        return classification switch
        {
            SemanticClassification.Keyword => new(Color.BrightBlue, background),
            SemanticClassification.StringLiteral => new(Color.BrightGreen, background),
            SemanticClassification.NumericLiteral => new(Color.BrightYellow, background),
            SemanticClassification.Comment => new(Color.Green, background),
            SemanticClassification.Method => new(Color.BrightYellow, background),
            SemanticClassification.Type => new(Color.Magenta, background),
            SemanticClassification.Namespace => new(Color.BrightCyan, background),
            SemanticClassification.Field => new(Color.Cyan, background),
            SemanticClassification.Parameter => new(Color.Blue, background),
            _ => new(Color.BrightBlue, background)
        };
    }

    private Attribute GetAttribute(DiagnosticSeverity severity)
    {
        var scheme = ColorScheme;
        var attr = HasFocus ? scheme.Focus : scheme.Normal;
        var background = attr.Background;

        return severity switch
        {
            DiagnosticSeverity.Error => new(Color.BrightRed, background),
            DiagnosticSeverity.Warning => new(Color.BrightYellow, background),
            _ => new(Color.BrightBlue, background)
        };
    }
}
