using NStack;
using Raven.CodeAnalysis;
using Raven.CodeAnalysis.Diagnostics;
using Raven.CodeAnalysis.Syntax;
using Raven.CodeAnalysis.Text;
using System.Collections.Generic;
using System.Linq;
using Terminal.Gui;
using Attribute = Terminal.Gui.Attribute;

namespace Raven.Editor;

/// <summary>
/// Simple text view with Raven-based syntax highlighting.
/// </summary>
public class CodeTextView : TextView
{
    private readonly Dictionary<List<Rune>, LineInfo> _lineInfos = new();
    private readonly Dictionary<string, Queue<LineInfo>> _lineInfoCache = new();

    private record struct TokenSpan(int Start, int End, SemanticClassification Classification);
    private record struct DiagnosticSpan(int Start, int End, DiagnosticSeverity Severity);
    private record struct LineInfo(List<TokenSpan> Tokens, List<DiagnosticSpan> Diagnostics);

    private static readonly Attribute KeywordAttr = new(Color.BrightBlue, Color.Black);
    private static readonly Attribute NumberAttr = new(Color.BrightYellow, Color.Black);
    private static readonly Attribute StringAttr = new(Color.BrightGreen, Color.Black);
    private static readonly Attribute CommentAttr = new(Color.Green, Color.Black);
    private static readonly Attribute MethodAttr = new(Color.BrightYellow, Color.Black);
    private static readonly Attribute TypeAttr = new(Color.Magenta, Color.Black);
    private static readonly Attribute NamespaceAttr = new(Color.BrightCyan, Color.Black);
    private static readonly Attribute FieldAttr = new(Color.Cyan, Color.Black);
    private static readonly Attribute ParameterAttr = new(Color.Blue, Color.Black);
    private static readonly Attribute ErrorAttr = new(Color.BrightRed, Color.Black);
    private static readonly Attribute WarningAttr = new(Color.BrightYellow, Color.Black);
    private static readonly Attribute InfoAttr = new(Color.BrightBlue, Color.Black);

    /// <inheritdoc />
    public override void OnContentsChanged()
    {
        _lineInfos.Clear();
        _lineInfoCache.Clear();

        var text = Text?.ToString() ?? string.Empty;
        var sourceText = SourceText.From(text);
        var tree = SyntaxTree.ParseText(sourceText);
        var compilation = Compilation.Create("Editor", [tree]);
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

        foreach (var diagnostic in compilation.GetDiagnostics().Where(d => d.Location.SourceTree == tree))
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
                var attr = span.Severity switch
                {
                    DiagnosticSeverity.Error => ErrorAttr,
                    DiagnosticSeverity.Warning => WarningAttr,
                    _ => InfoAttr
                };
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

    private static Attribute GetAttribute(SemanticClassification classification) => classification switch
    {
        SemanticClassification.Keyword => KeywordAttr,
        SemanticClassification.StringLiteral => StringAttr,
        SemanticClassification.NumericLiteral => NumberAttr,
        SemanticClassification.Comment => CommentAttr,
        SemanticClassification.Method => MethodAttr,
        SemanticClassification.Type => TypeAttr,
        SemanticClassification.Namespace => NamespaceAttr,
        SemanticClassification.Field => FieldAttr,
        SemanticClassification.Parameter => ParameterAttr,
        _ => InfoAttr
    };
}
