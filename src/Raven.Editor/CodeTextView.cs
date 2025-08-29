using NStack;
using Raven.CodeAnalysis;
using Raven.CodeAnalysis.Syntax;
using Raven.CodeAnalysis.Text;
using Terminal.Gui;
using Attribute = Terminal.Gui.Attribute;
using System.Collections.Generic;
using System.Linq;

namespace Raven.Editor;

/// <summary>
/// Simple text view with Raven-based syntax highlighting.
/// </summary>
public class CodeTextView : TextView
{
    private readonly Dictionary<List<Rune>, SyntaxToken[]> _lineTokens = new();

    private static readonly Attribute KeywordAttr = new(Color.BrightBlue, Color.Black);
    private static readonly Attribute NumberAttr = new(Color.BrightYellow, Color.Black);
    private static readonly Attribute StringAttr = new(Color.BrightGreen, Color.Black);

    /// <inheritdoc />
    public override void OnContentsChanged()
    {
        _lineTokens.Clear();
        base.OnContentsChanged();
    }

    /// <inheritdoc />
    protected override void SetNormalColor(List<Rune> line, int idx)
    {
        if (!_lineTokens.TryGetValue(line, out var tokens))
        {
            var text = new string(line.Select(r => (char)r).ToArray());
            var tree = SyntaxTree.ParseText(text);
            tokens = tree.GetRoot().DescendantTokens().ToArray();
            _lineTokens[line] = tokens;
        }

        foreach (var token in tokens)
        {
            var span = token.Span;
            if (idx >= span.Start && idx < span.End)
            {
                if (SyntaxFacts.IsReservedWordKind(token.Kind))
                {
                    Driver.SetAttribute(KeywordAttr);
                    return;
                }

                if (token.Kind == SyntaxKind.NumericLiteralToken)
                {
                    Driver.SetAttribute(NumberAttr);
                    return;
                }

                if (token.Kind == SyntaxKind.StringLiteralToken || token.Kind == SyntaxKind.CharacterLiteralToken)
                {
                    Driver.SetAttribute(StringAttr);
                    return;
                }

                break;
            }
        }

        base.SetNormalColor(line, idx);
    }
}
