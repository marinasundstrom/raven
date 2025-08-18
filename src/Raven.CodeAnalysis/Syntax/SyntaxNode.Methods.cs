using System.Net.Mail;

namespace Raven.CodeAnalysis.Syntax;

public partial class SyntaxNode
{
    public SyntaxToken GetFirstToken(bool includeZeroWidth = false)
    {
        return DescendantTokens().FirstOrDefault(t => includeZeroWidth || t.FullWidth > 0);
    }

    public SyntaxToken GetLastToken(bool includeZeroWidth = false)
    {
        foreach (var token in DescendantTokens(descendIntoTrivia: false).Reverse())
        {
            if (includeZeroWidth || token.Width > 0)
                return token;
        }

        return default;
    }

    public SyntaxNode FindNode(TextSpan span,
                              bool findInsideTrivia = false,
                              bool getInnermostNodeForTie = false)
    {
        if (!FullSpan.IntersectsWith(span))
            return null!;

        SyntaxNode? bestMatchFromChildren = null;
        foreach (var child in ChildNodes())
        {
            if (!child.FullSpan.Contains(span))
                continue;

            var childMatch = child.FindNode(span, findInsideTrivia, getInnermostNodeForTie);
            if (childMatch is not null)
            {
                // If we don't already have a match, take it.
                if (bestMatchFromChildren is null)
                {
                    bestMatchFromChildren = childMatch;
                }
                else
                {
                    // Both matches presumably have the same span; handle tie-breaking:
                    if (getInnermostNodeForTie)
                    {
                        // The childMatch is presumably "inner", so prefer the childMatch.
                        bestMatchFromChildren = childMatch;
                    }
                    else
                    {
                        // Keep the existing match for "outermost" behavior.
                    }
                }
            }
        }

        if (bestMatchFromChildren is not null)
            return bestMatchFromChildren;

        if (Span.Contains(span))
        {
            return this;
        }

        return null!;
    }

    public SyntaxToken FindToken(int position)
    {
        if (position < Position || position > this.FullSpan.End)
            throw new ArgumentOutOfRangeException(nameof(position), "Position is out of bounds of this syntax tree.");

        return FindTokenInternal(this, position);
    }

    public SyntaxTrivia FindTrivia(int position, bool descendIntoTrivia = false)
    {
        foreach (var token in DescendantTokens(descendIntoTrivia))
        {
            foreach (var trivia in token.LeadingTrivia)
            {
                if (trivia.FullSpan.Contains(position))
                    return trivia;

                if (descendIntoTrivia && trivia.HasStructure)
                {
                    var result = trivia.GetStructure().FindTrivia(position, true);
                    //if (result.RawKind != 0)
                    return result;
                }
            }

            if (token.FullSpan.Contains(position))
            {
                // Could be inside the token
                return default; // or throw, or define "no trivia here"
            }

            foreach (var trivia in token.TrailingTrivia)
            {
                if (trivia.FullSpan.Contains(position))
                    return trivia;

                if (descendIntoTrivia && trivia.HasStructure)
                {
                    var result = trivia.GetStructure().FindTrivia(position, true);
                    //if (result.RawKind != 0)
                    return result;
                }
            }
        }

        return default;
    }

    private static SyntaxToken FindTokenInternal(SyntaxNode node, int position)
    {
        foreach (var child in node.ChildNodesAndTokens())
        {
            if (child.IsToken)
            {
                var token = child.AsToken();
                if (token.FullSpan.Contains(position))
                    return token;
            }
            else
            {
                var childNode = child.AsNode()!;
                if (position >= childNode.Position && position < childNode.End)
                    return FindTokenInternal(childNode, position);
            }
        }

        // If not found (e.g., position == EndOfFile), return EOF token
        return node is CompilationUnitSyntax cu ? cu.EndOfFileToken : default;
    }

    public T? FirstAncestorOrSelf<T>() where T : SyntaxNode
    {
        var current = this;
        while (current != null)
        {
            if (current is T match)
                return match;
            current = current.Parent;
        }
        return null;
    }

    /// <summary>
    /// Retrieves all descendant nodes recursively.
    /// </summary>
    public IEnumerable<SyntaxNode> DescendantNodes()
    {
        foreach (var child in ChildNodes())
        {
            yield return child;

            foreach (var descendant in child.DescendantNodes())
                yield return descendant;
        }
    }

    public IEnumerable<SyntaxNode> DescendantNodesAndSelf()
    {
        yield return this;

        foreach (var child in ChildNodes())
        {
            foreach (var descendant in child.DescendantNodesAndSelf())
            {
                yield return descendant;
            }
        }
    }

    public IEnumerable<SyntaxNodeOrToken> DescendantNodesAndTokens(bool descendIntoTrivia = false)
    {
        foreach (var child in ChildNodesAndTokens())
        {
            yield return child;

            if (child.IsNode)
            {
                foreach (var descendant in child.AsNode()!.DescendantNodesAndTokens(descendIntoTrivia))
                {
                    yield return descendant;
                }
            }
        }
    }

    public IEnumerable<SyntaxNodeOrToken> DescendantNodesAndTokensAndSelf(bool descendIntoTrivia = false)
    {
        yield return new SyntaxNodeOrToken(this);

        foreach (var descendant in DescendantNodesAndTokens(descendIntoTrivia))
            yield return descendant;
    }

    public IEnumerable<SyntaxToken> DescendantTokens(bool descendIntoTrivia = false)
    {
        foreach (var child in ChildNodesAndTokens())
        {
            if (child.IsToken)
            {
                var token = child.AsToken();
                yield return token;

                if (descendIntoTrivia)
                {
                    foreach (var trivia in token.LeadingTrivia)
                    {
                        if (trivia.HasStructure)
                        {
                            foreach (var t in trivia.GetStructure().DescendantTokens(true))
                                yield return t;
                        }
                    }

                    foreach (var trivia in token.TrailingTrivia)
                    {
                        if (trivia.HasStructure)
                        {
                            foreach (var t in trivia.GetStructure().DescendantTokens(true))
                                yield return t;
                        }
                    }
                }
            }
            else
            {
                var node = child.AsNode()!;
                foreach (var token in node.DescendantTokens(descendIntoTrivia))
                    yield return token;
            }
        }
    }

    public IEnumerable<SyntaxTrivia> DescendantTrivia(bool descendIntoStructuredTrivia = false)
    {
        foreach (var token in DescendantTokens(descendIntoStructuredTrivia))
        {
            foreach (var trivia in token.LeadingTrivia)
            {
                yield return trivia;
                if (descendIntoStructuredTrivia && trivia.HasStructure)
                {
                    foreach (var t in trivia.GetStructure().DescendantTrivia(true))
                        yield return t;
                }
            }

            foreach (var trivia in token.TrailingTrivia)
            {
                yield return trivia;
                if (descendIntoStructuredTrivia && trivia.HasStructure)
                {
                    foreach (var t in trivia.GetStructure().DescendantTrivia(true))
                        yield return t;
                }
            }
        }
    }

    /// <summary>
    /// Retrieves all ancestor nodes up to the root.
    /// </summary>
    public IEnumerable<SyntaxNode> Ancestors()
    {
        var current = _parent;
        while (current != null)
        {
            yield return current;
            current = current._parent;
        }
    }

    public IEnumerable<SyntaxNode> AncestorsAndSelf()
    {
        var current = this;
        while (current is not null)
        {
            yield return current;
            current = current.Parent;
        }
    }

    public bool Contains(SyntaxNode node)
    {
        var current = node;
        while (current != null)
        {
            if (current == this)
                return true;
            current = current.Parent;
        }
        return false;
    }

    public SyntaxNode ReplaceNodes<T>(
        IEnumerable<T> nodes,
        Func<T, SyntaxNode, SyntaxNode?> computeReplacement) where T : SyntaxNode
    {
        var map = nodes.ToDictionary(n => (SyntaxNode)n, n => computeReplacement(n, n));
        return ReplaceSyntax(map, null, null);
    }

    public SyntaxNode ReplaceTokens(
        IEnumerable<SyntaxToken> tokens,
        Func<SyntaxToken, SyntaxToken, SyntaxToken?> computeReplacement)
    {
        var map = tokens.ToDictionary(t => (SyntaxToken)t, t => computeReplacement(t, t));
        return ReplaceSyntax(null, map, null);
    }

    public SyntaxNode ReplaceTrivia<T>(
        IEnumerable<SyntaxTrivia> trivia,
        Func<SyntaxTrivia, SyntaxTrivia, SyntaxTrivia?> computeReplacement)
    {
        var map = trivia.ToDictionary(t => (SyntaxTrivia)t, t => computeReplacement(t, t));
        return ReplaceSyntax(null, null, map);
    }

    public SyntaxNode ReplaceSyntax(
        IReadOnlyDictionary<SyntaxNode, SyntaxNode?>? nodeMap,
        IReadOnlyDictionary<SyntaxToken, SyntaxToken?>? tokenMap,
        IReadOnlyDictionary<SyntaxTrivia, SyntaxTrivia?>? triviaMap)
    {
        var replacer = new SyntaxReplacer(nodeMap, tokenMap, triviaMap);
        return replacer.Visit(this);
    }
}
