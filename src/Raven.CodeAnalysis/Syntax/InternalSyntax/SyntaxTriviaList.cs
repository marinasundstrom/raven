﻿using System.Collections;
using System.Formats.Asn1;

namespace Raven.CodeAnalysis.Syntax.InternalSyntax;

internal class SyntaxTriviaList : GreenNode, IEnumerable<SyntaxTrivia>
{
    public readonly static SyntaxTriviaList Empty = new SyntaxTriviaList([]);
    private readonly SyntaxTrivia[] _trivias;

    public SyntaxTriviaList(SyntaxTrivia[] trivias,
        IEnumerable<DiagnosticInfo>? diagnostics = null)
        : base(SyntaxKind.List, trivias?.Length ?? 0, diagnostics)
    {
        _trivias = trivias ?? Array.Empty<SyntaxTrivia>();

        Width = CalculateFullWidth(trivias);
        FullWidth = CalculateFullWidth(trivias);
    }

    public override GreenNode GetSlot(int index)
    {
        if (index < 0 || index >= SlotCount)
            throw new IndexOutOfRangeException($"Invalid slot index: {index}");
        return _trivias[index];
    }

    private static int CalculateFullWidth(SyntaxTrivia[] trivias) =>
        trivias?.Sum(trivia => trivia.FullWidth) ?? 0;


    /// <summary>
    /// Creates a new SyntaxTriviaList with the specified trivia added at the end.
    /// </summary>
    public SyntaxTriviaList Add(SyntaxTrivia trivia)
    {
        if (trivia == null) throw new ArgumentNullException(nameof(trivia));

        var newTrivias = _trivias.Concat(new[] { trivia }).ToArray();
        return new SyntaxTriviaList(newTrivias);
    }

    /// <summary>
    /// Creates a new SyntaxTriviaList with the specified trivia removed.
    /// </summary>
    public SyntaxTriviaList Remove(SyntaxTrivia trivia)
    {
        if (trivia == null) throw new ArgumentNullException(nameof(trivia));

        var newTrivias = _trivias.Where(t => !Equals(t, trivia)).ToArray();
        return new SyntaxTriviaList(newTrivias);
    }

    /// <summary>
    /// Creates a new SyntaxTriviaList from the given array of SyntaxTrivia.
    /// </summary>
    public static SyntaxTriviaList Create(SyntaxTrivia[] trivias)
    {
        if (trivias == null) throw new ArgumentNullException(nameof(trivias));
        return new SyntaxTriviaList(trivias);
    }

    public IEnumerator<SyntaxTrivia> GetEnumerator()
    {
        return _trivias.OfType<SyntaxTrivia>().GetEnumerator();
    }

    IEnumerator IEnumerable.GetEnumerator()
    {
        return GetEnumerator();
    }

    protected override GreenNode WithUpdatedChildren(GreenNode[] newChildren)
    {
        return new SyntaxList(newChildren);
    }

    internal override GreenNode WithDiagnostics(params DiagnosticInfo[] diagnostics)
    {
        return new SyntaxList(_trivias, _diagnostics);
    }

    internal override IEnumerable<DiagnosticInfo> GetDiagnosticsRecursive()
    {
        foreach (var child in GetChildren())
        {
            foreach (var diagnostic in child.GetDiagnosticsRecursive())
            {
                yield return diagnostic;
            }
        }
    }
}

internal static partial class SyntaxFactory
{
    public static SyntaxTriviaList TriviaList(
        SyntaxTrivia[] trivias,
        IEnumerable<DiagnosticInfo>? diagnostics = null)
      => new(trivias, diagnostics);
}
