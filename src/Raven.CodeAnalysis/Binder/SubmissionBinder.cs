using System.Collections.Generic;
using System.Collections.Immutable;

using Raven.CodeAnalysis.Symbols;

namespace Raven.CodeAnalysis;

/// <summary>
/// Places declarations from earlier script submissions in scope.
/// </summary>
/// <remarks>
/// This binder is deliberately separate from <see cref="TopLevelBinder"/>. Earlier
/// submission declarations are persistent script state, not locals declared by the
/// current top-level method. Their runtime representation is supplied by submission
/// lowering rather than by the lexical local scope.
/// </remarks>
internal sealed class SubmissionBinder : Binder
{
    private readonly ImmutableArray<ISymbol> _declarations;

    internal SubmissionBinder(Binder parent, Compilation compilation)
        : base(parent)
    {
        _declarations = compilation.GetPreviousSubmissionDeclarations();
    }

    internal IEnumerable<ISymbol> LookupSubmissionSymbols(string name)
    {
        foreach (var declaration in _declarations)
        {
            if (declaration.Name == name)
                yield return declaration;
        }
    }

    internal ImmutableArray<ISymbol> GetSubmissionSymbols()
        => _declarations;
}
