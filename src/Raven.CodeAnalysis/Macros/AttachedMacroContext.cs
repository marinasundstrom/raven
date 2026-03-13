using System;
using System.Threading;

using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Macros;

public sealed class AttachedMacroContext
{
    public AttachedMacroContext(
        Compilation compilation,
        SemanticModel semanticModel,
        AttributeSyntax syntax,
        SyntaxNode targetDeclaration,
        CancellationToken cancellationToken = default)
    {
        Compilation = compilation ?? throw new ArgumentNullException(nameof(compilation));
        SemanticModel = semanticModel ?? throw new ArgumentNullException(nameof(semanticModel));
        Syntax = syntax ?? throw new ArgumentNullException(nameof(syntax));
        TargetDeclaration = targetDeclaration ?? throw new ArgumentNullException(nameof(targetDeclaration));
        CancellationToken = cancellationToken;
    }

    public Compilation Compilation { get; }

    public SemanticModel SemanticModel { get; }

    public AttributeSyntax Syntax { get; }

    public SyntaxNode TargetDeclaration { get; }

    public CancellationToken CancellationToken { get; }
}
