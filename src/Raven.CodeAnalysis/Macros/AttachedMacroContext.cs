using System;
using System.Collections.Immutable;
using System.Threading;

using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Macros;

public class AttachedMacroContext
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
        Arguments = CreateArguments(syntax.ArgumentList, semanticModel);
        CancellationToken = cancellationToken;
    }

    public Compilation Compilation { get; }

    public SemanticModel SemanticModel { get; }

    public AttributeSyntax Syntax { get; }

    public ArgumentListSyntax? ArgumentList => Syntax.ArgumentList;

    public ImmutableArray<MacroArgument> Arguments { get; }

    public SyntaxNode TargetDeclaration { get; }

    public CancellationToken CancellationToken { get; }

    private static ImmutableArray<MacroArgument> CreateArguments(ArgumentListSyntax? argumentList, SemanticModel semanticModel)
    {
        if (argumentList is null)
            return ImmutableArray<MacroArgument>.Empty;

        var builder = ImmutableArray.CreateBuilder<MacroArgument>(argumentList.Arguments.Count);
        foreach (var argument in argumentList.Arguments)
            builder.Add(new MacroArgument(argument, semanticModel));

        return builder.MoveToImmutable();
    }
}

public sealed class AttachedMacroContext<TParameters> : AttachedMacroContext
    where TParameters : class
{
    public AttachedMacroContext(
        Compilation compilation,
        SemanticModel semanticModel,
        AttributeSyntax syntax,
        SyntaxNode targetDeclaration,
        TParameters parameters,
        CancellationToken cancellationToken = default)
        : base(compilation, semanticModel, syntax, targetDeclaration, cancellationToken)
    {
        Parameters = parameters ?? throw new ArgumentNullException(nameof(parameters));
    }

    public TParameters Parameters { get; }
}
