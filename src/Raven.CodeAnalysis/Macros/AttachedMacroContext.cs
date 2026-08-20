using System;
using System.Collections.Immutable;
using System.Threading;

using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Macros;

public class AttachedMacroContext : MacroContext
{
    public AttachedMacroContext(
        Compilation compilation,
        SemanticModel semanticModel,
        AttributeSyntax syntax,
        SyntaxNode targetDeclaration,
        SyntaxNode currentDeclaration,
        CancellationToken cancellationToken = default)
        : base(syntax ?? throw new ArgumentNullException(nameof(syntax)))
    {
        Compilation = compilation ?? throw new ArgumentNullException(nameof(compilation));
        SemanticModel = semanticModel ?? throw new ArgumentNullException(nameof(semanticModel));
        Syntax = syntax;
        TargetDeclaration = targetDeclaration ?? throw new ArgumentNullException(nameof(targetDeclaration));
        CurrentDeclaration = currentDeclaration ?? throw new ArgumentNullException(nameof(currentDeclaration));
        Arguments = CreateArguments(syntax.ArgumentList, semanticModel);
        CancellationToken = cancellationToken;
    }

    public Compilation Compilation { get; }

    public SemanticModel SemanticModel { get; }

    public AttributeSyntax Syntax { get; }

    public ArgumentListSyntax? ArgumentList => Syntax.ArgumentList;

    public ImmutableArray<MacroArgument> Arguments { get; }

    public SyntaxNode TargetDeclaration { get; }

    public SyntaxNode CurrentDeclaration { get; }

    public CancellationToken CancellationToken { get; }

    public override MacroExpansionDiagnostic CreateDiagnostic(
        string message,
        DiagnosticSeverity severity = DiagnosticSeverity.Error,
        SyntaxNode? syntax = null,
        string? code = null)
        => new(severity, message, syntax?.GetLocation() ?? Syntax.Name.GetLocation(), code);

    public MacroExpansionDiagnostic CreateArgumentDiagnostic(
        MacroArgument argument,
        string message,
        DiagnosticSeverity severity = DiagnosticSeverity.Error,
        string? code = null)
    {
        ArgumentNullException.ThrowIfNull(argument);
        return new MacroExpansionDiagnostic(severity, message, argument.Syntax.GetLocation(), code);
    }

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
