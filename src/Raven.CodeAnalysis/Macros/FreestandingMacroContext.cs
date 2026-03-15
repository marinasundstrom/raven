using System;
using System.Collections.Immutable;
using System.Threading;

using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Macros;

public class FreestandingMacroContext
{
    public FreestandingMacroContext(
        Compilation compilation,
        SemanticModel semanticModel,
        FreestandingMacroExpressionSyntax syntax,
        CancellationToken cancellationToken = default)
    {
        Compilation = compilation ?? throw new ArgumentNullException(nameof(compilation));
        SemanticModel = semanticModel ?? throw new ArgumentNullException(nameof(semanticModel));
        Syntax = syntax ?? throw new ArgumentNullException(nameof(syntax));
        Arguments = CreateArguments(syntax.ArgumentList, semanticModel);
        CancellationToken = cancellationToken;
    }

    public Compilation Compilation { get; }

    public SemanticModel SemanticModel { get; }

    public FreestandingMacroExpressionSyntax Syntax { get; }

    public ArgumentListSyntax ArgumentList => Syntax.ArgumentList;

    public ImmutableArray<MacroArgument> Arguments { get; }

    public CancellationToken CancellationToken { get; }

    public MacroExpansionDiagnostic CreateDiagnostic(
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

    private static ImmutableArray<MacroArgument> CreateArguments(ArgumentListSyntax argumentList, SemanticModel semanticModel)
    {
        var builder = ImmutableArray.CreateBuilder<MacroArgument>(argumentList.Arguments.Count);
        foreach (var argument in argumentList.Arguments)
            builder.Add(new MacroArgument(argument, semanticModel));

        return builder.MoveToImmutable();
    }
}

public sealed class FreestandingMacroContext<TParameters> : FreestandingMacroContext
    where TParameters : class
{
    public FreestandingMacroContext(
        Compilation compilation,
        SemanticModel semanticModel,
        FreestandingMacroExpressionSyntax syntax,
        TParameters parameters,
        CancellationToken cancellationToken = default)
        : base(compilation, semanticModel, syntax, cancellationToken)
    {
        Parameters = parameters ?? throw new ArgumentNullException(nameof(parameters));
    }

    public TParameters Parameters { get; }
}
