using System;
using System.Collections.Immutable;
using System.Threading;

using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Macros;

public class FreestandingMacroContext : MacroContext
{
    private readonly ImmutableArray<MacroFileDependency>.Builder _fileDependencies =
        ImmutableArray.CreateBuilder<MacroFileDependency>();

    public FreestandingMacroContext(
        Compilation compilation,
        SemanticModel semanticModel,
        FreestandingMacroExpressionSyntax syntax,
        CancellationToken cancellationToken = default)
        : this(compilation, semanticModel, FreestandingMacroInvocation.Create(syntax), cancellationToken)
    {
    }

    public FreestandingMacroContext(
        Compilation compilation,
        SemanticModel semanticModel,
        FreestandingMacroMemberDeclarationSyntax syntax,
        CancellationToken cancellationToken = default)
        : this(compilation, semanticModel, FreestandingMacroInvocation.Create(syntax), cancellationToken)
    {
    }

    public FreestandingMacroContext(
        Compilation compilation,
        SemanticModel semanticModel,
        FreestandingMacroDeclarationSyntax syntax,
        CancellationToken cancellationToken = default)
        : this(compilation, semanticModel, FreestandingMacroInvocation.Create(syntax), cancellationToken)
    {
    }

    internal FreestandingMacroContext(
        Compilation compilation,
        SemanticModel semanticModel,
        FreestandingMacroInvocation invocation,
        CancellationToken cancellationToken = default)
        : base(invocation.Syntax)
    {
        Compilation = compilation ?? throw new ArgumentNullException(nameof(compilation));
        SemanticModel = semanticModel ?? throw new ArgumentNullException(nameof(semanticModel));
        Syntax = invocation.Syntax;
        Invocation = invocation;
        Name = invocation.Name;
        ExclamationToken = invocation.ExclamationToken;
        ArgumentList = invocation.ArgumentList;
        TokenTree = invocation.TokenTree;
        Arguments = CreateArguments(invocation.ArgumentList, semanticModel);
        CancellationToken = cancellationToken;
    }

    public Compilation Compilation { get; }

    public SemanticModel SemanticModel { get; }

    public SyntaxNode Syntax { get; }

    public NameSyntax Name { get; }

    public SyntaxToken ExclamationToken { get; }

    public ArgumentListSyntax? ArgumentList { get; }

    public MacroTokenTreeSyntax? TokenTree { get; }

    public ImmutableArray<MacroArgument> Arguments { get; }

    public CancellationToken CancellationToken { get; }

    internal FreestandingMacroInvocation Invocation { get; }

    public override MacroExpansionDiagnostic CreateDiagnostic(
        string message,
        DiagnosticSeverity severity = DiagnosticSeverity.Error,
        SyntaxNode? syntax = null,
        string? code = null)
        => new(severity, message, syntax?.GetLocation() ?? Name.GetLocation(), code);

    public MacroExpansionDiagnostic CreateArgumentDiagnostic(
        MacroArgument argument,
        string message,
        DiagnosticSeverity severity = DiagnosticSeverity.Error,
        string? code = null)
    {
        ArgumentNullException.ThrowIfNull(argument);
        return new MacroExpansionDiagnostic(severity, message, argument.Syntax.GetLocation(), code);
    }

    internal MacroFileReadResult ReadFile(string path)
        => MacroFileReader.Read(Syntax, path, _fileDependencies);

    internal ImmutableArray<MacroFileDependency> GetFileDependencies()
        => _fileDependencies.ToImmutable();

    internal void AddFileDependencies(IEnumerable<MacroFileDependency> dependencies)
        => _fileDependencies.AddRange(dependencies);

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
