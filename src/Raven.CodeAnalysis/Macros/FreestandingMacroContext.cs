using System;
using System.Collections.Immutable;
using System.Threading;

using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Macros;

public partial class FreestandingMacroContext : MacroContext
{
    private readonly ImmutableArray<MacroFileDependency>.Builder _fileDependencies =
        ImmutableArray.CreateBuilder<MacroFileDependency>();

    private readonly MacroArgumentServices _services;

    internal FreestandingMacroContext(
        MacroArgumentServices services,
        FreestandingMacroInvocation invocation,
        CancellationToken cancellationToken = default)
        : base(invocation.Syntax)
    {
        _services = services ?? throw new ArgumentNullException(nameof(services));
        Syntax = invocation.Syntax;
        Invocation = invocation;
        Name = invocation.Name;
        ExclamationToken = invocation.ExclamationToken;
        Carrier = invocation.Carrier;
        ArgumentList = invocation.ArgumentList;
        ExpressionArgument = invocation.ExpressionArgument;
        DeclarationHeader = invocation.DeclarationHeader;
        TokenTree = invocation.TokenTree;
        Arguments = CreateArguments(invocation, services);
        CancellationToken = cancellationToken;
    }

    public SyntaxNode Syntax { get; }

    public NameSyntax Name { get; }

    public SyntaxToken ExclamationToken { get; }

    public MacroCarrierSyntax Carrier { get; }

    public ArgumentListSyntax? ArgumentList { get; }

    public ExpressionSyntax? ExpressionArgument { get; }

    public MacroDeclarationHeaderSyntax? DeclarationHeader { get; }

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

    /// <summary>
    /// Reads a text file relative to the invoking source file and observes it
    /// as an input to the cached macro expansion.
    /// </summary>
    public MacroFileReadResult ReadFile(string path)
        => MacroFileReader.Read(Syntax, path, _fileDependencies);

    internal ImmutableArray<MacroFileDependency> GetFileDependencies()
        => _fileDependencies.ToImmutable();

    internal void AddFileDependencies(IEnumerable<MacroFileDependency> dependencies)
        => _fileDependencies.AddRange(dependencies);

    private static ImmutableArray<MacroArgument> CreateArguments(
        FreestandingMacroInvocation invocation,
        MacroArgumentServices services)
    {
        if (invocation.ExpressionArgument is { } expression)
            return [new MacroArgument(expression, services)];

        if (invocation.ArgumentList is not { } argumentList)
            return [];

        var builder = ImmutableArray.CreateBuilder<MacroArgument>(argumentList.Arguments.Count);
        foreach (var argument in argumentList.Arguments)
            builder.Add(new MacroArgument(argument, services));

        return builder.MoveToImmutable();
    }
}
