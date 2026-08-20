using System.Collections.Immutable;

using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Macros;

public sealed class FreestandingMacroExpansionResult
{
    private SyntaxNode? _node;
    private ImmutableArray<MemberDeclarationSyntax> _members =
        ImmutableArray<MemberDeclarationSyntax>.Empty;

    public static FreestandingMacroExpansionResult Empty { get; } = new();

    public static FreestandingMacroExpansionResult FromExpression(ExpressionSyntax expression)
    {
        ArgumentNullException.ThrowIfNull(expression);
        return new FreestandingMacroExpansionResult
        {
            Node = expression
        };
    }

    public static FreestandingMacroExpansionResult FromExpression(
        ExpressionSyntax expression,
        ImmutableArray<Diagnostic> diagnostics)
    {
        ArgumentNullException.ThrowIfNull(expression);
        return new FreestandingMacroExpansionResult
        {
            Node = expression,
            Diagnostics = Normalize(diagnostics)
        };
    }

    public static FreestandingMacroExpansionResult FromExpression(
        ExpressionSyntax expression,
        ImmutableArray<Diagnostic> diagnostics,
        ImmutableArray<MacroExpansionDiagnostic> macroDiagnostics)
    {
        ArgumentNullException.ThrowIfNull(expression);
        return new FreestandingMacroExpansionResult
        {
            Node = expression,
            Diagnostics = Normalize(diagnostics),
            MacroDiagnostics = Normalize(macroDiagnostics)
        };
    }

    public static FreestandingMacroExpansionResult FromDiagnostic(
        MacroExpansionDiagnostic diagnostic)
    {
        ArgumentNullException.ThrowIfNull(diagnostic);
        return new FreestandingMacroExpansionResult
        {
            MacroDiagnostics = [diagnostic]
        };
    }

    public static FreestandingMacroExpansionResult FromDiagnostics(
        ImmutableArray<Diagnostic> diagnostics)
        => new()
        {
            Diagnostics = Normalize(diagnostics)
        };

    public static FreestandingMacroExpansionResult FromDiagnostics(
        ImmutableArray<MacroExpansionDiagnostic> macroDiagnostics)
        => new()
        {
            MacroDiagnostics = Normalize(macroDiagnostics)
        };

    public static FreestandingMacroExpansionResult FromDiagnostics(
        ImmutableArray<Diagnostic> diagnostics,
        ImmutableArray<MacroExpansionDiagnostic> macroDiagnostics)
        => new()
        {
            Diagnostics = Normalize(diagnostics),
            MacroDiagnostics = Normalize(macroDiagnostics)
        };

    public static FreestandingMacroExpansionResult FromStatement(StatementSyntax statement)
    {
        ArgumentNullException.ThrowIfNull(statement);
        return new FreestandingMacroExpansionResult
        {
            Node = statement
        };
    }

    public static FreestandingMacroExpansionResult FromNode(SyntaxNode node)
    {
        ArgumentNullException.ThrowIfNull(node);
        return new FreestandingMacroExpansionResult
        {
            Node = node
        };
    }

    public static FreestandingMacroExpansionResult FromMembers<TMember>(SyntaxList<TMember> members)
        where TMember : MemberDeclarationSyntax
    {
        var builder = ImmutableArray.CreateBuilder<MemberDeclarationSyntax>(members.Count);
        foreach (var member in members)
            builder.Add(member);

        return FromMembers(builder.MoveToImmutable());
    }

    public static FreestandingMacroExpansionResult FromMembers(
        ImmutableArray<MemberDeclarationSyntax> members)
        => new()
        {
            Members = Normalize(members)
        };

    /// <summary>
    /// Gets or sets the single syntax node produced by this invocation.
    /// </summary>
    /// <remarks>
    /// The compiler validates the node category against the invocation position.
    /// A single-node result is mutually exclusive with a member-list result.
    /// </remarks>
    public SyntaxNode? Node
    {
        get => _node;
        set
        {
            _node = value;
            if (value is not null)
            {
                _members = ImmutableArray<MemberDeclarationSyntax>.Empty;
                HasMemberExpansion = false;
            }
        }
    }

    public ExpressionSyntax? Expression
    {
        get => Node as ExpressionSyntax;
        set => Node = value;
    }

    public StatementSyntax? Statement
    {
        get => Node as StatementSyntax;
        set => Node = value;
    }

    /// <summary>
    /// Gets or sets the ordered members produced for a member-list carrier.
    /// </summary>
    /// <remarks>
    /// Setting this property selects member-list output even when the value is
    /// empty. A member-list result is mutually exclusive with <see cref="Node"/>.
    /// </remarks>
    public ImmutableArray<MemberDeclarationSyntax> Members
    {
        get => _members;
        set
        {
            _members = Normalize(value);
            _node = null;
            HasMemberExpansion = true;
        }
    }

    /// <summary>
    /// Gets whether this result explicitly selected member-list output.
    /// </summary>
    public bool HasMemberExpansion { get; private set; }

    public ImmutableArray<MacroExpansionDiagnostic> MacroDiagnostics { get; set; } = ImmutableArray<MacroExpansionDiagnostic>.Empty;

    public ImmutableArray<Diagnostic> Diagnostics { get; set; } = ImmutableArray<Diagnostic>.Empty;

    /// <summary>
    /// Gets or sets the ordinary Raven fragments contributed for editor tooling.
    /// </summary>
    public ImmutableArray<MacroFragmentRegion> FragmentRegions { get; set; } =
        ImmutableArray<MacroFragmentRegion>.Empty;

    /// <summary>
    /// Gets or sets the token metadata contributed for editor tooling.
    /// </summary>
    public ImmutableArray<MacroTokenInfo> TokenInfos { get; set; } =
        ImmutableArray<MacroTokenInfo>.Empty;

    internal ImmutableArray<MacroFileDependency> FileDependencies { get; set; } =
        ImmutableArray<MacroFileDependency>.Empty;

    private static ImmutableArray<T> Normalize<T>(ImmutableArray<T> values)
        => values.IsDefault ? ImmutableArray<T>.Empty : values;
}

internal sealed record FreestandingMacroExpansionCacheEntry(
    FreestandingMacroExpansionResult? Result)
{
    public bool IsCurrent()
        => Result is null ||
           Result.FileDependencies.All(static dependency => dependency.IsCurrent());
}
