namespace Raven.CodeAnalysis.Syntax;

/// <summary>
/// The source language used to render syntax factory construction code.
/// </summary>
public enum RavenQuoterOutputLanguage
{
    Raven,
    CSharp
}

/// <summary>
/// Options for RavenQuoter.
/// </summary>
public sealed class RavenQuoterOptions
{
    internal Func<SyntaxNode, string?>? NodeSourceOverride { get; init; }

    /// <summary>
    /// The source language used for imports, declarations, and collections.
    /// </summary>
    public RavenQuoterOutputLanguage OutputLanguage { get; init; } = RavenQuoterOutputLanguage.Raven;

    /// <summary>
    /// Include leading/trailing trivia in the quoted code.
    /// If false, trivia is ignored.
    /// </summary>
    public bool IncludeTrivia { get; init; } = true;

    /// <summary>
    /// If true, wraps the expression in a class with a Create() method.
    /// If false, only the factory expression followed by .NormalizeWhitespace() is emitted.
    /// </summary>
    public bool WrapInClass { get; init; } = false;

    /// <summary>
    /// Append a call to <c>NormalizeWhitespace()</c> to the generated root.
    /// Disable this when the generated factory expression must preserve the
    /// quoted node's exact static result type.
    /// </summary>
    public bool NormalizeWhitespace { get; init; } = true;

    /// <summary>
    /// If true, imports <c>SyntaxFactory</c> members and emits
    /// <c>CompilationUnit()</c>. If false, emits
    /// <c>SyntaxFactory.CompilationUnit()</c>.
    /// </summary>
    public bool UseStaticSyntaxFactoryImport { get; init; } = true;

    /// <summary>
    /// Fully qualify syntax factory, syntax kind, and syntax node type names.
    /// This allows the generated expression to bind without syntax namespace
    /// imports in the consuming source file.
    /// </summary>
    public bool FullyQualifyNames { get; init; } = false;

    /// <summary>
    /// If true, imports appropriate for <see cref="OutputLanguage"/> are generated.
    /// If false, no imports are generated.
    /// </summary>
    /// <remarks>This also affects the UseStaticSyntaxFactoryImport option</remarks>
    public bool GenerateUsingDirectives { get; init; } = true;

    /// <summary>
    /// If true, named arguments are generated.
    /// </summary>
    public bool UseNamedArguments { get; init; } = false;

    /// <summary>
    /// If true, named arguments that are null are not printed.
    /// If false, named arguments that are null are printed:
    ///     expression: null
    /// </summary>
    /// <remarks>When UseNamedArguments is enabled</remarks>
    public bool IgnoreNullValue { get; init; } = true;

    /// <summary>
    /// If true, single args will be inlined:
    ///      IdentifierName(Identifier("Foo"))
    /// If false, the default behavior:
    ///     IdentifierName(
    ///         Identifier("Foo")
    ///     )
    /// </summary>
    /// <remarks>This is automatically disabled for named arguments</remarks>
    public bool InlineSingleArg { get; init; } = false;

    /// <summary>
    /// If true, simple tokens are instantiated via their factory properties.
    /// If false, simple tokens are instantiated via <c>Token(SyntaxKind.Kind)</c>.
    /// </summary>
    public bool UseFactoryPropsForSimpleTokens { get; init; } = true;

    /// <summary>
    /// Name of the generated class when WrapInClass is true.
    /// </summary>
    public string GeneratedClassName { get; init; } = "QuotedSyntax";

    /// <summary>
    /// Name of the generated factory method when WrapInClass is true.
    /// </summary>
    public string GeneratedMethodName { get; init; } = "Create";
}
