namespace Raven.CodeAnalysis.Syntax;

/// <summary>
/// Options for RavenQuoter.
/// </summary>
public sealed class RavenQuoterOptions
{
    /// <summary>
    /// Include leading/trailing trivia in the quoted C#.
    /// If false, trivia is ignored.
    /// </summary>
    public bool IncludeTrivia { get; init; } = true;

    /// <summary>
    /// If true, wraps the expression in a full C# class with a Create() method.
    /// If false, only the factory expression followed by .NormalizeWhitespace() is emitted.
    /// </summary>
    public bool WrapInClass { get; init; } = false;

    /// <summary>
    /// If true, the generated code uses:
    ///   using static Raven.CodeAnalysis.Syntax.SyntaxFactory;
    ///   CompilationUnit()
    /// If false, the generated code uses:
    ///   using Raven.CodeAnalysis.Syntax;
    ///   SyntaxFactory.CompilationUnit()
    /// </summary>
    public bool UseStaticSyntaxFactoryImport { get; init; } = true;

    /// <summary>
    /// If true, using directives are generated:
    ///   using Raven.CodeAnalysis.Syntax;
    /// If false, no using directives are generated.
    /// </summary>
    /// <remarks>This also affects the UseStaticSyntaxFactoryImport option</remarks>
    public bool GenerateUsingDirectives { get; init; } = true;

    /// <summary>
    /// If true, named arguments are generated.
    /// </summary>
    public bool UseNamedArguments { get; init; } = false;

    /// <summary>
    /// If true, named arguments that are null are not printed.
    /// If false, named arguments that are null are printed.
    ///     expression: null
    /// </summary>
    /// <remarks>When UseNamedArguments is enabled</remarks>
    public bool IgnoreNullValue { get; init; } = true;

    /// <summary>
    /// If true, simple tokens are instantiated via their factory properties.
    /// If false, simple tokens are instantiated via Token(SyntaxKind.<Kind>)
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
