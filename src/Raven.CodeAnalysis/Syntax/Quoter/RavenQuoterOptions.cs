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

    public bool GenerateUsingDirectives { get; init; } = true;

    /// <summary>
    /// Name of the generated class when WrapInClass is true.
    /// </summary>
    public string GeneratedClassName { get; init; } = "QuotedSyntax";

    /// <summary>
    /// Name of the generated factory method when WrapInClass is true.
    /// </summary>
    public string GeneratedMethodName { get; init; } = "Create";
}
