using System.Reflection;
using Raven.CodeAnalysis;
using Raven.CodeAnalysis.Syntax;
using Raven.CodeAnalysis.Text;

namespace Raven.CompilerParity.Tests;

[Trait("Target", "DotNet")]
public sealed class SyntaxRuntimeIdentityTests
{
    [Fact]
    public void SyntaxRuntimeTypes_AreOwnedByTheSelectedImplementation()
    {
        var implementation = typeof(TextSpan).Assembly;
        Type[] types = [typeof(SyntaxNode), typeof(GreenNode), typeof(SyntaxNodeOrToken),
            typeof(SyntaxToken), typeof(SyntaxTrivia), typeof(SyntaxTokenList),
            typeof(SyntaxTriviaList), typeof(SyntaxList<ExpressionSyntax>),
            typeof(SeparatedSyntaxList<ExpressionSyntax>), typeof(SyntaxFactory),
            typeof(CompilationUnitSyntax), typeof(SyntaxTree), typeof(SyntaxReference),
            typeof(SyntaxVisitor), typeof(SyntaxVisitor<object>), typeof(SyntaxWalker),
            typeof(SyntaxRewriter), typeof(Diagnostic), typeof(Location), typeof(ParseOptions),
            typeof(SourceCodeKind), typeof(LocationKind), typeof(SourceTextWriter),
            typeof(Raven.CodeAnalysis.Syntax.InternalSyntax.SyntaxNode),
            typeof(Raven.CodeAnalysis.Syntax.InternalSyntax.SyntaxToken),
            typeof(Raven.CodeAnalysis.Syntax.InternalSyntax.SyntaxTrivia),
            typeof(Raven.CodeAnalysis.Syntax.InternalSyntax.SyntaxFactory),
            typeof(Raven.CodeAnalysis.Syntax.InternalSyntax.Parser.LanguageParser),
            typeof(Raven.CodeAnalysis.Syntax.InternalSyntax.Parser.ParseResult),
            typeof(PrinterOptions), typeof(PrettySyntaxTreePrinter), typeof(SubmissionCompleteness),
            typeof(RavenQuoter), typeof(RavenQuoterOptions), typeof(RavenQuoterOutputLanguage)];
        Assert.All(types, type => Assert.Same(implementation, type.Assembly));
    }

    [Fact]
    public void SelectedAssembly_AllTypesCanBeLoaded()
    {
        var assembly = typeof(TextSpan).Assembly;
        Assert.NotEmpty(assembly.GetTypes());
    }
}
