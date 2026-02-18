using System.Linq;

using Raven.CodeAnalysis.Syntax;

using Xunit;

namespace Raven.CodeAnalysis.Syntax.Tests;

public class NamespaceDirectiveSyntaxTests
{
    [Fact]
    public void FileScopedNamespaceDirective_OpenModifierAttachesToType()
    {
        const string source = """
        namespace Samples

        open class Person {}
        """;

        var tree = SyntaxTree.ParseText(source);
        var root = (CompilationUnitSyntax)tree.GetRoot();

        var ns = Assert.Single(root.Members.OfType<FileScopedNamespaceDeclarationSyntax>());
        var person = Assert.Single(ns.Members.OfType<ClassDeclarationSyntax>());

        Assert.Contains(person.Modifiers, modifier => modifier.Kind == SyntaxKind.OpenKeyword);
    }
}
