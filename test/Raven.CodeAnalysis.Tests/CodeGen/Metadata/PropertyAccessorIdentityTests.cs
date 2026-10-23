using System;
using System.IO;
using System.Linq;

using Raven.CodeAnalysis.Syntax;

using Xunit;

namespace Raven.CodeAnalysis.Tests.CodeGen;

public class PropertyAccessorIdentityTests
{
    [Fact]
    public void RepeatedBindingKeepsOneExpressionGetterPerUnionProperty()
    {
        var tree = SyntaxTree.ParseText("""
            union Maybe<T> { case None; case Some(value: T) }
            interface Error { val Cause: Maybe<Error> { get } }
            union Failure : Error {
                val Cause: Maybe<Error> => Maybe<Error>.None
                case Missing
            }
            """);
        var compilation = Compilation.Create("AccessorIdentity", [tree], TestMetadataReferences.Default,
            new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        var model = compilation.GetSemanticModel(tree);
        var declaration = tree.GetRoot().DescendantNodes().OfType<UnionDeclarationSyntax>().Single(n => n.Identifier.ValueText == "Failure");
        var type = (INamedTypeSymbol)model.GetDeclaredSymbol(declaration)!;
        var property = Assert.Single(type.GetMembers().OfType<IPropertySymbol>(), p => p.Name == "Cause");
        var getter = property.GetMethod;
        Assert.DoesNotContain(compilation.GetDiagnostics(), d => d.Severity == DiagnosticSeverity.Error);
        using var output = new MemoryStream();
        var result = compilation.Emit(output);
        Assert.True(result.Success, string.Join(Environment.NewLine, result.Diagnostics));
        Assert.Same(getter, property.GetMethod);
        Assert.Same(getter, Assert.Single(type.GetMembers().OfType<IMethodSymbol>(), m => m.Name == "get_Cause"));
    }
}
