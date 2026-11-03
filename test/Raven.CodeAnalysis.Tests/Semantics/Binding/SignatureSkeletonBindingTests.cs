using System;
using System.Linq;

using Raven.CodeAnalysis.Syntax;

using Raven.CodeAnalysis.Semantics.Tests;

using Xunit;

namespace Raven.CodeAnalysis.Tests.Semantics.Binding;

public sealed class SignatureSkeletonBindingTests : CompilationTestBase
{
    [Theory]
    [InlineData("import Raven.CodeAnalysis.Syntax.InternalSyntax.*", "DiagnosticInfo")]
    [InlineData("import Raven.CodeAnalysis.Syntax.InternalSyntax.DiagnosticInfo", "DiagnosticInfo")]
    [InlineData("", "Raven.CodeAnalysis.Syntax.InternalSyntax.DiagnosticInfo")]
    [Trait("Target", "Shared")]
    public void ImportedSkeletonParameter_PrefersSourceOverBootstrapMetadata(string import, string typeName)
    {
        var source = SyntaxTree.ParseText("""
            namespace Raven.CodeAnalysis.Syntax.InternalSyntax
            public class DiagnosticInfo {}
            """);
        var consumer = SyntaxTree.ParseText($$"""
            namespace Port
            {{import}}
            public class Consumer {
                func Use(value: {{typeName}}?[]) {}
            }
            """);
        var references = TestMetadataReferences.Default.Append(
            MetadataReference.CreateFromFile(typeof(Raven.CodeAnalysis.Syntax.GreenNode).Assembly.Location)).ToArray();
        var compilation = Compilation.Create("SkeletonOwnership", [source, consumer], references,
            new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        var expected = compilation.GetTypeByMetadataName("Raven.CodeAnalysis.Syntax.InternalSyntax.DiagnosticInfo")!;
        Assert.Same(compilation.Assembly, expected.ContainingAssembly);
        var model = compilation.GetSemanticModel(consumer);
        var declaration = consumer.GetRoot().DescendantNodes().OfType<ClassDeclarationSyntax>().Single();
        var containingType = Assert.IsAssignableFrom<NamedTypeSymbol>(model.GetDeclaredSymbol(declaration));
        var parameter = declaration.DescendantNodes().OfType<ParameterSyntax>().Single();
        var type = MemberSignatureDeclarationPass.ResolveSkeletonType(model, parameter.TypeAnnotation!.Type,
            compilation.ErrorTypeSymbol, containingType);
        var array = Assert.IsAssignableFrom<ArrayTypeSymbol>(type);
        Assert.True(SymbolEqualityComparer.Default.Equals(expected, array.ElementType.GetNonNullableType()));
        Assert.Same(compilation.Assembly, array.ElementType.GetNonNullableType().ContainingAssembly);
    }

    [Fact]
    public void PrimaryConstructorMethodBody_DoesNotReportDuplicateSkeletonCandidate()
    {
        const string source = """
import System.*
import System.Threading.Tasks.*

union Optional<T> {
    case Some(value: T)
    case None
}

let foo = Foo(42)
let result = await foo(true)

class Foo(value: int) {
    async func Test(flag: bool) -> Task<Optional<int>> {
        await Task.Delay(20)

        if flag {
            return .Some(value)
        }

        return .None
    }

    func self(flag: bool) -> Task<Optional<int>> {
        return Test(flag)
    }
}
""";

        var (compilation, _) = CreateCompilation(source);

        var diagnostics = compilation.GetDiagnostics();

        Assert.DoesNotContain(diagnostics, diagnostic => diagnostic.Severity == DiagnosticSeverity.Error);
        Assert.DoesNotContain(
            diagnostics,
            diagnostic => diagnostic.ToString().Contains("ambiguous", StringComparison.OrdinalIgnoreCase));
    }
}
