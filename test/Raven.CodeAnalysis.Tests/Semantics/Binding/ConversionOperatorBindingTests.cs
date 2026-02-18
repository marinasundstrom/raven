using System;
using System.Linq;

using Raven.CodeAnalysis;
using Raven.CodeAnalysis.Syntax;
using Raven.CodeAnalysis.Testing;
using Raven.CodeAnalysis.Tests;

using Xunit;

namespace Raven.CodeAnalysis.Semantics.Tests;

public class ConversionOperatorBindingTests : CompilationTestBase
{
    [Fact]
    public void ImplicitConversionOperator_AllowsOverloadResolutionAndExplicitCast()
    {
        const string source = """
        class Box {
            public static implicit operator(value: Box) -> string { return "" }
            public static explicit operator(value: Box) -> int { return 0 }
        }

        func takesString(value: string) -> () { }

        func test() -> () {
            val box: Box = default
            takesString(box)
            val number: int = (int)box
        }
        """;

        var (compilation, _) = CreateCompilation(source);
        var diagnostics = compilation.GetDiagnostics();

        Assert.DoesNotContain(
            diagnostics,
            d => d.Descriptor == CompilerDiagnostics.CannotConvertFromTypeToType);
    }

    [Fact]
    public void ExtensionConversion_PicksConstraintCompatibleOperator()
    {
        const string fixtureSource = """
namespace System

public union Option<T> {
    Some(value: T)
    None
}

public extension OptionExtensions1<T : class> for Option<T> {
    public static implicit operator(opt: Option<T>) -> T? {
        if opt is .Some(val value) {
            return value
        }
        null
    }
}

public extension OptionExtensions2<T : struct> for Option<T> {
    public static implicit operator(opt: Option<T>) -> T? {
        if opt is .Some(val value) {
            return value
        }
        null
    }
}
""";

        var ravenCoreReference = TestMetadataFactory.CreateFileReferenceFromSource(
            fixtureSource,
            assemblyName: $"raven-core-option-fixture-{Guid.NewGuid():N}");

            const string source = """
import System.*

val value = Option<int>.None
val result: int? = value
""";

            var (compilation, tree) = CreateCompilation(
                source,
                options: new CompilationOptions(OutputKind.ConsoleApplication),
                references: [.. TestMetadataReferences.Default, ravenCoreReference]);
            var diagnostics = compilation.GetDiagnostics();

            Assert.True(diagnostics.IsEmpty, string.Join(Environment.NewLine, diagnostics.Select(d => d.ToString())));

            var model = compilation.GetSemanticModel(tree);
            var declarators = tree.GetRoot().DescendantNodes().OfType<VariableDeclaratorSyntax>().ToArray();
            var valueDeclarator = declarators.Single(d => d.Identifier.ValueText == "value");
            var resultDeclarator = declarators.Single(d => d.Identifier.ValueText == "result");
            var valueSymbol = Assert.IsAssignableFrom<ILocalSymbol>(model.GetDeclaredSymbol(valueDeclarator));
            var resultSymbol = Assert.IsAssignableFrom<ILocalSymbol>(model.GetDeclaredSymbol(resultDeclarator));

            var conversion = compilation.ClassifyConversion(valueSymbol.Type, resultSymbol.Type, includeUserDefined: true);

            Assert.True(conversion.Exists);
            Assert.True(conversion.IsUserDefined);
            Assert.NotNull(conversion.MethodSymbol);
            Assert.Equal("OptionExtensions2", conversion.MethodSymbol?.ContainingType?.Name);
    }
}
