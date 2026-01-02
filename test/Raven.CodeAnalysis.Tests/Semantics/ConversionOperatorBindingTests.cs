using System;
using System.IO;
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
            let box: Box = default
            takesString(box)
            let number: int = (int)box
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
        var ravenCoreSourcePath = Path.GetFullPath(Path.Combine(
            "..", "..", "..", "..", "..", "src", "Raven.Core", "Option.rav"));
        var ravenCoreSource = File.ReadAllText(ravenCoreSourcePath);

        var ravenCoreTree = SyntaxTree.ParseText(ravenCoreSource);
        var ravenCoreCompilation = Compilation.Create(
            "raven-core-option-fixture",
            [ravenCoreTree],
            TestMetadataReferences.Default,
            new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));

        var ravenCoreAssemblyPath = Path.Combine(Path.GetTempPath(), $"raven-core-option-fixture-{Guid.NewGuid():N}.dll");

        try
        {
            using var ravenCoreStream = new MemoryStream();
            var ravenCoreEmit = ravenCoreCompilation.Emit(ravenCoreStream);
            Assert.True(ravenCoreEmit.Success, string.Join(Environment.NewLine, ravenCoreEmit.Diagnostics));

            File.WriteAllBytes(ravenCoreAssemblyPath, ravenCoreStream.ToArray());

            var ravenCoreReference = MetadataReference.CreateFromFile(ravenCoreAssemblyPath);

            const string source = """
import System.*

let value = Option<int>.None
let result: int? = value
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
        finally
        {
            if (File.Exists(ravenCoreAssemblyPath))
                File.Delete(ravenCoreAssemblyPath);
        }
    }
}
