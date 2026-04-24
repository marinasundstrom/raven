using System;
using System.Collections.Generic;
using System.Linq;

using Raven.CodeAnalysis;
using Raven.CodeAnalysis.Symbols;
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
            static func implicit(value: Box) -> string { return "" }
            static func explicit(value: Box) -> int { return 0 }
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
    case Some(value: T)
    case None
}

public extension OptionExtensions1<T : class> for Option<T> {
    static func implicit(opt: Option<T>) -> T? {
        if opt is .Some(val value) {
            return value
        }
        null
    }
}

public extension OptionExtensions2<T : struct> for Option<T> {
    static func implicit(opt: Option<T>) -> T? {
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

    [Fact]
    public void ExtensionConversion_FromNullableReferenceToOption_UsesImportedNullableTypeParameter()
    {
        const string fixtureSource = """
namespace System

public union Option<T> {
    case Some(value: T)
    case None
}

extension OptionExtensions1<T : class> for Option<T> {
    static func implicit(value: T?) -> Option<T> {
        if value is not null {
            return Some(value)
        }

        None
    }
}
""";

        var optionReference = TestMetadataFactory.CreateFileReferenceFromSource(
            fixtureSource,
            assemblyName: $"raven-core-option-fixture-{Guid.NewGuid():N}");

        const string source = """
import System.*

val raw: string? = null
val result: Option<string> = raw
""";

        var (compilation, tree) = CreateCompilation(
            source,
            options: new CompilationOptions(OutputKind.ConsoleApplication),
            references: [.. TestMetadataReferences.Default, optionReference]);
        var diagnostics = compilation.GetDiagnostics();

        Assert.True(diagnostics.IsEmpty, string.Join(Environment.NewLine, diagnostics.Select(d => d.ToString())));

        var model = compilation.GetSemanticModel(tree);
        var declarators = tree.GetRoot().DescendantNodes().OfType<VariableDeclaratorSyntax>().ToArray();
        var rawSymbol = Assert.IsAssignableFrom<ILocalSymbol>(model.GetDeclaredSymbol(declarators.Single(d => d.Identifier.ValueText == "raw")));
        var resultSymbol = Assert.IsAssignableFrom<ILocalSymbol>(model.GetDeclaredSymbol(declarators.Single(d => d.Identifier.ValueText == "result")));
        var conversion = compilation.ClassifyConversion(rawSymbol.Type, resultSymbol.Type, includeUserDefined: true);

        Assert.True(conversion.Exists);
        Assert.True(conversion.IsImplicit);
        Assert.True(conversion.IsUserDefined);
        Assert.Equal("OptionExtensions1", conversion.MethodSymbol?.ContainingType?.Name);
        Assert.IsType<NullableTypeSymbol>(conversion.MethodSymbol!.Parameters[0].Type);
    }

    [Fact]
    public void ReferencedGenericMethodParameter_WithoutNullableMetadata_RemainsPlainTypeParameter()
    {
        var compilation = CreateCompilation();
        compilation.EnsureSetup();

        var listType = (INamedTypeSymbol)compilation.GetType(typeof(List<>))!;
        var addMethod = listType.GetMembers("Add")
            .OfType<IMethodSymbol>()
            .First(m => m.Parameters.Length == 1 && m.Parameters[0].Type is ITypeParameterSymbol);

        Assert.IsAssignableFrom<ITypeParameterSymbol>(addMethod.Parameters[0].Type);
    }

    [Fact]
    public void ExtensionConversion_GenericNullableTypeArgument_DoesNotDoubleWrapDuringConstruction()
    {
        const string fixtureSource = """
namespace System

public union Option<T> {
    case Some(value: T)
    case None
}

public extension OptionExtensions1<T : class> for Option<T> {
    static func implicit(value: T?) -> Option<T> {
        if value is not null {
            return Some(value)
        }

        None
    }
}

public union Result<T, E> {
    case Ok(value: T)
    case Error(error: E)
}
""";

        var fixtureReference = TestMetadataFactory.CreateFileReferenceFromSource(
            fixtureSource,
            assemblyName: $"raven-core-result-option-fixture-{Guid.NewGuid():N}");

        const string source = """
import System.*

func format<T>(result: Result<T, string>) -> Option<string> {
    return result match {
        Ok(val value) => value.ToString()
        Error(_) => None
    }
}
""";

        var (compilation, _) = CreateCompilation(
            source,
            options: new CompilationOptions(OutputKind.DynamicallyLinkedLibrary),
            references: [.. TestMetadataReferences.Default, fixtureReference]);

        var diagnostics = compilation.GetDiagnostics();

        Assert.True(diagnostics.IsEmpty, string.Join(Environment.NewLine, diagnostics.Select(d => d.ToString())));
    }

    [Fact]
    public void FuncStyleConversionOperator_AllowsOverloadResolutionAndExplicitCast()
    {
        const string source = """
        class Box {
            static func implicit(value: Box) -> string { return "" }
            static func explicit(value: Box) -> int { return 0 }
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

}
