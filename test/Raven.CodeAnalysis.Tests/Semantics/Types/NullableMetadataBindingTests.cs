using System.Linq;

using Raven.CodeAnalysis.Syntax;
using Raven.CodeAnalysis.Tests;

namespace Raven.CodeAnalysis.Semantics.Tests;

[Trait("Target", "DotNet")]
public sealed class NullableMetadataBindingTests : CompilationTestBase
{
    [Theory]
    [InlineData(false)]
    [InlineData(true)]
    public void FirstOrDefault_ReferenceResultRetainsNullability(bool diagnosticsFirst)
    {
        const string source = """
import System.Collections.Generic.*
import System.Linq.*

func Find(values: IEnumerable<string>) -> string? {
    var value = values.FirstOrDefault()
    value = value?.Trim()
    return value
}
""";
        var (compilation, tree) = CreateCompilation(source);
        var model = compilation.GetSemanticModel(tree);
        if (diagnosticsFirst)
            _ = compilation.GetDiagnostics();
        var invocation = tree.GetRoot().DescendantNodes().OfType<InvocationExpressionSyntax>().First();
        var method = Assert.IsAssignableFrom<IMethodSymbol>(model.GetSymbolInfo(invocation).Symbol);
        Assert.True(method.ReturnType.IsNullable, $"Method return: {method.ReturnType}");
        Assert.True(model.GetTypeInfo(invocation).Type?.IsNullable);
        Assert.DoesNotContain(compilation.GetDiagnostics(), diagnostic => diagnostic.Severity == DiagnosticSeverity.Error);
    }

    [Theory]
    [InlineData("First", "string", false)]
    [InlineData("FirstOrDefault", "string", true)]
    [InlineData("LastOrDefault", "string", true)]
    [InlineData("SingleOrDefault", "string", true)]
    [InlineData("FirstOrDefault", "int", false)]
    public void GenericSequenceReturn_UsesSubstitutedNullability(string name, string elementType, bool nullable)
    {
        var source = $$"""
import System.Collections.Generic.*
import System.Linq.*
func Inspect(values: IEnumerable<{{elementType}}>) -> unit {
    let value = values.{{name}}()
}
""";
        var (compilation, tree) = CreateCompilation(source);
        var invocation = tree.GetRoot().DescendantNodes().OfType<InvocationExpressionSyntax>().Single();
        Assert.Equal(nullable, compilation.GetSemanticModel(tree).GetTypeInfo(invocation).Type?.IsNullable);
        Assert.DoesNotContain(compilation.GetDiagnostics(), diagnostic => diagnostic.Severity == DiagnosticSeverity.Error);
    }

    [Theory]
    [InlineData("int", false)]
    [InlineData("int?", true)]
    public void InterfaceSignature_DistinguishesAnnotationFromNullableValue(string parameterType, bool missingMember)
    {
        var source = $$"""
import System.*
class Comparable : IEquatable<int> {
    func Equals(other: {{parameterType}}) -> bool => true
}
""";
        var (compilation, _) = CreateCompilation(source);
        var errors = compilation.GetDiagnostics().Where(d => d.Severity == DiagnosticSeverity.Error).ToArray();
        if (missingMember)
            Assert.Contains(errors, d => d.Descriptor == CompilerDiagnostics.TypeDoesNotImplementAbstractMember);
        else
            Assert.Empty(errors);
    }

    [Fact]
    public void NotNullWhen_DoesNotRefineNullableArgument()
    {
        const string source = """
import Raven.ExtensionMethodsFixture.*

func Inspect(value: string?) -> unit {
    if NullableFlowFixture.ArePresent(value, value) {
        let length = value.Length
    }
}
""";

        var (compilation, _) = CreateCompilation(
            source,
            references: TestMetadataReferences.DefaultWithExtensionMethods);

        Assert.Contains(
            compilation.GetDiagnostics(),
            diagnostic => diagnostic.Descriptor == CompilerDiagnostics.NullableValueMemberAccess);
    }

    [Fact]
    public void MemberNotNull_DoesNotRefineNullableProperty()
    {
        const string source = """
import Raven.ExtensionMethodsFixture.*

func Inspect(holder: MemberNullabilityFixture) -> unit {
    holder.Initialize()
    let length = holder.Value.Length
}
""";

        var (compilation, _) = CreateCompilation(
            source,
            references: TestMetadataReferences.DefaultWithExtensionMethods);

        Assert.Contains(
            compilation.GetDiagnostics(),
            diagnostic => diagnostic.Descriptor == CompilerDiagnostics.NullableValueMemberAccess);
    }

    [Fact]
    public void AllowNullParameter_AcceptsNullInput()
    {
        const string source = """
import Raven.ExtensionMethodsFixture.*

NullableFlowFixture.AcceptNull(null)
""";

        var (compilation, _) = CreateCompilation(
            source,
            references: TestMetadataReferences.DefaultWithExtensionMethods);

        Assert.DoesNotContain(
            compilation.GetDiagnostics(),
            diagnostic => diagnostic.Descriptor == CompilerDiagnostics.CannotAssignNullToType);
    }

    [Fact]
    public void DisallowNullParameter_RejectsNullInput()
    {
        const string source = """
import Raven.ExtensionMethodsFixture.*

NullableFlowFixture.RejectNull(null)
""";

        var (compilation, _) = CreateCompilation(
            source,
            references: TestMetadataReferences.DefaultWithExtensionMethods);

        Assert.Contains(
            compilation.GetDiagnostics(),
            diagnostic => diagnostic.Descriptor == CompilerDiagnostics.CannotAssignNullToType);
    }

    [Theory]
    [InlineData("RequiredName", false)]
    [InlineData("OptionalName", true)]
    [InlineData("OrdinaryName", true)]
    public void PropertyInputContract_ControlsNullAssignment(string propertyName, bool expectDiagnostic)
    {
        var source = $$"""
import Raven.ExtensionMethodsFixture.*

NullableFlowFixture.{{propertyName}} = null
""";

        var (compilation, _) = CreateCompilation(
            source,
            references: TestMetadataReferences.DefaultWithExtensionMethods);

        Assert.Equal(
            expectDiagnostic,
            compilation.GetDiagnostics().Any(
                diagnostic => diagnostic.Descriptor == CompilerDiagnostics.CannotAssignNullToType));
    }

    [Fact]
    public void MaybeNullReturn_HasStaticNullableType()
    {
        const string source = """
import Raven.ExtensionMethodsFixture.*

let value = NullableFlowFixture.FindName()
let length = value.Length
""";

        var (compilation, tree) = CreateCompilation(
            source,
            references: TestMetadataReferences.DefaultWithExtensionMethods);
        var invocation = tree.GetRoot().DescendantNodes().OfType<InvocationExpressionSyntax>().First();
        var invocationType = compilation.GetSemanticModel(tree).GetTypeInfo(invocation).Type;

        Assert.True(invocationType?.IsNullable);
        Assert.Contains(
            compilation.GetDiagnostics(),
            diagnostic => diagnostic.Descriptor == CompilerDiagnostics.NullableValueMemberAccess);
    }

    [Fact]
    public void MaybeNullReturn_CannotInitializeNonNullableBinding()
    {
        const string source = """
import Raven.ExtensionMethodsFixture.*

let value: string = NullableFlowFixture.FindName()
""";

        var (compilation, _) = CreateCompilation(
            source,
            references: TestMetadataReferences.DefaultWithExtensionMethods);

        Assert.Contains(
            compilation.GetDiagnostics(),
            diagnostic =>
                diagnostic.Descriptor == CompilerDiagnostics.CannotAssignNullToType ||
                diagnostic.Descriptor == CompilerDiagnostics.CannotAssignFromTypeToType);
    }

    [Fact]
    public void NotNullIfNotNull_DoesNotChangeDeclaredNullableReturnType()
    {
        const string source = """
import Raven.ExtensionMethodsFixture.*

let value = NullableFlowFixture.Echo("raven")
let length = value.Length
""";

        var (compilation, tree) = CreateCompilation(
            source,
            references: TestMetadataReferences.DefaultWithExtensionMethods);
        var invocation = tree.GetRoot().DescendantNodes().OfType<InvocationExpressionSyntax>().First();

        Assert.True(compilation.GetSemanticModel(tree).GetTypeInfo(invocation).Type?.IsNullable);
        Assert.Contains(
            compilation.GetDiagnostics(),
            diagnostic => diagnostic.Descriptor == CompilerDiagnostics.NullableValueMemberAccess);
    }
}

[Trait("Target", "NeoCLR")]
public sealed class NeoClrNullableGenericMetadataTests
{
    [Theory]
    [InlineData("First", false)]
    [InlineData("FirstOrDefault", true)]
    public void NeoClrSemanticBindingWithHostReferences_PreservesDeclaredNullability(string name, bool nullable)
    {
        var tree = SyntaxTree.ParseText($$"""
import System.Collections.Generic.*
import System.Linq.*
func Inspect(values: IEnumerable<string>) -> unit {
    let value = values.{{name}}()
}
""");
        var compilation = Compilation.Create("NeoClrNullableGeneric", [tree], TestMetadataReferences.Default,
            new CompilationOptions(OutputKind.DynamicallyLinkedLibrary).WithTargetPlatform(TargetPlatform.NeoCLR));
        var model = compilation.GetSemanticModel(tree);
        var invocation = tree.GetRoot().DescendantNodes().OfType<InvocationExpressionSyntax>().Single();
        Assert.Equal(nullable, model.GetTypeInfo(invocation).Type?.IsNullable);
        Assert.DoesNotContain(model.GetDiagnostics(), diagnostic => diagnostic.Severity == DiagnosticSeverity.Error);
    }
}
