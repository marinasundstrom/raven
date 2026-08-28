using System;
using System.IO;
using System.Reflection;

using Raven.CodeAnalysis.Syntax;
using Raven.CodeAnalysis.Testing;
using Raven.CodeAnalysis.Tests;

using Xunit;

namespace Raven.CodeAnalysis.Tests.CodeGen;

public sealed class NamespaceMemberCodeGenTests
{
    [Fact]
    public void TopLevelFunctionAndConst_EmitIntoNamespaceContainer()
    {
        const string source = """
namespace Utilities {
    public const Answer: int = 41

    public func AddOne(value: int) -> int => value + 1
}
""";

        var tree = SyntaxTree.ParseText(source);
        var compilation = Compilation.Create(
            "namespaceMembers",
            [tree],
            TestMetadataReferences.Default,
            new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));

        using var peStream = new MemoryStream();
        var result = compilation.Emit(peStream);
        Assert.True(result.Success, string.Join(Environment.NewLine, result.Diagnostics));

        using var loaded = TestAssemblyLoader.LoadFromStream(peStream, compilation.References);
        var container = loaded.Assembly.GetType("Utilities.NamespaceMembers", throwOnError: true)!;
        Assert.True(container.IsAbstract);
        Assert.True(container.IsSealed);
        Assert.Contains(
            container.GetCustomAttributesData(),
            attribute => attribute.AttributeType.FullName == "System.Runtime.CompilerServices.TopLevelAttribute");

        var method = container.GetMethod("AddOne", BindingFlags.Public | BindingFlags.Static);
        Assert.NotNull(method);
        Assert.Equal(42, method!.Invoke(null, [41]));

        var field = container.GetField("Answer", BindingFlags.Public | BindingFlags.Static);
        Assert.NotNull(field);
        Assert.True(field!.IsLiteral);
        Assert.Equal(41, field.GetRawConstantValue());
    }

    [Fact]
    public void NamespaceScopeAttributes_EmitOnlyOnTheirDeclaredMetadataOwners()
    {
        const string source = """
import System.*
import System.ComponentModel.*

[assembly: Description("assembly")]
[module: Description("module")]

[Description("type")]
class Widget { }

[Description("global function")]
[return: Description("global return")]
public func TransformGlobal([Description("global parameter")] value: string) -> string => value

[Description("global const")]
public const GlobalAnswer: int = 41

namespace Utilities {
    [Description("function")]
    [return: Description("return")]
    public func Transform([Description("parameter")] value: string) -> string => value

    [Description("const")]
    public const Answer: int = 42
}
""";

        var tree = SyntaxTree.ParseText(source);
        var compilation = Compilation.Create(
            "namespaceMemberAttributes",
            [tree],
            TestMetadataReferences.Default,
            new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));

        using var peStream = new MemoryStream();
        var result = compilation.Emit(peStream);
        Assert.True(result.Success, string.Join(Environment.NewLine, result.Diagnostics));

        using var loaded = TestAssemblyLoader.LoadFromStream(peStream, compilation.References);
        var assembly = loaded.Assembly;
        Assert.Equal(["assembly"], GetDescriptionLabels(assembly.GetCustomAttributesData()));
        Assert.Equal(["module"], GetDescriptionLabels(assembly.ManifestModule.GetCustomAttributesData()));

        var widget = assembly.GetType("Widget", throwOnError: true)!;
        Assert.Equal(["type"], GetDescriptionLabels(widget.GetCustomAttributesData()));

        var globalContainer = assembly.GetType("NamespaceMembers", throwOnError: true)!;
        Assert.Empty(GetDescriptionLabels(globalContainer.GetCustomAttributesData()));

        var globalMethod = globalContainer.GetMethod("TransformGlobal", BindingFlags.Public | BindingFlags.Static)!;
        Assert.Equal(["global function"], GetDescriptionLabels(globalMethod.GetCustomAttributesData()));
        Assert.Equal(["global return"], GetDescriptionLabels(globalMethod.ReturnParameter.GetCustomAttributesData()));
        Assert.Equal(
            ["global parameter"],
            GetDescriptionLabels(Assert.Single(globalMethod.GetParameters()).GetCustomAttributesData()));

        var globalField = globalContainer.GetField("GlobalAnswer", BindingFlags.Public | BindingFlags.Static)!;
        Assert.Equal(["global const"], GetDescriptionLabels(globalField.GetCustomAttributesData()));

        var container = assembly.GetType("Utilities.NamespaceMembers", throwOnError: true)!;
        Assert.Empty(GetDescriptionLabels(container.GetCustomAttributesData()));

        var method = container.GetMethod("Transform", BindingFlags.Public | BindingFlags.Static)!;
        Assert.Equal(["function"], GetDescriptionLabels(method.GetCustomAttributesData()));
        Assert.Equal(["return"], GetDescriptionLabels(method.ReturnParameter.GetCustomAttributesData()));
        Assert.Equal(["parameter"], GetDescriptionLabels(Assert.Single(method.GetParameters()).GetCustomAttributesData()));

        var field = container.GetField("Answer", BindingFlags.Public | BindingFlags.Static)!;
        Assert.Equal(["const"], GetDescriptionLabels(field.GetCustomAttributesData()));
    }

    [Theory]
    [InlineData("Utilities.NamespaceMembers", """
        [Marker("function")]
        public func Handle(value: string) -> string => value

        [Marker("const")]
        public const Answer: int = 42
        """)]
    [InlineData("Utilities.NamespaceMembers", """
        [Marker("const")]
        public const Answer: int = 42

        [Marker("function")]
        public func Handle(value: string) -> string => value
        """)]
    [InlineData("NamespaceMembers", """
        [Marker("function")]
        public func Handle(value: string) -> string => value

        [Marker("const")]
        public const Answer: int = 42
        """)]
    [InlineData("NamespaceMembers", """
        [Marker("const")]
        public const Answer: int = 42

        [Marker("function")]
        public func Handle(value: string) -> string => value
        """)]
    public void NamespaceMemberDeclarationAttributes_DoNotLeakToSynthesizedContainer(
        string containerTypeName,
        string declarations)
    {
        var scopedDeclarations = containerTypeName.Contains('.')
            ? $$"""
              namespace Utilities {
              {{declarations}}
              }
              """
            : declarations;

        var source = $$"""
import System.*

[AttributeUsage(AttributeTargets.Method | AttributeTargets.Field, AllowMultiple: true)]
class MarkerAttribute : Attribute
{
    init(label: string) { }
}

{{scopedDeclarations}}
""";

        var tree = SyntaxTree.ParseText(source);
        var compilation = Compilation.Create(
            "namespaceMemberAttributeOrder",
            [tree],
            TestMetadataReferences.Default,
            new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));

        using var peStream = new MemoryStream();
        var result = compilation.Emit(peStream);
        Assert.True(result.Success, string.Join(Environment.NewLine, result.Diagnostics));

        using var loaded = TestAssemblyLoader.LoadFromStream(peStream, compilation.References);
        var container = loaded.Assembly.GetType(containerTypeName, throwOnError: true)!;

        Assert.Empty(GetMarkerLabels(container.GetCustomAttributesData()));
        Assert.Equal(
            ["function"],
            GetMarkerLabels(container.GetMethod("Handle", BindingFlags.Public | BindingFlags.Static)!.GetCustomAttributesData()));
        Assert.Equal(
            ["const"],
            GetMarkerLabels(container.GetField("Answer", BindingFlags.Public | BindingFlags.Static)!.GetCustomAttributesData()));
    }

    [Fact]
    public void CompilationUnitFunctionAttributes_AttachToFunctionSymbolAndEmittedMethod()
    {
        const string source = """
import System.ComponentModel.*

[Description("function")]
[return: Description("return")]
func Transform([Description("parameter")] value: string) -> string => value

let transformed = Transform("ok")
""";

        var tree = SyntaxTree.ParseText(source);
        var compilation = Compilation.Create(
            "compilationUnitFunctionAttributes",
            [tree],
            TestMetadataReferences.Default,
            new CompilationOptions(OutputKind.ConsoleApplication));

        var model = compilation.GetSemanticModel(tree);
        var declaration = tree.GetRoot().DescendantNodes().OfType<FunctionStatementSyntax>().Single();
        var methodSymbol = Assert.IsAssignableFrom<IMethodSymbol>(model.GetDeclaredSymbol(declaration));

        Assert.Equal("Program", methodSymbol.ContainingType?.Name);
        Assert.Equal(["function"], GetDescriptionLabels(methodSymbol.GetAttributes()));
        Assert.Equal(["return"], GetDescriptionLabels(methodSymbol.GetReturnTypeAttributes()));
        Assert.Equal(["parameter"], GetDescriptionLabels(Assert.Single(methodSymbol.Parameters).GetAttributes()));
        Assert.Empty(GetDescriptionLabels(methodSymbol.ContainingType!.GetAttributes()));

        using var peStream = new MemoryStream();
        var result = compilation.Emit(peStream);
        Assert.True(result.Success, string.Join(Environment.NewLine, result.Diagnostics));

        using var loaded = TestAssemblyLoader.LoadFromStream(peStream, compilation.References);
        var program = loaded.Assembly.GetType("Program", throwOnError: true)!;
        Assert.Empty(GetDescriptionLabels(program.GetCustomAttributesData()));

        var method = program.GetMethod("Transform", BindingFlags.NonPublic | BindingFlags.Static)!;
        Assert.Equal(["function"], GetDescriptionLabels(method.GetCustomAttributesData()));
        Assert.Equal(["return"], GetDescriptionLabels(method.ReturnParameter.GetCustomAttributesData()));
        Assert.Equal(["parameter"], GetDescriptionLabels(Assert.Single(method.GetParameters()).GetCustomAttributesData()));
    }

    [Fact]
    public void ClassTargetedAttributes_EmitOnRootFileAndBlockScopedNamespaceContainers()
    {
        var rootTree = SyntaxTree.ParseText("""
import System.*

[AttributeUsage(AttributeTargets.Class, AllowMultiple: true)]
class MarkerAttribute : Attribute
{
    init(label: string) { }
}

[class: Marker("root function")]
[class: Marker("root function second")]

public func RootFunction() -> int => 1

[class: Marker("root const")]

public const RootAnswer: int = 42

[class: Marker("root type")]

class Anchor { }
""");
        var blockScopedTree = SyntaxTree.ParseText("""
namespace Samples {
    [class: Marker("block function")]

    public func BlockFunction() -> int => 2
}
""");
        var fileScopedTree = SyntaxTree.ParseText("""
namespace Samples;

[class: Marker("file const")]

public const FileAnswer: int = 43
""");
        var namespaceDeclarationTree = SyntaxTree.ParseText("""
[class: Marker("root namespace declaration")]

namespace Empty {
    [class: Marker("empty namespace body")]

    public const Anchor: int = 0
}
""");
        var nestedNamespaceTree = SyntaxTree.ParseText("""
namespace Outer {
    [class: Marker("outer namespace")]

    namespace Inner {
        [class: Marker("inner namespace")]

        public func NestedFunction() -> int => 3
    }
}
""");
        var compilation = Compilation.Create(
            "namespaceContainerAttributes",
            [rootTree, blockScopedTree, fileScopedTree, namespaceDeclarationTree, nestedNamespaceTree],
            TestMetadataReferences.Default,
            new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));

        using var peStream = new MemoryStream();
        var result = compilation.Emit(peStream);
        Assert.True(result.Success, string.Join(Environment.NewLine, result.Diagnostics));

        using var loaded = TestAssemblyLoader.LoadFromStream(peStream, compilation.References);
        var rootContainer = loaded.Assembly.GetType("NamespaceMembers", throwOnError: true)!;
        Assert.Equal(
            ["root function", "root function second", "root const", "root type", "root namespace declaration"],
            GetMarkerLabels(rootContainer.GetCustomAttributesData()));
        Assert.Empty(GetMarkerLabels(rootContainer.GetMethod("RootFunction")!.GetCustomAttributesData()));
        Assert.Empty(GetMarkerLabels(rootContainer.GetField("RootAnswer")!.GetCustomAttributesData()));
        Assert.Empty(GetMarkerLabels(loaded.Assembly.GetType("Anchor", throwOnError: true)!.GetCustomAttributesData()));

        var samplesContainer = loaded.Assembly.GetType("Samples.NamespaceMembers", throwOnError: true)!;
        Assert.Equal(["block function", "file const"], GetMarkerLabels(samplesContainer.GetCustomAttributesData()));
        Assert.Empty(GetMarkerLabels(samplesContainer.GetMethod("BlockFunction")!.GetCustomAttributesData()));
        Assert.Empty(GetMarkerLabels(samplesContainer.GetField("FileAnswer")!.GetCustomAttributesData()));

        var emptyContainer = loaded.Assembly.GetType("Empty.NamespaceMembers", throwOnError: true)!;
        Assert.Equal(["empty namespace body"], GetMarkerLabels(emptyContainer.GetCustomAttributesData()));

        var outerContainer = loaded.Assembly.GetType("Outer.NamespaceMembers", throwOnError: true)!;
        Assert.Equal(["outer namespace"], GetMarkerLabels(outerContainer.GetCustomAttributesData()));

        var innerContainer = loaded.Assembly.GetType("Outer.Inner.NamespaceMembers", throwOnError: true)!;
        Assert.Equal(["inner namespace"], GetMarkerLabels(innerContainer.GetCustomAttributesData()));
        Assert.Empty(GetMarkerLabels(innerContainer.GetMethod("NestedFunction")!.GetCustomAttributesData()));
    }

    [Fact]
    public void NamespaceFunction_LambdaCapturesFunctionParameter()
    {
        const string source = """
namespace Utilities {
    public func AddOffset(value: int) -> int {
        let add = (offset: int) -> int => value + offset
        return add(2)
    }
}
""";

        var tree = SyntaxTree.ParseText(source);
        var compilation = Compilation.Create(
            "namespaceFunctionCapture",
            [tree],
            TestMetadataReferences.Default,
            new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));

        using var peStream = new MemoryStream();
        var result = compilation.Emit(peStream);
        Assert.True(result.Success, string.Join(Environment.NewLine, result.Diagnostics));

        using var loaded = TestAssemblyLoader.LoadFromStream(peStream, compilation.References);
        var container = loaded.Assembly.GetType("Utilities.NamespaceMembers", throwOnError: true)!;
        var method = container.GetMethod("AddOffset", BindingFlags.Public | BindingFlags.Static);

        Assert.NotNull(method);
        Assert.Equal(42, method!.Invoke(null, [40]));
    }

    [Fact]
    public void NamespaceFunction_ResultPropagationAndLambdasCaptureFunctionParameter_Emits()
    {
        const string source = """
import System.*
import System.Collections.Generic.*
import System.Linq.*

namespace Utilities {
    public func Find(value: string, values: IEnumerable<string>) -> Result<string, LookupError> {
        let selected = values.FirstOrError(
            candidate => candidate == value,
            () => LookupError(value))?

        return System.Result<string, LookupError>.Ok(selected)
    }

    record LookupError(Value: string)
}
""";

        var tree = SyntaxTree.ParseText(source);
        var compilation = Compilation.Create(
            "namespaceFunctionResultCapture",
            [tree],
            TestMetadataReferences.DefaultWithRavenCore,
            new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));

        using var peStream = new MemoryStream();
        var result = compilation.Emit(peStream);

        Assert.True(result.Success, string.Join(Environment.NewLine, result.Diagnostics));
    }

    private static string[] GetMarkerLabels(IList<CustomAttributeData> attributes)
        => attributes
            .Where(static attribute => attribute.AttributeType.Name == "MarkerAttribute")
            .Select(static attribute => Assert.IsType<string>(attribute.ConstructorArguments[0].Value))
            .ToArray();

    private static string[] GetDescriptionLabels(IList<CustomAttributeData> attributes)
        => attributes
            .Where(static attribute => attribute.AttributeType.Name == "DescriptionAttribute")
            .Select(static attribute => Assert.IsType<string>(attribute.ConstructorArguments[0].Value))
            .ToArray();

    private static string[] GetDescriptionLabels(IEnumerable<AttributeData> attributes)
        => attributes
            .Where(static attribute => attribute.AttributeClass?.Name == "DescriptionAttribute")
            .Select(static attribute => Assert.IsType<string>(attribute.ConstructorArguments[0].Value))
            .ToArray();
}
