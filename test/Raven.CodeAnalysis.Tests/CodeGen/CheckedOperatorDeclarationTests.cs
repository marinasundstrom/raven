using System;
using System.IO;
using System.Linq;

using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Tests;

public class CheckedOperatorDeclarationTests
{
    [Theory]
    [InlineData("static ", "+", "left: Box, right: Box", "Box", "left", "right", "Addition")]
    [InlineData("static ", "-", "left: Box, right: Box", "Box", "left", "right", "Subtraction")]
    [InlineData("static ", "*", "left: Box, right: Box", "Box", "left", "right", "Multiply")]
    [InlineData("static ", "/", "left: Box, right: Box", "Box", "left", "right", "Division")]
    [InlineData("static ", "-", "value: Box", "Box", "value", "Box()", "UnaryNegation")]
    [InlineData("static ", "++", "value: Box", "Box", "value", "Box()", "Increment")]
    [InlineData("static ", "--", "value: Box", "Box", "value", "Box()", "Decrement")]
    [InlineData("static ", "explicit", "value: Box", "int", "1", "42", "Explicit")]
    [InlineData("", "+=", "right: Box", "unit", "{ State = 1 }", "{ State = 42 }", "AdditionAssignment")]
    [InlineData("", "-=", "right: Box", "unit", "{ State = 1 }", "{ State = 42 }", "SubtractionAssignment")]
    [InlineData("", "*=", "right: Box", "unit", "{ State = 1 }", "{ State = 42 }", "MultiplicationAssignment")]
    [InlineData("", "/=", "right: Box", "unit", "{ State = 1 }", "{ State = 42 }", "DivisionAssignment")]
    [InlineData("", "++", "", "unit", "{ State = 1 }", "{ State = 42 }", "IncrementAssignment")]
    [InlineData("", "--", "", "unit", "{ State = 1 }", "{ State = 42 }", "DecrementAssignment")]
    public void CheckedDeclarationsPreserveDistinctSourceAndImportedMetadata(
        string modifier, string token, string parameters, string result, string ordinaryBody, string checkedBody, string name)
    {
        var compilation = Create($$"""
            public class Box {
                public var State: int = 0
                {{modifier}}func {{token}}({{parameters}}) -> {{result}} => {{ordinaryBody}}
                {{modifier}}func checked {{token}}({{parameters}}) -> {{result}} => {{checkedBody}}
            }
            """);
        var tree = compilation.SyntaxTrees.Single();
        var model = compilation.GetSemanticModel(tree);
        var declaration = tree.GetRoot().DescendantNodes().OfType<BaseMethodDeclarationSyntax>().Last();
        var symbol = Assert.IsAssignableFrom<IMethodSymbol>(model.GetDeclaredSymbol(declaration));
        Assert.Equal("op_Checked" + name, symbol.MetadataName);
        Assert.Equal(name == "Explicit" ? MethodKind.Conversion : MethodKind.UserDefinedOperator, symbol.MethodKind);
        Assert.Contains("checked", symbol.ToDisplayString());

        using var stream = new MemoryStream();
        var emitted = compilation.Emit(stream);
        Assert.True(emitted.Success, string.Join(Environment.NewLine, emitted.Diagnostics));
        using var loaded = TestAssemblyLoader.LoadFromStream(stream, TestMetadataReferences.Default);
        var type = loaded.Assembly.GetType("Box")!;
        var ordinaryMethod = type.GetMethod("op_" + name)!;
        var checkedMethod = type.GetMethod("op_Checked" + name)!;
        Assert.True(ordinaryMethod.IsSpecialName);
        Assert.True(checkedMethod.IsSpecialName);
        Assert.Equal(modifier.Length != 0, checkedMethod.IsStatic);
        var receiver = Activator.CreateInstance(type)!;
        var arguments = checkedMethod.GetParameters().Select(_ => Activator.CreateInstance(type)!).ToArray();
        var ordinaryResult = ordinaryMethod.Invoke(checkedMethod.IsStatic ? null : receiver, arguments);
        var checkedResult = checkedMethod.Invoke(checkedMethod.IsStatic ? null : receiver, arguments);
        if (result == "unit")
            Assert.Equal(42, type.GetProperty("State")!.GetValue(receiver));
        else if (result == "int")
        {
            Assert.Equal(1, ordinaryResult);
            Assert.Equal(42, checkedResult);
        }
        else
        {
            Assert.Same(arguments[0], ordinaryResult);
            if (arguments.Length == 2)
                Assert.Same(arguments[1], checkedResult);
            else
                Assert.NotSame(arguments[0], checkedResult);
        }

        var consumer = Create("class Consumer { }").AddReferences(MetadataReference.CreateFromImage(stream.ToArray()));
        var imported = Assert.Single(consumer.GetTypeByMetadataName("Box")!.GetMembers("op_Checked" + name).OfType<IMethodSymbol>());
        Assert.Equal(symbol.MethodKind, imported.MethodKind);
        Assert.Contains("checked", imported.ToDisplayString());
    }

    [Theory]
    [InlineData("static func checked +(value: Box) -> Box => value", "RAV0371")]
    [InlineData("static func checked %(a: Box, b: Box) -> Box => a", "RAV0371")]
    [InlineData("static func checked implicit(value: Box) -> int => 1", "RAV0371")]
    [InlineData("func checked %=(value: Box) { }", "RAV0371")]
    [InlineData("static func checked +(a: Box, b: Box) -> Box => a", "RAV0372")]
    [InlineData("static func checked explicit(value: Box) -> int => 1", "RAV0372")]
    [InlineData("func checked ++() { }", "RAV0372")]
    [InlineData("static func +(a: Box, b: int) -> Box => a\nstatic func checked +(a: Box, b: Box) -> Box => a", "RAV0372")]
    [InlineData("static func explicit(value: Box) -> long => 1\nstatic func checked explicit(value: Box) -> int => 1", "RAV0372")]
    public void UnsupportedOrUnpairedCheckedDeclarationsAreRejected(string member, string diagnostic)
    {
        Assert.Contains(Create($"class Box {{ {member} }}").GetDiagnostics(), d => d.Id == diagnostic);
    }

    [Fact]
    public void GenericCounterpartCanFollowCheckedDeclarationInAnotherPartialPart()
    {
        var compilation = Create("""
            partial class Box<T> {
                static func checked +(a: Box<T>, b: Box<T>) -> Box<T> => b
            }
            partial class Box<T> {
                static func +(left: Box<T>, right: Box<T>) -> Box<T> => left
            }
            """);
        Assert.DoesNotContain(compilation.GetDiagnostics(), d => d.Severity == DiagnosticSeverity.Error);
    }

    [Fact]
    public void OrdinaryExpressionsStillSelectOrdinaryOperatorsAndConversions()
    {
        var compilation = Create("""
            class Box {
                static func +(a: Box, b: Box) -> Box => a
                static func checked +(a: Box, b: Box) -> Box => b
                static func explicit(value: Box) -> int => 42
                static func checked explicit(value: Box) -> int => 1
            }
            class Runner {
                static func Run() -> int => (int)(Box() + Box())
                static func checked(value: int) -> int => value
            }
            """);
        using var stream = new MemoryStream();
        var result = compilation.Emit(stream);
        Assert.True(result.Success, string.Join(Environment.NewLine, result.Diagnostics));
        using var loaded = TestAssemblyLoader.LoadFromStream(stream, TestMetadataReferences.Default);
        Assert.Equal(42, loaded.Assembly.GetType("Runner")!.GetMethod("Run")!.Invoke(null, null));
    }

    [Fact]
    public void CSharpConsumerSelectsExportedCheckedOperators()
    {
        using var library = new MemoryStream();
        var provider = Create("""
            public class Box {
                public var State: int = 0
                static func +(a: Box, b: Box) -> Box => a
                static func checked +(a: Box, b: Box) -> Box => b
                static func explicit(value: Box) -> int => 1
                static func checked explicit(value: Box) -> int => 42
                func +=(value: Box) { State = 1 }
                func checked +=(value: Box) { State = 42 }
                func ++() { State = 1 }
                func checked ++() { State = 42 }
            }
            """).Emit(library);
        Assert.True(provider.Success, string.Join(Environment.NewLine, provider.Diagnostics));
        var paths = TargetFrameworkResolver.GetReferenceAssemblies(TargetFrameworkResolver.ResolveVersion("net11.0"));
        var consumer = Microsoft.CodeAnalysis.CSharp.CSharpCompilation.Create("CheckedConsumer",
            [Microsoft.CodeAnalysis.CSharp.CSharpSyntaxTree.ParseText("""
                public class Runner {
                    public static int Run() {
                        var a = new Box();
                        var b = new Box();
                        if (!object.ReferenceEquals(a + b, a)) return 1;
                        if (!object.ReferenceEquals(checked(a + b), b)) return 2;
                        if ((int)a != 1 || checked((int)a) != 42) return 3;
                        return 42;
                    }
                }
                """, new Microsoft.CodeAnalysis.CSharp.CSharpParseOptions(Microsoft.CodeAnalysis.CSharp.LanguageVersion.Preview))],
            paths.Select(p => Microsoft.CodeAnalysis.MetadataReference.CreateFromFile(p))
                .Append(Microsoft.CodeAnalysis.MetadataReference.CreateFromImage(library.ToArray())),
            new Microsoft.CodeAnalysis.CSharp.CSharpCompilationOptions(Microsoft.CodeAnalysis.OutputKind.DynamicallyLinkedLibrary));
        using var output = new MemoryStream();
        var emitted = consumer.Emit(output);
        Assert.True(emitted.Success, string.Join(Environment.NewLine, emitted.Diagnostics));
        var context = new System.Runtime.Loader.AssemblyLoadContext("CheckedConsumer", isCollectible: true);
        try
        {
            library.Position = 0;
            context.LoadFromStream(library);
            output.Position = 0;
            var assembly = context.LoadFromStream(output);
            Assert.Equal(42, assembly.GetType("Runner")!.GetMethod("Run")!.Invoke(null, null));
        }
        finally { context.Unload(); }
    }

    private static Compilation Create(string source) => Compilation.Create("CheckedDeclarations" + Guid.NewGuid().ToString("N"),
        new CompilationOptions(OutputKind.DynamicallyLinkedLibrary))
        .AddSyntaxTrees(SyntaxTree.ParseText(source)).AddReferences(TestMetadataReferences.Default);
}
