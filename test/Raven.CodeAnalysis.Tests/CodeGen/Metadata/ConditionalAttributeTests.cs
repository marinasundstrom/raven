using System;
using System.IO;
using System.Linq;
using System.Reflection;

using Raven.CodeAnalysis.Syntax;

using Xunit;

namespace Raven.CodeAnalysis.Tests.CodeGen;

public class ConditionalAttributeTests
{
    private static Compilation Compile(string source, params string[] symbols)
        => Compilation.Create("Conditional_" + Guid.NewGuid().ToString("N"),
            [SyntaxTree.ParseText(source, new ParseOptions().WithPreprocessorSymbols(symbols))],
            TestMetadataReferences.Default, new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));

    private static byte[] Emit(Compilation compilation)
    {
        using var output = new MemoryStream();
        var result = compilation.Emit(output);
        Assert.True(result.Success, string.Join(Environment.NewLine, result.Diagnostics));
        return output.ToArray();
    }

    [Theory]
    [InlineData(false, 0)]
    [InlineData(true, 1)]
    public void OverridesInheritConditions(bool enabled, int expected)
    {
        var compilation = Compile("""
            public open class Base {
                [System.Diagnostics.Conditional("ON")]
                public virtual func Record() { }
            }
            public class Probe : Base {
                private static var count: int = 0
                public override func Record() { count += 1 }
                public static func Run() -> int { Probe().Record(); return count }
            }
            """, enabled ? ["ON"] : []);
        var assembly = Assembly.Load(Emit(compilation));
        Assert.Equal(expected, assembly.GetType("Probe")!.GetMethod("Run")!.Invoke(null, null));
    }

    [Theory]
    [InlineData("Create().Record(Next())", false, 0)]
    [InlineData("Create().Record(Next())", true, 111)]
    [InlineData("CreateNullable()?.Record(Next())", false, 0)]
    [InlineData("CreateNullable()?.Record(Next())", true, 111)]
    [InlineData("Next() |> RecordStatic()", false, 0)]
    [InlineData("Next() |> RecordStatic()", true, 11)]
    public void ReceiversConditionalAccessAndPipelinesRespectOmission(string call, bool enabled, int expected)
    {
        var compilation = Compile($$"""
            public class Probe {
                private static var count: int = 0
                private static func Next() -> int { count += 1; return count }
                private static func Create() -> Probe { count += 100; return Probe() }
                private static func CreateNullable() -> Probe? { count += 100; return Probe() }
                [System.Diagnostics.Conditional("ON")]
                public func Record(value: int) { count += 10 }
                [System.Diagnostics.Conditional("ON")]
                public static func RecordStatic(value: int) { count += 10 }
                public static func Run() -> int { {{call}}; return count }
            }
            """, enabled ? ["ON"] : []);
        var assembly = Assembly.Load(Emit(compilation));
        Assert.Equal(expected, assembly.GetType("Probe")!.GetMethod("Run")!.Invoke(null, null));
    }

    [Theory]
    [InlineData(false)]
    [InlineData(true)]
    public void ImportedMethodUsesCallerSymbols(bool enabled)
    {
        var library = Compile("""
            public class Library {
                [System.Diagnostics.Conditional("CLIENT")]
                public static func Check() { System.Console.WriteLine("called") }
            }
            """);
        var caller = Compile("public class Probe { public static func Run() { Library.Check() } }",
            enabled ? ["CLIENT"] : []);
        caller = caller.AddReferences(MetadataReference.CreateFromImage(Emit(library)));
        using var image = new MemoryStream(Emit(caller));
        using var assembly = Mono.Cecil.AssemblyDefinition.ReadAssembly(image);
        // Check the retained dependency, without constraining the lowering instruction sequence.
        var references = assembly.MainModule.GetMemberReferences();
        Assert.Equal(enabled, references.Any(m => m.DeclaringType.Name == "Library" && m.Name == "Check"));
    }

    [Theory]
    [InlineData(false, 0)]
    [InlineData(true, 11)]
    public void CallerSymbolControlsInvocationAndArgumentSideEffects(bool enabled, int expected)
    {
        var compilation = Compile("""
            import System.Diagnostics.*
            public class Probe {
                private static var count: int = 0
                private static func Next() -> int { count += 1; return count }
                [Conditional("TRACE_ERRORS")]
                [Conditional("DEBUG")]
                public static func Record(value: int) { count += 10 }
                public static func Run() -> int { Record(Next()); return count }
            }
            """, enabled ? ["DEBUG"] : []);
        var tree = compilation.SyntaxTrees.Single();
        var invocation = tree.GetRoot().DescendantNodes().OfType<InvocationExpressionSyntax>()
            .Single(n => n.Expression.ToString() == "Record");
        Assert.Equal("Record", compilation.GetSemanticModel(tree).GetSymbolInfo(invocation).Symbol?.Name);
        var assembly = Assembly.Load(Emit(compilation));
        Assert.Equal(expected, assembly.GetType("Probe")!.GetMethod("Run")!.Invoke(null, null));
        Assert.Equal(2, assembly.GetType("Probe")!.GetMethod("Record")!.GetCustomAttributesData().Count(
            a => a.AttributeType.FullName == "System.Diagnostics.ConditionalAttribute"));
    }

    [Theory]
    [InlineData(false)]
    [InlineData(true)]
    public void ConditionalAttributeClassesRemainBoundButEmissionUsesApplicationSymbols(bool enabled)
    {
        var compilation = Compile("""
            import System.*
            import System.Diagnostics.*
            [Conditional("META")]
            public class MarkerAttribute : Attribute { public init() { } }
            [Marker]
            public class Item { }
            """, enabled ? ["META"] : []);
        Assert.Contains(compilation.GetTypeByMetadataName("Item")!.GetAttributes(),
            a => a.AttributeClass.Name == "MarkerAttribute");
        var assembly = Assembly.Load(Emit(compilation));
        Assert.Equal(enabled, assembly.GetType("Item")!.GetCustomAttributesData().Any(
            a => a.AttributeType.Name == "MarkerAttribute"));
    }

    [Theory]
    [InlineData("[System.Diagnostics.Conditional(\"X\")] public static func Bad() -> int => 1")]
    [InlineData("[System.Diagnostics.Conditional(\"not a symbol\")] public static func Bad() { }")]
    public void InvalidDeclarationsAreDiagnosed(string member)
    {
        var compilation = Compile("public class Probe { " + member + " }");
        Assert.Contains(compilation.GetDiagnostics(), d => d.Id == "RAV0506");
    }

    [Fact]
    public void OmittedArgumentsAreStillTypeChecked()
    {
        var compilation = Compile("""
            public class Probe {
                [System.Diagnostics.Conditional("UNDEFINED")]
                public static func Record(value: int) { }
                public static func Run() { Record("invalid") }
            }
            """);
        Assert.Contains(compilation.GetDiagnostics(), d => d.Severity == DiagnosticSeverity.Error);
    }

    [Fact]
    public void ConditionalMethodCannotEscapeThroughDelegate()
    {
        var compilation = Compile("""
            public class Probe {
                [System.Diagnostics.Conditional("UNDEFINED")]
                public static func Record() { }
                public static func Run() { let callback: System.Action = Record }
            }
            """);
        Assert.Contains(compilation.GetDiagnostics(), d => d.Id == "RAV0506");
    }
}
