using System.Reflection;
using System.Runtime.Loader;

using BoundNodeGenerator;

using Raven.CodeAnalysis;
using Raven.CodeAnalysis.Syntax;

namespace Raven.GeneratorParity.Tests;

[Trait("Target", "DotNet")]
public sealed class SymbolVisitorOutputTests
{
    private static readonly SymbolInterfaceModel[] Interfaces =
    [
        new("MethodSymbol", "Raven.CodeAnalysis"),
        new("IntersectionTypeSymbol", "Raven.CodeAnalysis"),
        new("FieldSymbol", "Raven.CodeAnalysis"),
    ];

    [Theory]
    [InlineData(false)]
    [InlineData(true)]
    public void RavenOutputIsDeterministicAndParses(bool generic)
    {
        var output = SymbolVisitorGenerator.GenerateRavenVisitor(Interfaces, generic);
        Assert.Equal(output, SymbolVisitorGenerator.GenerateRavenVisitor(Interfaces.Reverse(), generic));
        Assert.Empty(SyntaxTree.ParseText(output).GetDiagnostics().Where(d => d.Severity == DiagnosticSeverity.Error));
        Assert.Contains("VisitIntersectionType(symbol: IntersectionTypeSymbol)", output);
        Assert.DoesNotContain("Visitntersection", output);
        Assert.DoesNotContain("\n{", output);
        Assert.Empty(SyntaxTree.ParseText(SymbolVisitorGenerator.GenerateRavenVisitor([], generic))
            .GetDiagnostics().Where(d => d.Severity == DiagnosticSeverity.Error));
    }

    [Fact]
    public void GeneratedRavenVisitorsDispatchAndPreserveVirtualDefaults()
    {
        var sources = new[]
        {
            SymbolVisitorGenerator.GenerateRavenVisitor(Interfaces),
            SymbolVisitorGenerator.GenerateRavenVisitor(Interfaces, generic: true),
            """
            namespace Raven.CodeAnalysis

            public interface Symbol {
                func Accept(visitor: SymbolVisitor)
                func Accept<TResult>(visitor: SymbolVisitor<TResult>) -> TResult
            }
            public interface MethodSymbol : Symbol { }
            public interface FieldSymbol : Symbol { }
            public interface IntersectionTypeSymbol : Symbol { }

            public abstract partial class SymbolVisitor {
                virtual func Visit(symbol: Symbol) { if symbol is not null { symbol.Accept(self) } }
                virtual func DefaultVisit(symbol: Symbol) { }
            }
            public abstract partial class SymbolVisitor<TResult> {
                virtual func Visit(symbol: Symbol?) -> TResult? {
                    if symbol is Symbol value { return value.Accept(self) }
                    return default
                }
                virtual func DefaultVisit(symbol: Symbol) -> TResult => default!
            }
            public class SampleMethod : MethodSymbol {
                func Accept(visitor: SymbolVisitor) => visitor.VisitMethod(self)
                func Accept<TResult>(visitor: SymbolVisitor<TResult>) -> TResult => visitor.VisitMethod(self)
            }
            public class CountVisitor : SymbolVisitor {
                var Count: int { get; private set; } = 0
                override func DefaultVisit(symbol: Symbol) { Count = Count + 1 }
            }
            public class ResultVisitor : SymbolVisitor<string> {
                override func DefaultVisit(symbol: Symbol) -> string => "default"
                override func VisitMethod(symbol: MethodSymbol) -> string => "method"
            }
            public class IntVisitor : SymbolVisitor<int> { }
            public static class Probe {
                static func Count() -> int {
                    let visitor = CountVisitor()
                    visitor.Visit(SampleMethod())
                    visitor.VisitField(null!)
                    visitor.VisitIntersectionType(null!)
                    return visitor.Count
                }
                static func Result() -> string => ResultVisitor().Visit(SampleMethod())!
                static func DefaultResult() -> string => ResultVisitor().VisitField(null!)
                static func Absent() -> int => IntVisitor().Visit(null)
            }
            """
        };
        var runtime = Path.GetDirectoryName(typeof(object).Assembly.Location)!;
        var references = new[] { typeof(object).Assembly.Location, Path.Combine(runtime, "System.Runtime.dll") }
            .Select(path => MetadataReference.CreateFromFile(path)).ToArray();
        var compilation = Compilation.Create("SymbolVisitors" + Guid.NewGuid().ToString("N"),
            sources.Select(source => SyntaxTree.ParseText(source)).ToArray(), references,
            new CompilationOptions(OutputKind.DynamicallyLinkedLibrary).WithFrameworkProjectionMode(FrameworkProjectionMode.None));
        using var image = new MemoryStream();
        var result = compilation.Emit(image);
        Assert.True(result.Success, string.Join(Environment.NewLine, result.Diagnostics));
        image.Position = 0;
        var context = new AssemblyLoadContext("SymbolVisitorOutput", isCollectible: true);
        try
        {
            var assembly = context.LoadFromStream(image);
            var probe = assembly.GetType("Raven.CodeAnalysis.Probe")!;
            Assert.Equal(3, probe.GetMethod("Count")!.Invoke(null, null));
            Assert.Equal("method", probe.GetMethod("Result")!.Invoke(null, null));
            Assert.Equal("default", probe.GetMethod("DefaultResult")!.Invoke(null, null));
            Assert.Equal(0, probe.GetMethod("Absent")!.Invoke(null, null));
            foreach (var typeName in new[] { "SymbolVisitor", "SymbolVisitor`1" })
            {
                var visitor = assembly.GetType("Raven.CodeAnalysis." + typeName)!;
                foreach (var method in visitor.GetMethods(BindingFlags.Public | BindingFlags.Instance | BindingFlags.DeclaredOnly)
                    .Where(method => method.Name.StartsWith("Visit") && method.Name != "Visit"))
                {
                    Assert.True(method.IsVirtual);
                    Assert.Same(assembly, method.GetParameters().Single().ParameterType.Assembly);
                }
            }
        }
        finally { context.Unload(); }
    }
}
