using System.Reflection;

using Raven.CodeAnalysis.CodeGen.Portable;
using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Tests.CodeGen;

public class PortablePatternBodyTests
{
    [Theory]
    [InlineData("""
        public class Cell {
            val Value: int
            init(value: int) { Value = value }
        }
        public static class Consumer {
            public static func Read(input: Cell?) -> int {
                let first = if input is Cell value { value.Value } else { 1 }
                let second = if input is Cell value { value.Value } else { 1 }
                return first + second
            }
            public static func Run() -> int {
                if Read(null) != 2 { return 0 }
                return Read(Cell(21))
            }
        }
        """)]
    [InlineData("""
        public class Cell {
            val Value: int
            init(value: int) { Value = value }
        }
        public static class Consumer {
            public static func Read(value: Cell?) -> int {
                let copy: Cell? = value
                if copy is not null {
                    if copy is Cell cell { return cell.Value }
                }
                return 1
            }
            public static func Run() -> int {
                if Read(null) != 1 { return 0 }
                return Read(Cell(42))
            }
        }
        """)]
    [InlineData("""
        import Outcome.*
        public union Outcome<T> {
            case Completed(T)
            case Cancelled
        }
        public static class Consumer {
            public static func Read(value: Outcome<int>) -> int {
                if value is Cancelled { return 42 }
                return 0
            }
            public static func Run() -> int => Read(.Cancelled)
        }
        """)]
    public void PortablePatternsPreserveOrdinaryDotNetResults(string source)
    {
        var tree = SyntaxTree.ParseText(source);
        var compilation = Compilation.Create("PortablePatterns" + Guid.NewGuid().ToString("N"), [tree], TestMetadataReferences.Default,
            new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        Assert.DoesNotContain(compilation.GetDiagnostics(), d => d.Severity == DiagnosticSeverity.Error);
        var syntax = tree.GetRoot().DescendantNodes().OfType<MethodDeclarationSyntax>().Single(m => m.Identifier.ValueText == "Read");
        var symbol = (IMethodSymbol)compilation.GetSemanticModel(tree).GetDeclaredSymbol(syntax)!;
        var capabilities = new EmissionCapabilities(Enum.GetValues<EmissionPrimitiveType>(), Enum.GetValues<LinearInstructionKind>(),
            Enum.GetValues<EmissionDeclarationKind>(), Enum.GetValues<Accessibility>(), Enum.GetValues<Accessibility>(), Enum.GetValues<Accessibility>(),
            allowsRootClassLocals: true, allowsRootClassSignatures: true, allowsGenericClassOwners: true,
            allowsGenericStaticOwners: true, allowsGenericInstanceMethods: true, allowsConstructedFieldReferences: true,
            allowsManagedReferences: true, allowsCasePatterns: true);
        Assert.True(SourceCallablePlan.TryCreate(symbol, out var plan, capabilities));
        Assert.True(plan!.TryLowerBody(compilation, _ => false, out _, out var failure, capabilities), failure?.Detail);
        using var image = new MemoryStream();
        var emitted = compilation.Emit(image);
        Assert.True(emitted.Success, string.Join("\n", emitted.Diagnostics));
        Assert.Equal(42, Assembly.Load(image.ToArray()).GetType("Consumer")!.GetMethod("Run")!.Invoke(null, null));
    }
}
