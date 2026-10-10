using System.Reflection;

using Raven.CodeAnalysis.CodeGen.Portable;
using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Tests.CodeGen;

public class PortablePatternBodyTests
{
    [Fact]
    public void PortableReferenceFieldReturnUsesEmptyStackBoundary()
    {
        const string source = """
        public class Cell {
            public field Value: int
        }
        public static class Consumer {
            public static func Read(input: Cell?) -> int {
                let cell = Cell()
                cell.Value = {
                    if input is null { return 42 }
                    1
                }
                return cell.Value
            }
            public static func Run() -> int {
                if Read(Cell()) != 1 { return 0 }
                return Read(null)
            }
        }
        """;
        var tree = SyntaxTree.ParseText(source);
        var compilation = Compilation.Create("FieldReturn", [tree], TestMetadataReferences.Default,
            new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        Assert.DoesNotContain(compilation.GetDiagnostics(), d => d.Severity == DiagnosticSeverity.Error);
        var syntax = tree.GetRoot().DescendantNodes().OfType<MethodDeclarationSyntax>().Single(m => m.Identifier.ValueText == "Read");
        var symbol = (IMethodSymbol)compilation.GetSemanticModel(tree).GetDeclaredSymbol(syntax)!;
        var capabilities = new EmissionCapabilities(Enum.GetValues<EmissionPrimitiveType>(), Enum.GetValues<LinearInstructionKind>(),
            Enum.GetValues<EmissionDeclarationKind>(), Enum.GetValues<Accessibility>(), Enum.GetValues<Accessibility>(), Enum.GetValues<Accessibility>(),
            allowsRootClassLocals: true, allowsRootClassSignatures: true, allowsCasePatterns: true);
        Assert.True(SourceCallablePlan.TryCreate(symbol, out var plan, capabilities));
        Assert.True(plan!.TryLowerBody(compilation, _ => false, out _, out var failure, capabilities), failure?.Detail);
    }

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
    [InlineData("""
        public static class Consumer {
            public static func Read(value: long) -> int {
                if value is 4294967296L { return 42 }
                if value is 0L { return 1 }
                return 2
            }
            public static func Run() -> int {
                if Read(0L) != 1 || Read(4294967297L) != 2 { return 0 }
                return Read(4294967296L)
            }
        }
        """)]
    [InlineData("""
        public static class Consumer {
            public static func Read(value: string?) -> int {
                if value is "hello" { return 42 }
                if value is "" { return 1 }
                return 2
            }
            public static func Run() -> int {
                if Read(null) != 2 || Read("other") != 2 || Read("") != 1 { return 0 }
                return Read(string.Concat("hel", "lo"))
            }
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
