using System.IO;
using System.Reflection;

using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Tests;

public class ExtensionIndexerCodeGenTests
{
    [Fact]
    public void GenericExtensionIndexer_ReadWrite_EmitsAndRuns()
    {
        const string source = """
import System.*
import System.Collections.Generic.*

extension Access<T> for List<T> {
    var self[index: Index]: T {
        get => self[index.GetOffset(self.Count)]
        set { self[index.GetOffset(self.Count)] = value }
    }
}

class Harness {
    public static func Run() -> int {
        let items = List<int>()
        items.Add(3)
        items[^1] = 42
        return items[^1]
    }
}
""";
        var references = TestMetadataReferences.Default;
        var tree = SyntaxTree.ParseText(source);
        Assert.Empty(tree.GetDiagnostics());
        Assert.Single(tree.GetRoot().DescendantNodes().OfType<IndexerDeclarationSyntax>());
        var compilation = Compilation.Create("ExtensionIndexers", new CompilationOptions(OutputKind.DynamicallyLinkedLibrary))
            .AddSyntaxTrees(tree).AddReferences(references);
        using var stream = new MemoryStream();
        var result = compilation.Emit(stream);
        Assert.True(result.Success, string.Join(Environment.NewLine, result.Diagnostics));
        using var loaded = TestAssemblyLoader.LoadFromStream(stream, references);
        Assert.Equal(42, loaded.Assembly.GetType("Harness")!.GetMethod("Run", BindingFlags.Public | BindingFlags.Static)!.Invoke(null, null));
    }

    [Theory]
    [InlineData(false)]
    [InlineData(true)]
    public void ExtensionIndexer_Overloads_RoundTrip(bool metadata)
    {
        const string library = """
import System.*
import System.Collections.Generic.*
public extension Access<T> for List<T> {
    var self[index: Index]: T {
        get => self[index.GetOffset(self.Count)]
        set { self[index.GetOffset(self.Count)] = value }
    }
    val self[range: Range]: int {
        get => range.GetOffsetAndLength(self.Count).Item2
    }
}
""";
        const string consumer = """
import System.*
import System.Collections.Generic.*
class Harness {
    public static func Run() -> int {
        let values = List<int>()
        values.Add(40)
        values.Add(0)
        values[^1] = 2
        return values[^2] + values[..]
    }
}
""";
        var references = TestMetadataReferences.Default;
        var compilation = Compilation.Create("ExtensionLibrary", new CompilationOptions(OutputKind.DynamicallyLinkedLibrary))
            .AddSyntaxTrees(SyntaxTree.ParseText(library)).AddReferences(references);
        using var libraryStream = new MemoryStream();
        if (metadata)
        {
            var libraryResult = compilation.Emit(libraryStream);
            Assert.True(libraryResult.Success, string.Join(Environment.NewLine, libraryResult.Diagnostics));
            var reference = MetadataReference.CreateFromImage(libraryStream.ToArray());
            compilation = Compilation.Create("ExtensionConsumer", new CompilationOptions(OutputKind.DynamicallyLinkedLibrary))
                .AddSyntaxTrees(SyntaxTree.ParseText(consumer)).AddReferences(references).AddReferences(reference);
        }
        else compilation = compilation.AddSyntaxTrees(SyntaxTree.ParseText(consumer));
        using var stream = new MemoryStream();
        var result = compilation.Emit(stream);
        Assert.True(result.Success, string.Join(Environment.NewLine, result.Diagnostics));
        using var loaded = TestAssemblyLoader.LoadFromStream(stream, references);
        if (metadata)
        {
            libraryStream.Position = 0;
            System.Runtime.Loader.AssemblyLoadContext.GetLoadContext(loaded.Assembly)!.LoadFromStream(libraryStream);
        }
        Assert.Equal(42, loaded.Assembly.GetType("Harness")!.GetMethod("Run")!.Invoke(null, null));
    }

    [Fact]
    public void ExtensionIndexer_SemanticModel_ReportsPropertyWithoutReceiverParameter()
    {
        const string source = """
extension Access for string {
    val self[index: bool]: int { get => if index { 1 } else { 2 } }
}
let value = "test"[true]
""";
        var tree = SyntaxTree.ParseText(source);
        var compilation = Compilation.Create("Semantic", new CompilationOptions(OutputKind.ConsoleApplication))
            .AddSyntaxTrees(tree).AddReferences(TestMetadataReferences.Default);
        Assert.Empty(compilation.GetDiagnostics());
        var model = compilation.GetSemanticModel(tree);
        var access = tree.GetRoot().DescendantNodes().OfType<ElementAccessExpressionSyntax>().Single();
        var symbol = Assert.IsAssignableFrom<IPropertySymbol>(model.GetSymbolInfo(access).Symbol);
        Assert.True(symbol.IsIndexer);
        Assert.True(symbol.IsExtensionProperty);
        Assert.Single(symbol.Parameters);
        var operation = Assert.IsAssignableFrom<Raven.CodeAnalysis.Operations.IElementAccessOperation>(model.GetOperation(access));
        Assert.Equal(symbol, operation.Indexer);
    }

    [Fact]
    public void ExtensionIndexer_CompoundAssignment_EvaluatesOperandsOnce()
    {
        const string source = """
import System.Collections.Generic.*
extension Access for List<int> {
    var self[index: bool]: int {
        get => self[0]
        set { self[0] = value }
    }
    val self[index: int]: int { get => 999 }
}
class Observer {
    val Items: List<int> = List<int>()
    var Calls: int = 0
    func Read() -> List<int> { Calls = Calls * 10 + 1; return Items }
    func Index() -> bool { Calls = Calls * 10 + 2; return true }
    func Value() -> int { Calls = Calls * 10 + 3; return 40 }
}
class Harness {
    public static func Run() -> int {
        let observed = Observer()
        observed.Items.Add(2)
        observed.Read()[observed.Index()] += observed.Value()
        if observed.Calls != 123 { return -1 }
        return observed.Items[0]
    }
}
""";
        var references = TestMetadataReferences.Default;
        var compilation = Compilation.Create("Evaluation", new CompilationOptions(OutputKind.DynamicallyLinkedLibrary))
            .AddSyntaxTrees(SyntaxTree.ParseText(source)).AddReferences(references);
        using var stream = new MemoryStream();
        var result = compilation.Emit(stream);
        Assert.True(result.Success, string.Join(Environment.NewLine, result.Diagnostics));
        using var loaded = TestAssemblyLoader.LoadFromStream(stream, references);
        Assert.Equal(42, loaded.Assembly.GetType("Harness")!.GetMethod("Run")!.Invoke(null, null));
    }

    [Theory]
    [InlineData("extension A for string { val self[index: bool]: int { get => 1 } }\nextension B for string { val self[index: bool]: int { get => 2 } }\nlet value = \"x\"[true]")]
    [InlineData("extension A for string { val self[index: bool]: int { get => 1 } }\n\"x\"[true] = 3")]
    [InlineData("extension A for string { var self[index: bool]: int { get => 1; private set { } } }\n\"x\"[true] = 3")]
    [InlineData("class C { val self[index: bool]: int => 1 }\nextension A for C { var self[index: bool]: int { get => 2; set { } } }\nC()[true] = 3")]
    [InlineData("extension A for string { static val self[index: bool]: int => 1 }")]
    public void ExtensionIndexer_InvalidAccess_ProducesDiagnostics(string source)
    {
        var compilation = Compilation.Create("Invalid", new CompilationOptions(OutputKind.ConsoleApplication))
            .AddSyntaxTrees(SyntaxTree.ParseText(source)).AddReferences(TestMetadataReferences.Default);
        Assert.Contains(compilation.GetDiagnostics(), d => d.Severity == DiagnosticSeverity.Error);
    }
}
