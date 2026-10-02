using System.Reflection;

using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Tests.Semantics.Diagnostics;

public class WriteOnlyIndexerTests
{
    [Theory]
    [InlineData("value[2] = 40", false)]
    [InlineData("let result = value[2]", true)]
    [InlineData("value[2] += 40", true)]
    [InlineData("value[2]++", true)]
    public void ImportedWriteOnlyIndexerChecksRequiredAccessor(string statement, bool rejects)
    {
        var tree = SyntaxTree.ParseText("""
            import Raven.CodeAnalysis.Tests.Semantics.Diagnostics.*
            public func Update(value: WriteOnlyIndexerFixture) -> int {
            """ + "\n" + statement + "\nreturn value.Result\n}");
        var reference = MetadataReference.CreateFromFile(typeof(WriteOnlyIndexerFixture).Assembly.Location);
        var compilation = Compilation.Create("WriteOnlyConsumer", [tree], [.. TestMetadataReferences.Default, reference],
            new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        var errors = compilation.GetDiagnostics().Where(d => d.Severity == DiagnosticSeverity.Error).ToArray();
        if (rejects) { Assert.NotEmpty(errors); return; }
        Assert.Empty(errors);
        var owner = ((IAssemblySymbol)compilation.GetAssemblyOrModuleSymbol(reference)!).GetTypeByMetadataName(typeof(WriteOnlyIndexerFixture).FullName!)!;
        var indexer = owner.GetMembers().OfType<IPropertySymbol>().Single(p => p.IsIndexer);
        Assert.Null(indexer.GetMethod);
        Assert.Single(indexer.Parameters);
        Assert.Equal(SpecialType.System_Int32, indexer.Parameters[0].Type.SpecialType);
        using var stream = new MemoryStream();
        var emitted = compilation.Emit(stream);
        Assert.True(emitted.Success, string.Join("; ", emitted.Diagnostics));
        var method = Assembly.Load(stream.ToArray()).GetTypes().SelectMany(t => t.GetMethods()).Single(m => m.Name == "Update");
        Assert.Equal(42, method.Invoke(null, [new WriteOnlyIndexerFixture()]));
    }
}

public class WriteOnlyIndexerFixture
{
    public int Result { get; private set; }
    public int this[int index] { set => Result = index + value; }
}
