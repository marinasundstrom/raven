using System.Linq;

using Raven.CodeAnalysis.Syntax;

using Xunit;

namespace Raven.CodeAnalysis.Semantics.Tests;

public sealed class SourceInterfaceOrderTests : CompilationTestBase
{
    [Fact]
    public void ConversionQueriedDuringDeclarationBinding_UsesCompletedInterfaceRelationships()
    {
        var tree = SyntaxTree.ParseText("""
            public class Items<T> : IList<T> { public init() {} }
            public interface IList<T> : ISequence<T> {}
            public interface ISequence<T> {}
            """);
        var compilation = CreateCompilation(tree);
        compilation.EnsureSourceTypeDeclarationsDeclared();
        Assert.True(compilation.TryGetDeclaredTypeSymbol("Items", 1, out var itemsDefinition));
        Assert.True(compilation.TryGetDeclaredTypeSymbol("ISequence", 1, out var sequenceDefinition));
        var element = compilation.GetSpecialType(SpecialType.System_Byte);
        var items = itemsDefinition.Construct(element);
        var sequence = sequenceDefinition.Construct(element);
        // Signature inference may query this relationship while declaration binding is in progress.
        _ = compilation.ClassifyConversion(items, sequence);
        compilation.EnsureSourceDeclarationsComplete();
        Assert.Empty(compilation.GetDiagnostics().Where(d => d.Severity == DiagnosticSeverity.Error));
        Assert.True(compilation.ClassifyConversion(items, sequence).IsImplicit);
    }

    [Theory]
    [InlineData(false)]
    [InlineData(true)]
    public void GenericInterfaceInitializer_DoesNotDependOnSourceOrder(bool emptyFirst)
    {
        var sources = new[]
        {
            "public class Storage { private var values: IList<byte> = Items<byte>() }",
            "public class Items<T> : IList<T> { public init() {} }",
            "public interface IList<T> : ISequence<T> {}",
            "public interface ISequence<T> {}"
        };
        var trees = sources.Select(source => SyntaxTree.ParseText(source)).ToList();
        if (emptyFirst)
            trees.Insert(0, SyntaxTree.ParseText("// No declarations."));
        var compilation = CreateCompilation(trees);
        _ = compilation.GetTypeByMetadataName("Storage");
        Assert.Empty(compilation.GetDiagnostics().Where(d => d.Severity == DiagnosticSeverity.Error));
        var items = compilation.GetTypeByMetadataName("Items`1")!.Construct(compilation.GetSpecialType(SpecialType.System_Byte));
        var sequence = compilation.GetTypeByMetadataName("ISequence`1")!.Construct(compilation.GetSpecialType(SpecialType.System_Byte));
        Assert.True(compilation.ClassifyConversion(items, sequence).IsImplicit);
    }
}
