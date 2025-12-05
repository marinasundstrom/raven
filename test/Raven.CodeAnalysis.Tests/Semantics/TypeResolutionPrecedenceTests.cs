using System;
using System.IO;
using System.Linq;

using Raven.CodeAnalysis;
using Raven.CodeAnalysis.Symbols;
using Raven.CodeAnalysis.Syntax;
using Xunit;

namespace Raven.CodeAnalysis.Semantics.Tests;

public class TypeResolutionPrecedenceTests : CompilationTestBase
{
    [Fact]
    public void LocalType_BindsAheadOfImportedMetadataType()
    {
        const string metadataSource = """
namespace System {
    public union Result<T> {
        Ok(value: T)
        Error()
    }
}
""";

        var metadataTree = SyntaxTree.ParseText(metadataSource);
        var metadataCompilation = CreateCompilation(
            metadataTree,
            options: new CompilationOptions(OutputKind.DynamicallyLinkedLibrary),
            assemblyName: "MetadataResult");

        metadataCompilation.EnsureSetup();

        using var metadataStream = new MemoryStream();
        var metadataEmit = metadataCompilation.Emit(metadataStream);
        Assert.True(metadataEmit.Success, string.Join(Environment.NewLine, metadataEmit.Diagnostics.Select(d => d.ToString())));

        var metadataReference = MetadataReference.CreateFromImage(metadataStream.ToArray());

        const string source = """
import System.*

class IntHelpers {
    ParseNumber(value: int) -> Result<int> {
        return .Ok(value)
    }
}

public union Result<T> {
    Ok(value: T)
    Error(message: string)
}
""";

        var (compilation, tree) = CreateCompilation(
            source,
            references: [.. TestMetadataReferences.Default, metadataReference]);

        compilation.EnsureSetup();

        var diagnostics = compilation.GetDiagnostics();
        Assert.True(diagnostics.IsEmpty, string.Join(Environment.NewLine, diagnostics.Select(d => d.ToString())));

        var model = compilation.GetSemanticModel(tree);
        var unionDeclaration = tree.Root.DescendantNodes().OfType<UnionDeclarationSyntax>().Single(u => u.Identifier.ValueText == "Result");
        var unionSymbol = Assert.IsAssignableFrom<INamedTypeSymbol>(model.GetDeclaredSymbol(unionDeclaration));

        var methodDeclaration = tree.Root.DescendantNodes().OfType<MethodDeclarationSyntax>().Single(m => m.Identifier.ValueText == "ParseNumber");
        var methodSymbol = Assert.IsAssignableFrom<IMethodSymbol>(model.GetDeclaredSymbol(methodDeclaration));

        var returnType = Assert.IsAssignableFrom<INamedTypeSymbol>(methodSymbol.ReturnType);
        Assert.True(SymbolEqualityComparer.Default.Equals(returnType.OriginalDefinition, unionSymbol));
        Assert.Collection(
            returnType.TypeArguments,
            arg => Assert.Equal(SpecialType.System_Int32, arg.SpecialType));
    }
}
