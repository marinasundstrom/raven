using System;
using System.IO;
using System.Linq;

using Raven.CodeAnalysis;
using Raven.CodeAnalysis.Symbols;
using Raven.CodeAnalysis.Syntax;
using Raven.CodeAnalysis.Tests;

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
    func ParseNumber(value: int) -> Result<int> {
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
            options: new CompilationOptions(OutputKind.DynamicallyLinkedLibrary),
            references: [.. TestMetadataReferences.Default, metadataReference]);

        compilation.EnsureSetup();

        var diagnostics = compilation.GetDiagnostics();
        Assert.True(diagnostics.IsEmpty, string.Join(Environment.NewLine, diagnostics.Select(d => d.ToString())));

        var model = compilation.GetSemanticModel(tree);
        var root = tree.GetRoot();
        var unionDeclaration = root.DescendantNodes().OfType<UnionDeclarationSyntax>().Single(u => u.Identifier.ValueText == "Result");
        var unionSymbol = Assert.IsAssignableFrom<INamedTypeSymbol>(model.GetDeclaredSymbol(unionDeclaration));

        var methodDeclaration = root.DescendantNodes().OfType<MethodDeclarationSyntax>().Single(m => m.Identifier.ValueText == "ParseNumber");
        var methodSymbol = Assert.IsAssignableFrom<IMethodSymbol>(model.GetDeclaredSymbol(methodDeclaration));

        var returnType = Assert.IsAssignableFrom<INamedTypeSymbol>(methodSymbol.ReturnType);
        Assert.True(SymbolEqualityComparer.Default.Equals(returnType.OriginalDefinition, unionSymbol));
        Assert.Collection(
            returnType.TypeArguments,
            arg => Assert.Equal(SpecialType.System_Int32, arg.SpecialType));
    }

    [Fact]
    public void LocalUnionCases_UseLocalUnionConstructorsWhenMetadataTypeIsImported()
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
    func ParseNumber(value: int) -> Result<int> {
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
            options: new CompilationOptions(OutputKind.DynamicallyLinkedLibrary),
            references: [.. TestMetadataReferences.Default, metadataReference]);

        compilation.EnsureSetup();

        var model = compilation.GetSemanticModel(tree);
        var root = tree.GetRoot();
        var unionDeclaration = root.DescendantNodes().OfType<UnionDeclarationSyntax>().Single(u => u.Identifier.ValueText == "Result");
        var unionSymbol = Assert.IsAssignableFrom<INamedTypeSymbol>(model.GetDeclaredSymbol(unionDeclaration));
        var constructedUnion = Assert.IsAssignableFrom<INamedTypeSymbol>(
            unionSymbol.Construct(compilation.GetSpecialType(SpecialType.System_Int32)));
        var okCase = Assert.IsAssignableFrom<INamedTypeSymbol>(constructedUnion.LookupType("Ok"));
        Assert.NotNull(okCase.TryGetDiscriminatedUnionCase());
        Assert.NotNull(constructedUnion.TryGetDiscriminatedUnion());
        var unionConstructors = constructedUnion.Constructors
            .Where(ctor => !ctor.IsStatic && ctor.Parameters.Length == 1)
            .ToArray();
        Assert.NotEmpty(unionConstructors);
        Assert.Contains(unionConstructors, ctor => SymbolEqualityComparer.Default.Equals(ctor.Parameters.Single().Type, okCase));

        var conversion = compilation.ClassifyConversion(okCase, constructedUnion);

        Assert.True(conversion.Exists);
        Assert.True(conversion.IsDiscriminatedUnion);
        Assert.False(conversion.IsUserDefined);
        Assert.Null(conversion.MethodSymbol);
        Assert.NotNull(conversion.ConstructorSymbol);

    }
}
