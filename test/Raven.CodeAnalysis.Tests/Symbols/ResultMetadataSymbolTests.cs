using System;
using System.IO;
using System.Linq;

using Raven.CodeAnalysis;
using Raven.CodeAnalysis.Symbols;
using Raven.CodeAnalysis.Syntax;
using Raven.CodeAnalysis.Tests;

namespace Raven.CodeAnalysis.Tests;

public class ResultMetadataSymbolTests
{
    [Fact]
    public void ResultFromMetadata_ExposesCaseTypesAndTryGetParameters()
    {
        var (reference, path) = CreateRavenCoreResultReference();
        try
        {
            var compilation = Compilation.Create("metadata-result", new CompilationOptions(OutputKind.ConsoleApplication))
                .AddReferences([.. TestMetadataReferences.Default, reference]);

            var systemNamespace = compilation.GlobalNamespace.LookupNamespace("System")
                ?? compilation.GlobalNamespace;
            var resultDefinition = systemNamespace
                .GetMembers("Result")
                .OfType<INamedTypeSymbol>()
                .Single(symbol => symbol.Arity == 2);

            var okCase = Assert.Single(resultDefinition.GetMembers("Ok").OfType<INamedTypeSymbol>());
            var errorCase = Assert.Single(resultDefinition.GetMembers("Error").OfType<INamedTypeSymbol>());

            var tryGetOk = Assert.Single(resultDefinition.GetMembers("TryGetOk").OfType<IMethodSymbol>());
            var tryGetError = Assert.Single(resultDefinition.GetMembers("TryGetError").OfType<IMethodSymbol>());

            var okParameter = Assert.Single(tryGetOk.Parameters);
            var errorParameter = Assert.Single(tryGetError.Parameters);

            var okByRef = Assert.IsType<ByRefTypeSymbol>(okParameter.Type);
            var errorByRef = Assert.IsType<ByRefTypeSymbol>(errorParameter.Type);

            Assert.True(SymbolEqualityComparer.Default.Equals(okCase, okByRef.ElementType));
            Assert.True(SymbolEqualityComparer.Default.Equals(errorCase, errorByRef.ElementType));
        }
        finally
        {
            if (File.Exists(path))
                File.Delete(path);
        }
    }

    [Fact]
    public void ConstructedResultFromMetadata_SubstitutesCaseTypesAndTryGetParameters()
    {
        var (reference, path) = CreateRavenCoreResultReference();
        try
        {
            var compilation = Compilation.Create("metadata-result-constructed", new CompilationOptions(OutputKind.ConsoleApplication))
                .AddReferences([.. TestMetadataReferences.Default, reference]);

            var systemNamespace = compilation.GlobalNamespace.LookupNamespace("System")
                ?? compilation.GlobalNamespace;
            var resultDefinition = systemNamespace
                .GetMembers("Result")
                .OfType<INamedTypeSymbol>()
                .Single(symbol => symbol.Arity == 2);

            var stringType = compilation.GetSpecialType(SpecialType.System_String);
            var exceptionType = compilation.GetTypeByMetadataName("System.Exception")!;
            var constructedResult = Assert.IsAssignableFrom<INamedTypeSymbol>(resultDefinition.Construct(stringType, exceptionType));

            var okCase = Assert.Single(constructedResult.GetMembers("Ok").OfType<INamedTypeSymbol>());
            var errorCase = Assert.Single(constructedResult.GetMembers("Error").OfType<INamedTypeSymbol>());

            var tryGetOk = Assert.Single(constructedResult.GetMembers("TryGetOk").OfType<IMethodSymbol>());
            var tryGetError = Assert.Single(constructedResult.GetMembers("TryGetError").OfType<IMethodSymbol>());

            var okParameter = Assert.Single(tryGetOk.Parameters);
            var errorParameter = Assert.Single(tryGetError.Parameters);

            var okByRef = Assert.IsType<ByRefTypeSymbol>(okParameter.Type);
            var errorByRef = Assert.IsType<ByRefTypeSymbol>(errorParameter.Type);

            Assert.True(SymbolEqualityComparer.Default.Equals(okCase, okByRef.ElementType));
            Assert.True(SymbolEqualityComparer.Default.Equals(errorCase, errorByRef.ElementType));
        }
        finally
        {
            if (File.Exists(path))
                File.Delete(path);
        }
    }

    [Fact]
    public void ResultFromMetadata_ImplicitConversions_UseCaseTypes()
    {
        var (reference, path) = CreateRavenCoreResultReference();
        try
        {
            var compilation = Compilation.Create("metadata-result-conversions", new CompilationOptions(OutputKind.ConsoleApplication))
                .AddReferences([.. TestMetadataReferences.Default, reference]);

            var systemNamespace = compilation.GlobalNamespace.LookupNamespace("System")
                ?? compilation.GlobalNamespace;
            var resultDefinition = systemNamespace
                .GetMembers("Result")
                .OfType<INamedTypeSymbol>()
                .Single(symbol => symbol.Arity == 2);

            var okCase = Assert.Single(resultDefinition.GetMembers("Ok").OfType<INamedTypeSymbol>());
            var errorCase = Assert.Single(resultDefinition.GetMembers("Error").OfType<INamedTypeSymbol>());

            var conversions = resultDefinition.GetMembers("op_Implicit").OfType<IMethodSymbol>().ToArray();
            Assert.NotEmpty(conversions);

            Assert.Contains(conversions, conversion => HasParameterType(conversion, okCase));
            Assert.Contains(conversions, conversion => HasParameterType(conversion, errorCase));
        }
        finally
        {
            if (File.Exists(path))
                File.Delete(path);
        }
    }

    [Fact]
    public void ConstructedResultFromMetadata_ImplicitConversions_UseCaseTypes()
    {
        var (reference, path) = CreateRavenCoreResultReference();
        try
        {
            var compilation = Compilation.Create("metadata-result-conversions-constructed", new CompilationOptions(OutputKind.ConsoleApplication))
                .AddReferences([.. TestMetadataReferences.Default, reference]);

            var systemNamespace = compilation.GlobalNamespace.LookupNamespace("System")
                ?? compilation.GlobalNamespace;
            var resultDefinition = systemNamespace
                .GetMembers("Result")
                .OfType<INamedTypeSymbol>()
                .Single(symbol => symbol.Arity == 2);

            var stringType = compilation.GetSpecialType(SpecialType.System_String);
            var exceptionType = compilation.GetTypeByMetadataName("System.Exception")!;
            var constructedResult = Assert.IsAssignableFrom<INamedTypeSymbol>(resultDefinition.Construct(stringType, exceptionType));

            var okCase = Assert.Single(constructedResult.GetMembers("Ok").OfType<INamedTypeSymbol>());
            var errorCase = Assert.Single(constructedResult.GetMembers("Error").OfType<INamedTypeSymbol>());

            var conversions = constructedResult.GetMembers("op_Implicit").OfType<IMethodSymbol>().ToArray();
            Assert.NotEmpty(conversions);

            Assert.Contains(conversions, conversion => HasParameterType(conversion, okCase));
            Assert.Contains(conversions, conversion => HasParameterType(conversion, errorCase));
        }
        finally
        {
            if (File.Exists(path))
                File.Delete(path);
        }
    }

    private static (MetadataReference Reference, string Path) CreateRavenCoreResultReference()
    {
        var sourcePath = Path.GetFullPath(Path.Combine(
            "..", "..", "..", "..", "..", "src", "Raven.Core", "Result.rav"));
        var source = File.ReadAllText(sourcePath);
        var tree = SyntaxTree.ParseText(source);
        var compilation = Compilation.Create(
            "raven-core-result-fixture",
            [tree],
            TestMetadataReferences.Default,
            new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));

        using var stream = new MemoryStream();
        var emitResult = compilation.Emit(stream);
        Assert.True(emitResult.Success, string.Join(Environment.NewLine, emitResult.Diagnostics));

        var path = Path.Combine(Path.GetTempPath(), $"raven-core-result-{Guid.NewGuid():N}.dll");
        File.WriteAllBytes(path, stream.ToArray());
        return (MetadataReference.CreateFromFile(path), path);
    }

    private static bool HasParameterType(IMethodSymbol method, INamedTypeSymbol expectedType)
    {
        if (method.Parameters.Length != 1)
            return false;

        var parameterType = method.Parameters[0].Type;
        return SymbolEqualityComparer.Default.Equals(parameterType, expectedType);
    }
}
