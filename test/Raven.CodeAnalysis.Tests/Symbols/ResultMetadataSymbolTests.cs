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
                .OfType<IDiscriminatedUnionSymbol>()
                .Single(symbol => symbol.Arity == 2);

            var okCase = Assert.IsAssignableFrom<INamedTypeSymbol>(resultDefinition.Cases.Single(c => c.Name == "Ok"));
            var errorCase = Assert.IsAssignableFrom<INamedTypeSymbol>(resultDefinition.Cases.Single(c => c.Name == "Error"));

            var tryGetMethods = resultDefinition.GetMembers("TryGetValue").OfType<IMethodSymbol>().ToArray();
            var tryGetOk = Assert.Single(tryGetMethods.Where(m => HasParameterType(m, okCase)));
            var tryGetError = Assert.Single(tryGetMethods.Where(m => HasParameterType(m, errorCase)));

            var okParameter = Assert.Single(tryGetOk.Parameters);
            var errorParameter = Assert.Single(tryGetError.Parameters);

            Assert.True(HasParameterType(tryGetOk, okCase));
            Assert.True(HasParameterType(tryGetError, errorCase));
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
                .OfType<IDiscriminatedUnionSymbol>()
                .Single(symbol => symbol.Arity == 2);

            var stringType = compilation.GetSpecialType(SpecialType.System_String);
            var exceptionType = compilation.GetTypeByMetadataName("System.Exception")!;
            var constructedResult = Assert.IsAssignableFrom<IDiscriminatedUnionSymbol>(resultDefinition.Construct(stringType, exceptionType));

            var okCase = Assert.IsAssignableFrom<INamedTypeSymbol>(constructedResult.Cases.Single(c => c.Name == "Ok"));
            var errorCase = Assert.IsAssignableFrom<INamedTypeSymbol>(constructedResult.Cases.Single(c => c.Name == "Error"));

            var tryGetMethods = constructedResult.GetMembers("TryGetValue").OfType<IMethodSymbol>().ToArray();
            var tryGetOk = Assert.Single(tryGetMethods.Where(m => HasParameterType(m, okCase)));
            var tryGetError = Assert.Single(tryGetMethods.Where(m => HasParameterType(m, errorCase)));

            var okParameter = Assert.Single(tryGetOk.Parameters);
            var errorParameter = Assert.Single(tryGetError.Parameters);

            Assert.True(HasParameterType(tryGetOk, okCase));
            Assert.True(HasParameterType(tryGetError, errorCase));
        }
        finally
        {
            if (File.Exists(path))
                File.Delete(path);
        }
    }

    [Fact]
    public void ResultFromMetadata_Constructors_UseCaseTypes()
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
                .OfType<IDiscriminatedUnionSymbol>()
                .Single(symbol => symbol.Arity == 2);

            var okCase = Assert.IsAssignableFrom<INamedTypeSymbol>(resultDefinition.Cases.Single(c => c.Name == "Ok"));
            var errorCase = Assert.IsAssignableFrom<INamedTypeSymbol>(resultDefinition.Cases.Single(c => c.Name == "Error"));

            var constructors = resultDefinition.Constructors
                .Where(ctor => !ctor.IsStatic && ctor.Parameters.Length == 1)
                .ToArray();
            Assert.NotEmpty(constructors);

            Assert.Contains(constructors, constructor => HasParameterType(constructor, okCase));
            Assert.Contains(constructors, constructor => HasParameterType(constructor, errorCase));
        }
        finally
        {
            if (File.Exists(path))
                File.Delete(path);
        }
    }

    [Fact]
    public void ConstructedResultFromMetadata_Constructors_UseCaseTypes()
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
                .OfType<IDiscriminatedUnionSymbol>()
                .Single(symbol => symbol.Arity == 2);

            var stringType = compilation.GetSpecialType(SpecialType.System_String);
            var exceptionType = compilation.GetTypeByMetadataName("System.Exception")!;
            var constructedResult = Assert.IsAssignableFrom<IDiscriminatedUnionSymbol>(resultDefinition.Construct(stringType, exceptionType));

            var okCase = Assert.IsAssignableFrom<INamedTypeSymbol>(constructedResult.Cases.Single(c => c.Name == "Ok"));
            var errorCase = Assert.IsAssignableFrom<INamedTypeSymbol>(constructedResult.Cases.Single(c => c.Name == "Error"));

            var constructors = constructedResult.Constructors
                .Where(ctor => !ctor.IsStatic && ctor.Parameters.Length == 1)
                .ToArray();
            Assert.NotEmpty(constructors);

            Assert.Contains(constructors, constructor => HasParameterType(constructor, okCase));
            Assert.Contains(constructors, constructor => HasParameterType(constructor, errorCase));
        }
        finally
        {
            if (File.Exists(path))
                File.Delete(path);
        }
    }

    [Fact]
    public void ResultFromMetadata_ExtensionPropertyGetter_BindsInvokedUnionCaseIdentifier()
    {
        var (reference, path) = CreateRavenCoreResultReference();
        try
        {
            const string source = """
import System.*

extension ResultExt<T, E> for Result<T, E> {
    val Probe: Option<T> {
        get {
            if self is Ok(val value) {
                return Some(value)
            }

            None
        }
    }
}

class Container {
    static func ProbeValue<T, E>(value: T) -> Option<T> {
        val result: Result<T, E> = Ok(value)
        return result.Probe
    }
}
""";

            var tree = SyntaxTree.ParseText(source);
            var compilation = Compilation.Create(
                    "metadata-result-property-union-invoke",
                    [tree],
                    [.. TestMetadataReferences.Default, reference],
                    new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));

            compilation.EnsureSetup();
            var diagnostics = compilation.GetDiagnostics();
            Assert.DoesNotContain(diagnostics, d => d.Severity == DiagnosticSeverity.Error);

            var invocation = tree.GetRoot()
                .DescendantNodes()
                .OfType<InvocationExpressionSyntax>()
                .Single(node => node.Expression is IdentifierNameSyntax id && id.Identifier.ValueText == "Some");

            var model = compilation.GetSemanticModel(tree);
            var bound = model.GetBoundNode(invocation);

            Assert.IsNotType<BoundErrorExpression>(bound);

            switch (bound)
            {
                case BoundUnionCaseExpression unionCase:
                    Assert.Equal("Option", unionCase.UnionType.Name);
                    Assert.Equal("Some", unionCase.CaseType.Name);
                    Assert.Single(unionCase.Arguments);
                    break;
                case BoundObjectCreationExpression objectCreation:
                    Assert.Equal("Some", objectCreation.Constructor.ContainingType.Name);
                    Assert.Single(objectCreation.Arguments);
                    break;
                default:
                    Assert.True(false, $"Unexpected bound node for Some(value): {bound.GetType().Name}");
                    break;
            }
        }
        finally
        {
            if (File.Exists(path))
                File.Delete(path);
        }
    }

    private static (MetadataReference Reference, string Path) CreateRavenCoreResultReference()
    {
        var coreDirectory = Path.GetFullPath(Path.Combine(
            "..", "..", "..", "..", "..", "src", "Raven.Core"));
        var optionTree = SyntaxTree.ParseText(File.ReadAllText(Path.Combine(coreDirectory, "Option.rav")));
        var resultTree = SyntaxTree.ParseText(File.ReadAllText(Path.Combine(coreDirectory, "Result.rav")));
        var compilation = Compilation.Create(
            "raven-core-result-fixture",
            [optionTree, resultTree],
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

        var parameterType = method.Parameters[0].GetByRefElementType();
        return AreEquivalentTypes(parameterType, expectedType);
    }

    private static bool AreEquivalentTypes(ITypeSymbol left, ITypeSymbol right)
    {
        if (SymbolEqualityComparer.Default.Equals(left, right))
            return true;

        var leftNamed = left as INamedTypeSymbol;
        var rightNamed = right as INamedTypeSymbol;
        if (leftNamed is null || rightNamed is null)
            return false;

        return SymbolEqualityComparer.Default.Equals(leftNamed.OriginalDefinition, rightNamed.OriginalDefinition);
    }
}
