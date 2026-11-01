using System;
using System.IO;
using System.Linq;

using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Tests;

[Trait("Target", "DotNet")]
public sealed class NestedGenericMethodSubstitutionTests
{
    [Fact]
    public void ImmutableArrayBuilder_SubstitutesMethodArgumentThroughContainingType()
    {
        var compilation = Compilation.Create(
            "nested_generic_method_substitution", [],
            references: TestMetadataReferences.Default,
            options: new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        var factory = compilation.GetTypeByMetadataName("System.Collections.Immutable.ImmutableArray")!;
        var createBuilder = Assert.Single(factory.GetMembers("CreateBuilder").OfType<IMethodSymbol>(),
            method => method.TypeParameters.Length == 1 && method.Parameters.Length == 0);
        var intType = compilation.GetSpecialType(SpecialType.System_Int32);
        var builder = Assert.IsAssignableFrom<INamedTypeSymbol>(createBuilder.Construct(intType).ReturnType);

        Assert.True(SymbolEqualityComparer.Default.Equals(intType, Assert.Single(builder.ContainingType!.TypeArguments)));
        var add = Assert.Single(builder.GetMembers("Add").OfType<IMethodSymbol>(), method => method.Parameters.Length == 1);
        Assert.True(SymbolEqualityComparer.Default.Equals(intType, Assert.Single(add.Parameters).Type));
        var move = Assert.Single(builder.GetMembers("MoveToImmutable").OfType<IMethodSymbol>());
        var array = Assert.IsAssignableFrom<INamedTypeSymbol>(move.ReturnType);
        Assert.True(SymbolEqualityComparer.Default.Equals(intType, Assert.Single(array.TypeArguments)));
    }

    [Fact]
    public void ImmutableArrayBuilder_EmittedCallsUseClosedMemberSignatures()
    {
        var compilation = Compilation.Create(
            "NestedBuilderRuntime",
            [SyntaxTree.ParseText("""
                import System.Collections.Immutable.*
                public class Cases {
                    static func Run() -> int {
                        val builder = ImmutableArray.CreateBuilder<int>(1)
                        builder.Add(42)
                        val array = builder.MoveToImmutable()
                        return array[0]
                    }
                }
                """)],
            TestMetadataReferences.Default,
            new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        using var stream = new MemoryStream();
        var result = compilation.Emit(stream);
        Assert.True(result.Success, string.Join(Environment.NewLine, result.Diagnostics));
        using var loaded = TestAssemblyLoader.LoadFromStream(stream, compilation.References);
        Assert.Equal(42, loaded.Assembly.GetType("Cases")!.GetMethod("Run")!.Invoke(null, null));
    }

}
