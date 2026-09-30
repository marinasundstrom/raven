using System.Collections.Immutable;
using System.Diagnostics.CodeAnalysis;

using Raven.CodeAnalysis.Metadata;
using Raven.CodeAnalysis.Symbols;
using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Tests;

public sealed class MethodParameterQueriesTests
{
    [Theory]
    [InlineData(0, 1, true)]
    [InlineData(1, 0, true)]
    [InlineData(3, 0, false)]
    public void ProviderFactsRespectOffsetsWithoutMaterializingParameters(int offset, int expectedRequired, bool expectedParams)
    {
        var compilation = Compilation.Create("test", [], TestMetadataReferences.Default);
        var method = new ProviderMethod(compilation);

        Assert.True(MethodParameterQueries.TryGetCount(method, out var count));
        Assert.Equal(3, count);
        Assert.True(MethodParameterQueries.TryGetRequiredCount(method, offset, out var required, out var hasParams));
        Assert.Equal(expectedRequired, required);
        Assert.Equal(expectedParams, hasParams);
        Assert.True(MethodParameterQueries.TryGetType(method, 0, out var type));
        Assert.Same(method.ParameterType, type);
    }

    [Theory]
    [InlineData(-1)]
    [InlineData(4)]
    public void InvalidOffsetsDoNotReadProviderUsage(int offset)
    {
        var compilation = Compilation.Create("test", [], TestMetadataReferences.Default);
        var method = new ProviderMethod(compilation) { FailUsage = true };
        Assert.False(MethodParameterQueries.TryGetRequiredCount(method, offset, out _, out _));
        Assert.Equal(0, method.UsageQueries);
    }

    [Fact]
    public void UnavailableProviderFactsDoNotFallBackToFullSignature()
    {
        var compilation = Compilation.Create("test", [], TestMetadataReferences.Default);
        var method = new ProviderMethod(compilation) { FailCount = true, FailUsage = true, FailType = true };
        Assert.False(MethodParameterQueries.TryGetCount(method, out _));
        Assert.False(MethodParameterQueries.TryGetRequiredCount(method, 0, out _, out _));
        Assert.Equal(0, method.UsageQueries);
        Assert.False(MethodParameterQueries.TryGetType(method, 0, out _));
        method.FailCount = false;
        Assert.False(MethodParameterQueries.TryGetRequiredCount(method, 0, out _, out _));
        Assert.Equal(1, method.UsageQueries);
    }

    [Theory]
    [InlineData(false)]
    [InlineData(true)]
    public void SourceAndPeFactsMatchPublicSignature(bool metadata)
    {
        var compilation = Compilation.Create("test", [SyntaxTree.ParseText("""
            class Example {
                static func Run(first: int, second: int = 1) -> int => first
            }
            """)], TestMetadataReferences.Default);
        var method = metadata
            ? compilation.GetTypeByMetadataName("System.String")!.GetMembers("Substring").OfType<IMethodSymbol>().First()
            : compilation.GetTypeByMetadataName("Example")!.GetMembers("Run").OfType<IMethodSymbol>().Single();

        Assert.True(MethodParameterQueries.TryGetCount(method, out var count));
        Assert.Equal(method.Parameters.Length, count);
        Assert.True(MethodParameterQueries.TryGetRequiredCount(method, 0, out var required, out var hasParams));
        Assert.Equal(method.Parameters.Count(p => !p.HasExplicitDefaultValue && !p.IsVarParams), required);
        Assert.Equal(method.Parameters.Any(p => p.IsVarParams), hasParams);
        for (var i = 0; i < count; i++)
        {
            Assert.True(MethodParameterQueries.TryGetType(method, i, out var type));
            Assert.True(SymbolEqualityComparer.Default.Equals(method.Parameters[i].Type, type));
        }
        Assert.False(MethodParameterQueries.TryGetType(method, -1, out _));
        Assert.False(MethodParameterQueries.TryGetType(method, count, out _));
    }

    private sealed class ProviderMethod(Compilation compilation)
        : SourceMethodSymbol("Run", compilation.GetSpecialType(SpecialType.System_Object), [],
            compilation.Assembly, null, compilation.SourceGlobalNamespace, [], []), IMethodParameterInfo, IMethodSymbol
    {
        internal ITypeSymbol ParameterType { get; } = compilation.GetSpecialType(SpecialType.System_Int32);
        internal bool FailCount { get; set; }
        internal bool FailUsage { get; init; }
        internal bool FailType { get; init; }
        internal int UsageQueries { get; private set; }
        public bool TryGetParameterCount(out int count)
        {
            count = FailCount ? 0 : 3;
            return !FailCount;
        }
        public bool TryGetParameterType(int index, [NotNullWhen(true)] out ITypeSymbol? type)
        {
            type = !FailType && index >= 0 && index < 3 ? ParameterType : null;
            return type is not null;
        }
        public bool TryGetParameterUsage(int index, out bool isOptional, out bool isVariadic)
        {
            UsageQueries++;
            isOptional = index == 1;
            isVariadic = index == 2;
            return !FailUsage && index >= 0 && index < 3;
        }
        ImmutableArray<IParameterSymbol> IMethodSymbol.Parameters
            => throw new InvalidOperationException("Provider facts must not materialize Parameters.");
    }
}
