using System.Collections.Immutable;

using Raven.CodeAnalysis.Metadata;
using Raven.CodeAnalysis.Symbols;

namespace Raven.CodeAnalysis.Tests;

public sealed class ParameterConversionQueriesTests
{
    [Fact]
    public void NonPeProviderCategoriesPreserveRankingWithoutLoadingSignatures()
    {
        var compilation = Compilation.Create("test", [], TestMetadataReferences.Default);
        var argument = compilation.GetSpecialType(SpecialType.System_Int32);
        var scores = new List<int>();
        foreach (var conversion in new[] { ParameterConversionKind.Identity, ParameterConversionKind.ImplicitNumeric, ParameterConversionKind.ToObject })
        {
            var method = new ProviderMethod(compilation) { Conversion = conversion };
            var score = 0;
            Assert.True(ParameterConversionQueries.TryScore(argument, method, 2, ref score, out var handled));
            Assert.True(handled);
            Assert.Same(argument, method.Argument);
            Assert.Equal(2, method.ParameterIndex);
            scores.Add(score);
        }
        Assert.True(scores[0] > scores[1]);
        Assert.True(scores[1] > scores[2]);
    }

    [Theory]
    [InlineData(false)]
    [InlineData(true)]
    public void UnavailableAndRejectedConversionsRemainDistinctAndDoNotChangeScore(bool available)
    {
        var compilation = Compilation.Create("test", [], TestMetadataReferences.Default);
        var method = new ProviderMethod(compilation) { Available = available, Conversion = ParameterConversionKind.None };
        var score = 7;
        Assert.False(ParameterConversionQueries.TryScore(compilation.GetSpecialType(SpecialType.System_Int32), method, 0, ref score, out var handled));
        Assert.Equal(available, handled);
        Assert.Equal(7, score);
    }

    [Theory]
    [InlineData(SpecialType.System_Int64, "Long", 1)]
    [InlineData(SpecialType.System_Int32, "Long", 2)]
    [InlineData(SpecialType.System_String, "Object", 3)]
    [InlineData(SpecialType.System_Boolean, "Long", 0)]
    [InlineData(SpecialType.System_String, "String", 1)]
    public void PeProviderPreservesExistingShortcutClassification(SpecialType argumentType, string methodName, int expected)
    {
        var compilation = CreateMetadataCompilation();
        var method = compilation.GetTypeByMetadataName("Receivers")!.GetMembers(methodName).OfType<IMethodSymbol>().Single();
        var classifier = Assert.IsAssignableFrom<IParameterConversionClassifier>(method);
        Assert.True(classifier.TryClassifyParameterConversion(compilation.GetSpecialType(argumentType), 0, out var conversion));
        Assert.Equal((ParameterConversionKind)expected, conversion);
    }

    [Fact]
    public void PeProviderDeclinesUnsupportedArgumentShapeAndMissingParameter()
    {
        var compilation = CreateMetadataCompilation();
        var type = compilation.GetTypeByMetadataName("Receivers")!;
        var method = type.GetMembers("Long").OfType<IMethodSymbol>().Single();
        var classifier = Assert.IsAssignableFrom<IParameterConversionClassifier>(method);
        Assert.False(classifier.TryClassifyParameterConversion(type, 0, out _));
        Assert.False(classifier.TryClassifyParameterConversion(compilation.GetSpecialType(SpecialType.System_Int32), 1, out _));
    }

    [Theory]
    [InlineData(1, true)]
    [InlineData(2, false)]
    public void PeArrayShortcutOnlyHandlesSupportedRank(int rank, bool handled)
    {
        var compilation = CreateMetadataCompilation();
        var method = compilation.GetTypeByMetadataName("Receivers")!.GetMembers("Array").OfType<IMethodSymbol>().Single();
        var classifier = Assert.IsAssignableFrom<IParameterConversionClassifier>(method);
        var argument = compilation.CreateArrayTypeSymbol(compilation.GetSpecialType(SpecialType.System_String), rank);
        Assert.Equal(handled, classifier.TryClassifyParameterConversion(argument, 0, out var conversion));
        if (handled)
            Assert.Equal(ParameterConversionKind.Identity, conversion);
    }

    private static Compilation CreateMetadataCompilation()
    {
        var reference = TestMetadataFactory.CreateFromSource("""
            public class Receivers {
                public static func Long(value: System.Int64) -> int => 0
                public static func Object(value: System.Object) -> int => 0
                public static func String(value: System.String) -> int => 0
                public static func Array(value: System.String[]) -> int => 0
            }
            """, "ConversionReceivers");
        return Compilation.Create("test", [], TestMetadataReferences.Default.Append(reference).ToArray());
    }

    private sealed class ProviderMethod(Compilation compilation)
        : SourceMethodSymbol("Convert", compilation.GetSpecialType(SpecialType.System_Object), [],
            compilation.Assembly, null, compilation.SourceGlobalNamespace, [], []), IParameterConversionClassifier, IMethodSymbol
    {
        internal bool Available { get; init; } = true;
        internal ParameterConversionKind Conversion { get; init; }
        internal ITypeSymbol? Argument { get; private set; }
        internal int ParameterIndex { get; private set; }
        public bool TryClassifyParameterConversion(ITypeSymbol argumentType, int parameterIndex, out ParameterConversionKind conversion)
        {
            Argument = argumentType;
            ParameterIndex = parameterIndex;
            conversion = Conversion;
            return Available;
        }
        ImmutableArray<IParameterSymbol> IMethodSymbol.Parameters
            => throw new InvalidOperationException("Conversion shortcut must not load signatures.");
    }
}
