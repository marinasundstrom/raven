using System.Collections.Immutable;

using Raven.CodeAnalysis.Macros;
using Raven.CodeAnalysis.Symbols;
using Raven.CodeAnalysis.Syntax;
using Raven.CodeAnalysis.Tests.Symbols;
using Raven.CodeAnalysis.Text;

namespace Raven.CodeAnalysis.Tests.Macros;

[Trait("Target", "Shared")]
public sealed class MacroFragmentValueContractTests
{
    [Fact]
    public void LocalEqualityPreservesTypeIdentityAndDeclarationSpans()
    {
        var type = Named("Types", "Example.Value");
        var local = new MacroFragmentLocal("item", type, new TextSpan(2, 4), new TextSpan(12, 4));
        var equal = new MacroFragmentLocal("item", type, new TextSpan(2, 4), new TextSpan(12, 4));
        Assert.True(local == equal);
        Assert.Equal(local.GetHashCode(), equal.GetHashCode());
        Assert.Same(type, local.Type);
        Assert.False(local.IsParameter);
        Assert.True(local != new MacroFragmentLocal("item", type, null, new TextSpan(12, 4), isParameter: true));
        Assert.True(local != new MacroFragmentLocal("item", Named("Other", "Example.Value"), new TextSpan(2, 4), new TextSpan(12, 4)));
    }

    [Fact]
    public void RegionNormalizesDefaultLocalsAndRetainsTargetType()
    {
        var type = Named("Types", "Example.Value");
        var region = new MacroFragmentRegion(MacroFragmentKind.Expression, new TextSpan(2, 3), new TextSpan(12, 3), default, type);
        var equal = new MacroFragmentRegion(MacroFragmentKind.Expression, new TextSpan(2, 3), new TextSpan(12, 3), [], type);
        Assert.True(region == equal);
        Assert.False(region.Locals.IsDefault);
        Assert.Empty(region.Locals);
        Assert.Same(type, region.TargetType);
        Assert.True(region != new MacroFragmentRegion(region.Kind, region.BodyRelativeSpan, region.Span, [], null));
        var local = new MacroFragmentLocal("x", type, null, null);
        var locals = ImmutableArray.Create(local);
        var populated = new MacroFragmentRegion(region.Kind, region.BodyRelativeSpan, region.Span, locals, type);
        Assert.Same(local, Assert.Single(populated.Locals));
        Assert.Equal(populated, new MacroFragmentRegion(region.Kind, region.BodyRelativeSpan, region.Span, locals, type));
        Assert.NotEqual(populated, new MacroFragmentRegion(region.Kind, region.BodyRelativeSpan, region.Span, ImmutableArray.Create(local), type));
    }

    [Fact]
    public void SemanticInfoPreservesAuthoredSyntaxAndSemanticPayload()
    {
        var syntax = SyntaxFactory.ParseExpression("value");
        var type = Named("Types", "Example.Value");
        var symbols = new SymbolInfo(type);
        var types = new TypeInfo(type, type);
        var region = new MacroFragmentRegion(MacroFragmentKind.Expression, new TextSpan(0, 5), new TextSpan(10, 5), [], type);
        var info = new MacroFragmentSemanticInfo(region, region.Span, symbols, types, syntax);
        Assert.Equal(info, new MacroFragmentSemanticInfo(region, region.Span, symbols, types, syntax));
        Assert.Same(syntax, info.Syntax);
        Assert.Same(region, info.Region);
        Assert.Equal(symbols, info.SymbolInfo);
        Assert.Same(types, info.TypeInfo);
        Assert.NotEqual(info, new MacroFragmentSemanticInfo(region, new TextSpan(11, 5), symbols, types, syntax));
    }

    [Fact]
    public void InferredAnnotationEqualityIncludesSpanAndType()
    {
        var type = Named("Types", "Example.Value");
        var annotation = new MacroFragmentInferredTypeAnnotation(new TextSpan(4, 2), type);
        Assert.Equal(annotation, new MacroFragmentInferredTypeAnnotation(new TextSpan(4, 2), type));
        Assert.NotEqual(annotation, new MacroFragmentInferredTypeAnnotation(new TextSpan(5, 2), type));
        Assert.Same(type, annotation.Type);
    }

    [Fact]
    public void EmbeddedProjectionPreservesTextAndValueEquality()
    {
        var projection = new MacroEmbeddedLanguageProjection("html", "<p>\n</p>", new TextSpan(20, 8));
        Assert.Equal(projection, new MacroEmbeddedLanguageProjection("html", "<p>\n</p>", new TextSpan(20, 8)));
        Assert.Equal("<p>\n</p>", projection.Text);
        Assert.NotEqual(projection, new MacroEmbeddedLanguageProjection("xml", projection.Text, projection.Span));
    }

    [Fact]
    public void TokenInfoPreservesRawKindTextAndOptionalSymbol()
    {
        var token = SyntaxFactory.ParseExpression("item").GetFirstToken().WithRawKind(10001);
        var symbol = Named("Types", "Example.Value");
        var info = new MacroTokenInfo(token, new TextSpan(0, 4), new TextSpan(10, 4), "item", MacroTokenClassification.Identifier, symbol);
        Assert.Equal(10001, info.RawKind);
        Assert.Equal("item", info.Text);
        Assert.Same(symbol, info.Symbol);
        Assert.Equal(info, new MacroTokenInfo(token, info.BodyRelativeSpan, info.Span, "item", info.Classification, symbol));
        Assert.NotEqual(info, new MacroTokenInfo(token, info.BodyRelativeSpan, info.Span, null, info.Classification, null));
    }

    [Fact]
    public void PublicDataObjectsRemainSealedWithGetterOnlyStoredProperties()
    {
        foreach (var type in new[] { typeof(MacroFragmentLocal), typeof(MacroFragmentRegion), typeof(MacroFragmentSemanticInfo),
            typeof(MacroFragmentInferredTypeAnnotation), typeof(MacroEmbeddedLanguageProjection), typeof(MacroTokenInfo) })
        {
            Assert.True(type.IsSealed);
            Assert.All(type.GetProperties(), property => Assert.Null(property.SetMethod));
        }
        Assert.Equal(5, (int)MacroFragmentKind.Block);
        Assert.Equal(7, (int)MacroTokenClassification.Comment);
    }

    private static NamedTypeSymbol Named(string assemblyName, string fullName)
    {
        var symbol = MetadataSymbolFixture.Named(assemblyName, fullName);
        MetadataSymbolFixture.Set(symbol, "Equals", (Func<object?[]?, object?>)(arguments => ReferenceEquals(symbol, arguments![0])));
        return symbol;
    }
}
