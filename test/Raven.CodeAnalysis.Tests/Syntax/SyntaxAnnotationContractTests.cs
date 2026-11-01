using System.Reflection;

using Raven.CodeAnalysis.Syntax.InternalSyntax;
using Raven.CodeAnalysis.Syntax.InternalSyntax.Parser;

namespace Raven.CodeAnalysis.Syntax.Tests;

[Trait("Target", "Shared")]
public sealed class SyntaxAnnotationContractTests
{
    [Theory]
    [InlineData("tag", null)]
    [InlineData("tag", "value")]
    [InlineData("", "")]
    [InlineData("λ", "\0\ud83d\ude00")]
    public void EqualValuesHaveEqualHashesAndOperators(string kind, string? data)
    {
        var first = new SyntaxAnnotation(kind, data);
        var second = new SyntaxAnnotation(kind, data);
        Assert.NotSame(first, second);
        Assert.Equal(kind, first.Kind);
        Assert.Equal(data, first.Data);
        Assert.True(first.Equals(second));
        Assert.True(((IEquatable<SyntaxAnnotation>)first).Equals(second));
        Assert.True(first.Equals((object)second));
        Assert.True(first == second);
        Assert.False(first != second);
        Assert.Equal(first.GetHashCode(), second.GetHashCode());
        Assert.Single(new HashSet<SyntaxAnnotation> { first, second });
    }

    [Fact]
    public void EqualityDistinguishesBothValuesAndNull()
    {
        var annotation = new SyntaxAnnotation("tag");
        Assert.Null(annotation.Data);
        Assert.NotEqual(annotation, new SyntaxAnnotation("other"));
        Assert.NotEqual(annotation, new SyntaxAnnotation("tag", ""));
        Assert.False(annotation.Equals((object)"tag"));
        Assert.False(annotation.Equals((SyntaxAnnotation?)null));
        Assert.False(annotation == null);
        Assert.False(null == annotation);
        Assert.True(annotation != null);
        SyntaxAnnotation? absent = null;
        Assert.True(absent == null);
        Assert.False(absent != null);
    }

    [Fact]
    public void ConstructorsRejectNullKind()
    {
        Assert.Equal("kind", Assert.Throws<ArgumentNullException>(() => new SyntaxAnnotation(null!)).ParamName);
        Assert.Equal("kind", Assert.Throws<ArgumentNullException>(() => new SyntaxAnnotation(null!, "data")).ParamName);
    }

    [Fact]
    public void BothConstructorSignaturesArePreserved()
    {
        Assert.NotNull(typeof(SyntaxAnnotation).GetConstructor([typeof(string)]));
        Assert.NotNull(typeof(SyntaxAnnotation).GetConstructor([typeof(string), typeof(string)]));
        Assert.Equal(new SyntaxAnnotation("tag", null), new SyntaxAnnotation("tag"));
    }

    [Fact]
    public void LexerInterfacesPreserveCallbackAndByReferenceContracts()
    {
        Assert.True(typeof(ILexer).IsInterface);
        Assert.False(typeof(ILexer).IsPublic);
        var callback = typeof(ILexer).GetProperty(nameof(ILexer.DiagnosticSink))!;
        Assert.Equal(typeof(Action<DiagnosticInfo>), callback.PropertyType);
        Assert.NotNull(callback.GetMethod);
        Assert.NotNull(callback.SetMethod);
        var peek = typeof(ILexer).GetMethod(nameof(ILexer.PeekToken))!;
        Assert.Equal(typeof(Token), peek.ReturnType);
        Assert.Equal(0, Assert.Single(peek.GetParameters()).DefaultValue);
        Assert.Equal(typeof(IEnumerable<Token>), typeof(ILexer).GetMethod(nameof(ILexer.ReadTokens))!.ReturnType);
        var macro = typeof(IMacroBodyScanner).GetMethod(nameof(IMacroBodyScanner.ReadMacroBody))!;
        Assert.Equal(typeof(Token), macro.ReturnType);
        var terminated = Assert.Single(macro.GetParameters());
        Assert.True(terminated.IsOut);
        Assert.Equal(typeof(bool).MakeByRefType(), terminated.ParameterType);
    }

    [Fact]
    public void ElasticMarkerAndImmutableReferenceShapeArePreserved()
    {
        Assert.False(typeof(SyntaxAnnotation).IsValueType);
        Assert.True(typeof(SyntaxAnnotation).IsSealed);
        Assert.Equal(new SyntaxAnnotation("Elastic"), SyntaxAnnotation.ElasticAnnotation);
        Assert.Same(SyntaxAnnotation.ElasticAnnotation, SyntaxAnnotation.ElasticAnnotation);
        Assert.True(typeof(SyntaxAnnotation).GetField(nameof(SyntaxAnnotation.ElasticAnnotation))!.IsInitOnly);
        foreach (var property in typeof(SyntaxAnnotation).GetProperties(BindingFlags.Public | BindingFlags.Instance))
            Assert.Null(property.SetMethod);
        Assert.Null(typeof(SyntaxAnnotation).GetMethod("GetDebuggerDisplay", BindingFlags.Public | BindingFlags.Instance));
    }
}
