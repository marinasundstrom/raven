using System.Reflection;

using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Tests;

[Trait("Target", "Shared")]
public sealed class ParameterSyntaxUtilitiesTests
{
    [Theory]
    [InlineData("x: int", RefKind.None, ScopedKind.None)]
    [InlineData("ref x: int", RefKind.Ref, ScopedKind.None)]
    [InlineData("out x: int", RefKind.Out, ScopedKind.None)]
    [InlineData("in x: int", RefKind.In, ScopedKind.None)]
    [InlineData("x: &int", RefKind.Ref, ScopedKind.None)]
    [InlineData("scoped ref x: int", RefKind.Ref, ScopedKind.ScopedRef)]
    [InlineData("scoped x: &int", RefKind.Ref, ScopedKind.ScopedRef)]
    [InlineData("scoped x: int", RefKind.None, ScopedKind.ScopedValue)]
    public void PassingModeComesFromTheModifierOrByRefType(string source, RefKind passing, ScopedKind scope)
    {
        var parameter = ParseParameter(source);
        Assert.Equal("x", ParameterSyntaxUtilities.GetParameterName(parameter, 7));
        Assert.Same(parameter.TypeAnnotation!.Type, ParameterSyntaxUtilities.GetDeclaredTypeSyntax(parameter));
        Assert.Equal(passing, ParameterSyntaxUtilities.GetRefKind(parameter));
        Assert.Equal(scope, ParameterSyntaxUtilities.GetScopedKind(parameter));
    }

    [Theory]
    [InlineData("Row(let value)", "Row")]
    [InlineData("Row(let value): Box", "Box")]
    [InlineData("(first, second)", null)]
    public void PatternParametersUseSyntheticNamesAndAllowAnExplicitType(string source, string? type)
    {
        var parameter = ParseParameter(source);
        Assert.NotNull(parameter.Pattern);
        Assert.Equal("<arg7>", ParameterSyntaxUtilities.GetParameterName(parameter, 7));
        Assert.Equal(type, ParameterSyntaxUtilities.GetDeclaredTypeSyntax(parameter)?.ToString());
    }

    [Theory]
    [InlineData("value: int", false, 0)]
    [InlineData("(x, y): (int, int)", true, 0)]
    [InlineData("(x, y): (int, int)", false, 1)]
    [InlineData("ref (x, y): (int, int)", true, 1)]
    [InlineData("ref (x, y): (int, int)", false, 2)]
    public void PatternContextReportsEachApplicableRestriction(string source, bool hasBody, int count)
    {
        var parameter = ParseParameter(source);
        var diagnostics = new DiagnosticBag();
        ParameterSyntaxUtilities.ValidatePatternContext(parameter, hasBody, diagnostics);
        var actual = diagnostics.AsEnumerable().ToArray();
        Assert.Equal(count, actual.Length);
        Assert.All(actual, diagnostic =>
        {
            Assert.Equal("RAV1619", diagnostic.Id);
            Assert.Equal(parameter.Pattern!.Span, diagnostic.Location.SourceSpan);
        });
    }

    [Theory]
    [InlineData(TypeKind.Class, false, false, true)]
    [InlineData(TypeKind.Struct, true, false, false)]
    [InlineData(TypeKind.TypeParameter, false, true, false)]
    [InlineData(TypeKind.TypeParameter, false, false, true)]
    [InlineData(TypeKind.Error, false, false, false)]
    public void ScopedValueValidatesRefLikeTypesAndConstraints(TypeKind kind, bool refLike, bool allowsRefLike, bool report)
    {
        TypeSymbol type = kind == TypeKind.TypeParameter
            ? DispatchProxy.Create<TypeParameterSymbol, TypeProxy>()
            : DispatchProxy.Create<NamedTypeSymbol, TypeProxy>();
        var proxy = (TypeProxy)(object)type;
        proxy.Kind = kind;
        proxy.RefLike = refLike;
        proxy.AllowsRefLike = allowsRefLike;
        var diagnostics = new DiagnosticBag();
        var parameter = ParseParameter("scoped x: T");
        Assert.Equal(ScopedKind.ScopedValue, ParameterSyntaxUtilities.GetScopedKind(parameter, type, diagnostics));
        Assert.Equal(refLike || allowsRefLike, SemanticFacts.MayBeRefLike(type));
        if (report)
        {
            var diagnostic = Assert.Single(diagnostics.AsEnumerable());
            Assert.Equal("RAV0354", diagnostic.Id);
            Assert.Equal(parameter.ScopedKeyword.Span, diagnostic.Location.SourceSpan);
        }
        else
        {
            Assert.Empty(diagnostics.AsEnumerable());
        }
    }

    private static ParameterSyntax ParseParameter(string source)
        => SyntaxTree.ParseText($"func Test({source}) {{ }}").GetRoot().DescendantNodes().OfType<ParameterSyntax>().Single();

    public class TypeProxy : DispatchProxy
    {
        public TypeKind Kind { get; set; }
        public bool RefLike { get; set; }
        public bool AllowsRefLike { get; set; }

        protected override object? Invoke(MethodInfo? method, object?[]? args) => method?.Name switch
        {
            "get_TypeKind" => Kind,
            "get_IsRefLikeType" => RefLike,
            "get_ConstraintKind" => AllowsRefLike ? TypeParameterConstraintKind.AllowByRefLike : default,
            _ => throw new NotSupportedException(method?.Name)
        };
    }
}
