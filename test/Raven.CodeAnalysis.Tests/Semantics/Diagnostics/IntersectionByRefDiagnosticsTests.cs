using System.Collections.Generic;
using System.Linq;

using Raven.CodeAnalysis.Symbols;
using Raven.CodeAnalysis.Syntax;

using Xunit;

namespace Raven.CodeAnalysis.Semantics.Tests;

public sealed class IntersectionByRefDiagnosticsTests : CompilationTestBase
{
    [Theory]
    [InlineData(false, false)]
    [InlineData(false, true)]
    [InlineData(true, false)]
    [InlineData(true, true)]
    public void AddressOf_RejectsCompoundStorage(bool parameter, bool nested)
    {
        Check(parameter, nested, null);
    }

    [Theory]
    [InlineData(false, false, RefKind.Ref)]
    [InlineData(false, true, RefKind.Ref)]
    [InlineData(true, false, RefKind.Ref)]
    [InlineData(true, true, RefKind.Ref)]
    [InlineData(false, false, RefKind.Out)]
    [InlineData(false, true, RefKind.Out)]
    [InlineData(true, false, RefKind.Out)]
    [InlineData(true, true, RefKind.Out)]
    [InlineData(false, false, RefKind.In)]
    [InlineData(false, true, RefKind.In)]
    [InlineData(true, false, RefKind.In)]
    [InlineData(true, true, RefKind.In)]
    public void ByRefArgument_RejectsCompoundStorage(bool parameter, bool nested, RefKind kind)
    {
        Check(parameter, nested, kind);
    }

    [Fact]
    public void ByValueArgument_DoesNotIntroduceAStorageRestriction()
    {
        var (compilation, _) = CreateCompilation("interface A {}\ninterface B {}");
        var type = compilation.CreateIntersectionTypeSymbol(compilation.GetTypeByMetadataName("A")!, compilation.GetTypeByMetadataName("B")!);
        var binder = new ProbeBinder(compilation, type, parameter: false);
        var syntax = ParseExpression("value");
        var bound = binder.BindArgument(RefKind.None, syntax);
        Assert.Same(type, bound.Type);
        Assert.Empty(binder.Diagnostics.AsEnumerable());
    }

    private void Check(bool parameter, bool nested, RefKind? kind)
    {
        var (compilation, _) = CreateCompilation("interface A {}\ninterface B {}");
        var a = compilation.GetTypeByMetadataName("A")!;
        var intersection = compilation.CreateIntersectionTypeSymbol(a, compilation.GetTypeByMetadataName("B")!);
        Verify(intersection, rejected: true);
        Verify(a, rejected: false);

        void Verify(ITypeSymbol type, bool rejected)
        {
            if (nested)
                type = compilation.CreateArrayTypeSymbol(type);
            var binder = new ProbeBinder(compilation, type, parameter);
            var syntax = ParseExpression(kind is null ? "&value" : "value");
            var result = kind is { } refKind ? binder.BindArgument(refKind, syntax) : binder.BindExpression(syntax);
            if (rejected)
            {
                Assert.IsType<BoundErrorExpression>(result);
                var diagnostic = Assert.Single(binder.Diagnostics.AsEnumerable());
                Assert.Equal("RAV0363", diagnostic.Id);
                Assert.Equal(syntax.Span, diagnostic.Location.SourceSpan);
            }
            else
            {
                Assert.IsType<BoundAddressOfExpression>(result);
                Assert.Empty(binder.Diagnostics.AsEnumerable());
            }
        }
    }

    private static ExpressionSyntax ParseExpression(string text)
        => SyntaxTree.ParseText(text).GetRoot().DescendantNodes().OfType<ExpressionStatementSyntax>().Single().Expression;

    private sealed class ProbeBinder : BlockBinder
    {
        private readonly ISymbol _value;
        public override SemanticModel SemanticModel { get; }

        public ProbeBinder(Compilation compilation, ITypeSymbol type, bool parameter)
            : base(compilation.Assembly, compilation.GlobalBinder)
        {
            SemanticModel = compilation.GetSemanticModel(compilation.SyntaxTrees.First());
            _value = parameter
                ? new SourceParameterSymbol("value", type, compilation.Assembly, null, null, [], [], isMutable: true)
                : new SourceLocalSymbol("value", type, true, compilation.Assembly, null, null, [], []);
        }

        public override IEnumerable<ISymbol> LookupSymbols(string name)
            => name == "value" ? [_value] : base.LookupSymbols(name);

        public BoundExpression BindArgument(RefKind kind, ExpressionSyntax syntax)
            => BindByRefInvocationArgument(BindExpression(syntax), kind, syntax);
    }
}
