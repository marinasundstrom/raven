using System.Linq;

using Raven.CodeAnalysis.Symbols;
using Raven.CodeAnalysis.Syntax;

using Xunit;

namespace Raven.CodeAnalysis.Semantics.Tests;

public sealed class IntersectionStorageDiagnosticsTests : CompilationTestBase
{
    [Theory]
    [InlineData("direct")]
    [InlineData("array")]
    [InlineData("nullable")]
    [InlineData("reference")]
    [InlineData("address")]
    [InlineData("pointer")]
    [InlineData("tuple")]
    [InlineData("parameter")]
    [InlineData("return")]
    [InlineData("synthesized-delegate")]
    [InlineData("generic")]
    [InlineData("nested")]
    public void StorageChecksSemanticShape_NotOnlySourceAnnotations(string shape)
    {
        var (compilation, tree) = CreateCompilation("interface A {}\ninterface B {}\nclass Box<T> { class Inner {} }");
        Assert.DoesNotContain(compilation.GetDiagnostics(), d => d.Severity == DiagnosticSeverity.Error);
        var a = compilation.GetTypeByMetadataName("A")!;
        var intersection = compilation.CreateIntersectionTypeSymbol(a, compilation.GetTypeByMetadataName("B")!);
        var location = tree.GetRoot().GetLocation();
        var binder = new BlockBinder(compilation.Assembly, compilation.GlobalBinder);

        var rejected = binder.EnsureTypeValidForStorageLocation(Wrap(intersection), location);
        Assert.Equal(TypeKind.Error, rejected.TypeKind);
        var diagnostic = Assert.Single(binder.Diagnostics.AsEnumerable());
        Assert.Equal("RAV0363", diagnostic.Id);
        Assert.Equal(location, diagnostic.Location);

        var nominalBinder = new BlockBinder(compilation.Assembly, compilation.GlobalBinder);
        var nominal = Wrap(a);
        Assert.Same(nominal, nominalBinder.EnsureTypeValidForStorageLocation(nominal, location));
        Assert.Empty(nominalBinder.Diagnostics.AsEnumerable());

        ITypeSymbol Wrap(ITypeSymbol type) => shape switch
        {
            "array" => compilation.CreateArrayTypeSymbol(type),
            "nullable" => new NullableTypeSymbol(type, null, null, null, []),
            "reference" => new RefTypeSymbol(compilation.CreateArrayTypeSymbol(type)),
            "address" => new AddressTypeSymbol(type),
            "pointer" => compilation.CreatePointerTypeSymbol(type),
            "tuple" => compilation.CreateTupleTypeSymbol([(null, a), (null, type)]),
            "parameter" => compilation.CreateFunctionTypeSymbol([type], a),
            "return" => compilation.CreateFunctionTypeSymbol([a], type),
            "synthesized-delegate" => compilation.CreateFunctionTypeSymbol(Enumerable.Repeat<ITypeSymbol>(a, 17).ToArray(), type),
            "generic" => compilation.GetTypeByMetadataName("Box`1")!.Construct(type),
            "nested" => compilation.GetTypeByMetadataName("Box`1")!.Construct(type).GetMembers("Inner").OfType<INamedTypeSymbol>().Single(),
            _ => type
        };
    }

    [Theory]
    [InlineData(false, false)]
    [InlineData(false, true)]
    [InlineData(true, false)]
    [InlineData(true, true)]
    public void CaptureRejectsCompoundStorage_ButAllowsNominalProjection(bool parameter, bool nested)
    {
        var (compilation, tree) = CreateCompilation("interface A {}\ninterface B {}");
        var a = compilation.GetTypeByMetadataName("A")!;
        var intersection = compilation.CreateIntersectionTypeSymbol(a, compilation.GetTypeByMetadataName("B")!);
        var location = tree.GetRoot().GetLocation();
        var diagnostics = new DiagnosticBag();
        RefSafetyDiagnosticReporter.ReportCaptures([Capture(intersection)], location, diagnostics);
        var diagnostic = Assert.Single(diagnostics.AsEnumerable());
        Assert.Equal("RAV0363", diagnostic.Id);
        Assert.Equal(location, diagnostic.Location);

        diagnostics = new DiagnosticBag();
        RefSafetyDiagnosticReporter.ReportCaptures([Capture(a)], location, diagnostics);
        Assert.Empty(diagnostics.AsEnumerable());

        ISymbol Capture(ITypeSymbol type)
        {
            if (nested)
                type = compilation.CreateArrayTypeSymbol(type);
            return parameter
                ? new SourceParameterSymbol("value", type, compilation.Assembly, null, null, [], [])
                : new SourceLocalSymbol("value", type, false, compilation.Assembly, null, null, [], []);
        }
    }

    [Theory]
    [InlineData(false)]
    [InlineData(true)]
    public void IteratorStorageRejectsCompoundLocal(bool nested)
    {
        var (compilation, tree) = CreateCompilation("interface A {}\ninterface B {}\nclass Runner { func Run() {} }");
        var method = compilation.GetTypeByMetadataName("Runner")!.GetMembers("Run").OfType<IMethodSymbol>().Single();
        var a = compilation.GetTypeByMetadataName("A")!;
        var intersection = compilation.CreateIntersectionTypeSymbol(a, compilation.GetTypeByMetadataName("B")!);
        Check(intersection, expectedError: true);
        Check(a, expectedError: false);

        void Check(ITypeSymbol type, bool expectedError)
        {
            if (nested)
                type = compilation.CreateArrayTypeSymbol(type);
            var local = new SourceLocalSymbol("value", type, false, method, method.ContainingType,
                method.ContainingNamespace, [tree.GetRoot().GetLocation()], []);
            var body = new BoundBlockStatement([new BoundLocalDeclarationStatement([new BoundVariableDeclarator(local, null)])]);
            var diagnostics = new DiagnosticBag();
            RefSafetyDiagnosticReporter.ReportIteratorStorage(body, method, diagnostics);
            if (expectedError)
                Assert.Equal("RAV0363", Assert.Single(diagnostics.AsEnumerable()).Id);
            else
                Assert.Empty(diagnostics.AsEnumerable());
        }
    }

    [Fact]
    public void AwaitStorageRejectsCompoundLocalsAndParameters()
    {
        var (compilation, tree) = CreateCompilation("""
            import System.Threading.Tasks.*
            interface A {}
            interface B {}
            class Runner {
                async func Run() -> Task { await Task.Delay(1) }
            }
            """);
        Assert.DoesNotContain(compilation.GetDiagnostics(), d => d.Severity == DiagnosticSeverity.Error);
        var model = compilation.GetSemanticModel(tree);
        var awaitSyntax = tree.GetRoot().DescendantNodes().OfType<PrefixOperatorExpressionSyntax>().Single(n => n.Kind == SyntaxKind.AwaitExpression);
        var awaitExpression = Assert.IsType<BoundAwaitExpression>(model.GetBoundNode(awaitSyntax));
        var a = compilation.GetTypeByMetadataName("A")!;
        var intersection = compilation.CreateIntersectionTypeSymbol(a, compilation.GetTypeByMetadataName("B")!);
        Check(intersection, expectedError: true);
        Check(a, expectedError: false);

        void Check(ITypeSymbol type, bool expectedError)
        {
            var method = new SourceMethodSymbol("Work", compilation.GetSpecialType(SpecialType.System_Unit), [],
                compilation.Assembly, null, null, [], [], isAsync: true);
            var local = new SourceLocalSymbol("value", type, false, method, null, null, [awaitSyntax.GetLocation()], []);
            var body = new BoundBlockStatement([
                new BoundLocalDeclarationStatement([new BoundVariableDeclarator(local, null)]),
                new BoundExpressionStatement(awaitExpression),
                new BoundExpressionStatement(new BoundLocalAccess(local))
            ]);
            Assert.Contains(local, AsyncLowerer.GetLocalsCapturedAcrossAwait(body));
            var diagnostics = new DiagnosticBag();
            RefSafetyDiagnosticReporter.ReportLocalsAcrossAwait(body, diagnostics);
            CheckDiagnostics(diagnostics);

            method.SetParameters([new SourceParameterSymbol("argument", type, method, null, null, [awaitSyntax.GetLocation()], [])]);
            diagnostics = new DiagnosticBag();
            RefSafetyDiagnosticReporter.ReportParametersAcrossAwait(method, diagnostics);
            CheckDiagnostics(diagnostics);

            void CheckDiagnostics(DiagnosticBag bag)
            {
                if (expectedError)
                    Assert.Equal("RAV0363", Assert.Single(bag.AsEnumerable()).Id);
                else
                    Assert.Empty(bag.AsEnumerable());
            }
        }
    }

    [Fact]
    public void NominalTypeParameterWithConjunctiveConstraints_RemainsValidStorage()
    {
        var (compilation, tree) = CreateCompilation("interface A {}\ninterface B {}\nclass Box<T: A & B> {}");
        var parameter = compilation.GetTypeByMetadataName("Box`1")!.TypeParameters.Single();
        var binder = new BlockBinder(compilation.Assembly, compilation.GlobalBinder);
        Assert.Same(parameter, binder.EnsureTypeValidForStorageLocation(parameter, tree.GetRoot().GetLocation()));
        Assert.Empty(binder.Diagnostics.AsEnumerable());
    }
}
