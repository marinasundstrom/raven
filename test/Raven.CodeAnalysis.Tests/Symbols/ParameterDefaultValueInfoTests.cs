using Raven.CodeAnalysis.Metadata;
using Raven.CodeAnalysis.Symbols;
using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Tests;

public sealed class ParameterDefaultValueInfoTests
{
    [Theory]
    [InlineData(false, "0")]
    [InlineData(true, "default")]
    public void DisplayDistinguishesProviderTypeDefaultFromLiteral(bool typeDefault, string expected)
    {
        var compilation = Compilation.Create("test", [], TestMetadataReferences.Default);
        var parameter = new ProviderParameter(compilation, compilation.GetSpecialType(SpecialType.System_Int32), 0, typeDefault);
        var format = SymbolDisplayFormat.RavenSignatureFormat;

        Assert.EndsWith(" = " + expected, parameter.ToDisplayString(format));
    }

    [Theory]
    [InlineData(false)]
    [InlineData(true)]
    public void BindingUsesProviderTypeDefaultForNonLiteralStruct(bool typeDefault)
    {
        var tree = SyntaxTree.ParseText("");
        var compilation = Compilation.Create("test", [tree], TestMetadataReferences.Default);
        var type = compilation.GetTypeByMetadataName("System.Guid")!;
        var parameter = new ProviderParameter(compilation, type, default(Guid), typeDefault);
        var binder = new OptionalArgumentBinder(compilation.Assembly, compilation.GetSemanticModel(tree).GetBinder(tree.GetRoot()));

        var argument = binder.Create(parameter);

        if (typeDefault)
            Assert.IsType<BoundDefaultValueExpression>(argument);
        else
            Assert.IsType<BoundErrorExpression>(argument);
        Assert.True(SymbolEqualityComparer.Default.Equals(type, argument.Type));
    }

    [Theory]
    [InlineData(0, true)]
    [InlineData(1, true)]
    [InlineData(2, true)]
    [InlineData(0, false)]
    [InlineData(1, false)]
    [InlineData(2, false)]
    public void ConstructedParametersPreserveDefaultKindAndSubstituteType(int wrapperKind, bool typeDefault)
    {
        var tree = SyntaxTree.ParseText("");
        var compilation = Compilation.Create("test", [tree], TestMetadataReferences.Default);
        var objectType = compilation.GetSpecialType(SpecialType.System_Object);
        var owner = new SourceNamedTypeSymbol("Owner", objectType, TypeKind.Class,
            compilation.Assembly, null, compilation.SourceGlobalNamespace, [], [], addAsMember: false);
        var method = new SourceMethodSymbol("Run", objectType, [], owner, owner,
            compilation.SourceGlobalNamespace, [], []);
        var parameterType = new SourceTypeParameterSymbol("T", wrapperKind == 0 ? method : owner, owner,
            compilation.SourceGlobalNamespace, [], [], 0, TypeParameterConstraintKind.None, [], VarianceKind.None);
        var original = new ProviderParameter(compilation, parameterType, default(Guid), typeDefault);
        method.SetParameters([original]);
        var guidType = compilation.GetTypeByMetadataName("System.Guid")!;
        IMethodSymbol constructed;
        if (wrapperKind == 0)
        {
            method.SetTypeParameters([parameterType]);
            constructed = new ConstructedMethodSymbol(method, [guidType]);
        }
        else
        {
            owner.SetTypeParameters([parameterType]);
            var constructedOwner = new ConstructedNamedTypeSymbol(owner, [guidType]);
            constructed = constructedOwner.GetMembers("Run").OfType<IMethodSymbol>().Single();
            if (wrapperKind == 2)
                constructed = new ConstructedMethodSymbol(constructed, []);
        }
        var parameter = Assert.Single(constructed.Parameters);
        Assert.True(SymbolEqualityComparer.Default.Equals(guidType, parameter.Type));
        var binder = new OptionalArgumentBinder(compilation.Assembly, compilation.GetSemanticModel(tree).GetBinder(tree.GetRoot()));
        var argument = binder.Create(parameter);

        if (typeDefault)
        {
            Assert.IsType<BoundDefaultValueExpression>(argument);
            Assert.EndsWith(" = default", parameter.ToDisplayString(SymbolDisplayFormat.RavenSignatureFormat));
        }
        else
        {
            Assert.IsType<BoundErrorExpression>(argument);
            Assert.DoesNotContain(" = default", parameter.ToDisplayString(SymbolDisplayFormat.RavenSignatureFormat));
        }
        Assert.True(SymbolEqualityComparer.Default.Equals(guidType, argument.Type));
    }

    [Fact]
    public void ProviderFlagDoesNotMakeARequiredParameterOptional()
    {
        var tree = SyntaxTree.ParseText("");
        var compilation = Compilation.Create("test", [tree], TestMetadataReferences.Default);
        var parameter = new ProviderParameter(compilation, compilation.GetSpecialType(SpecialType.System_Int32), 0, true, hasDefault: false);
        var binder = new OptionalArgumentBinder(compilation.Assembly, compilation.GetSemanticModel(tree).GetBinder(tree.GetRoot()));

        Assert.IsType<BoundErrorExpression>(binder.Create(parameter));
        var format = SymbolDisplayFormat.RavenSignatureFormat;
        Assert.DoesNotContain(" = ", parameter.ToDisplayString(format));
    }

    private sealed class ProviderParameter(Compilation compilation, ITypeSymbol type, object? value, bool typeDefault, bool hasDefault = true)
        : SourceParameterSymbol("value", type, compilation.Assembly, null, compilation.SourceGlobalNamespace, [], [],
            hasExplicitDefaultValue: hasDefault, explicitDefaultValue: value), IParameterDefaultValueInfo
    {
        public bool ExplicitDefaultValueIsTypeDefault => typeDefault;
    }

    private sealed class OptionalArgumentBinder(ISymbol owner, Binder parent) : BlockBinder(owner, parent)
    {
        internal BoundExpression Create(IParameterSymbol parameter) => CreateOptionalArgument(parameter);
    }
}
