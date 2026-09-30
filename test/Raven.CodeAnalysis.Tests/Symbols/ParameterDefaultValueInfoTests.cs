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
