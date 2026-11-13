using System.Collections.Immutable;

using Raven.CodeAnalysis.Syntax;

using static Raven.CodeAnalysis.Tests.Symbols.MetadataSymbolFixture;

namespace Raven.CodeAnalysis.Tests.Symbols;

[Trait("Target", "Shared")]
public sealed class CallerInfoAttributeFactsContractTests
{
    [Theory]
    [InlineData("Unknown", 0)]
    [InlineData("CallerArgumentExpression", 1)]
    [InlineData("CallerMemberName", 2)]
    [InlineData("CallerFilePath", 3)]
    [InlineData("CallerLineNumber", 4)]
    public void RecognizesCallerKinds(string name, int expected)
        => Assert.Equal(expected, (int)CallerInfoAttributeFacts.GetKind(Attribute(name)));

    [Fact]
    public void EffectiveAttributeUsesPrecedenceAndKeepsFirstTie()
    {
        var line = Attribute("CallerLineNumber");
        var member = Attribute("CallerMemberName");
        var file = Attribute("CallerFilePath");
        var expression = Attribute("CallerArgumentExpression");
        Assert.Null(CallerInfoAttributeFacts.GetEffectiveAttribute([]));
        Assert.Null(CallerInfoAttributeFacts.GetEffectiveAttribute([Attribute("Unknown")]));
        Assert.Same(line, CallerInfoAttributeFacts.GetEffectiveAttribute([expression, member, file, line]));
        Assert.Same(line, CallerInfoAttributeFacts.GetEffectiveAttribute([line, file, member, expression, Attribute("CallerLineNumber")]));
    }

    [Fact]
    public void MissingSinkNonParameterAndUnrelatedAttributeDoNothing()
    {
        var provider = new Provider();
        var syntax = Syntax();
        var owner = Make<Symbol>(SymbolKind.Namespace, "Owner");
        CallerInfoAttributeFacts.Validate(provider, null, owner, syntax, Attribute("CallerMemberName"));
        var diagnostics = new DiagnosticBag();
        CallerInfoAttributeFacts.Validate(provider, diagnostics, owner, syntax, Attribute("CallerMemberName"));
        CallerInfoAttributeFacts.Validate(provider, diagnostics, Parameter(provider), syntax, Attribute("Unknown"));
        Assert.True(diagnostics.IsEmpty);
        Assert.Equal(0, provider.TypeRequests);
    }

    [Fact]
    public void DefaultValueIsRequiredBeforeAnyConversionOrAttributeLookup()
    {
        var provider = new Provider();
        var parameter = Parameter(provider);
        Set(parameter, "get_HasExplicitDefaultValue", false);
        AssertDiagnostic(provider, parameter, Attribute("CallerMemberName"), "RAV0379");
        Assert.Equal(0, provider.TypeRequests);
    }

    [Theory]
    [InlineData(false)]
    [InlineData(true)]
    public void RequiresExistingImplicitStandardConversion(bool explicitConversion)
    {
        var provider = new Provider { Conversion = explicitConversion ? new Conversion(isNumeric: true, isImplicit: false) : default };
        AssertDiagnostic(provider, Parameter(provider), Attribute("CallerLineNumber"), "RAV0380");
        Assert.Equal(SpecialType.System_Int32, provider.RequestedType);
        Assert.False(provider.IncludeUserDefined);
        Assert.Equal(0, provider.AttributeRequests);
    }

    [Fact]
    public void HigherPrecedenceBoundAttributeReportsWarningWithoutReenteringSymbolAttributes()
    {
        var provider = new Provider { Attributes = [Attribute("CallerLineNumber")] };
        AssertDiagnostic(provider, Parameter(provider), Attribute("CallerMemberName"), "RAV0383");
        Assert.Equal(SpecialType.System_String, provider.RequestedType);
        Assert.Equal(1, provider.AttributeRequests);
    }

    [Theory]
    [InlineData("text", "RAV0382")]
    [InlineData("missing", "RAV0381")]
    public void ArgumentExpressionValidatesSiblingName(string sibling, string expected)
    {
        var provider = new Provider();
        var parameter = Parameter(provider);
        var method = Make<MethodSymbol>(SymbolKind.Method, "Read");
        Set(method, "get_Parameters", ImmutableArray.Create(parameter));
        Set(parameter, "get_ContainingSymbol", method);
        AssertDiagnostic(provider, parameter, Attribute("CallerArgumentExpression", sibling), expected);
    }

    [Theory]
    [InlineData(false)]
    [InlineData(true)]
    public void ArgumentExpressionAcceptsSiblingForMethodsAndIndexers(bool indexer)
    {
        var provider = new Provider();
        var parameter = Parameter(provider);
        var sibling = Make<ParameterSymbol>(SymbolKind.Parameter, "value");
        Symbol owner = indexer ? Make<PropertySymbol>(SymbolKind.Property, "Item") : Make<MethodSymbol>(SymbolKind.Method, "Read");
        Set(owner, "get_Parameters", ImmutableArray.Create(parameter, sibling));
        Set(parameter, "get_ContainingSymbol", owner);
        var diagnostics = new DiagnosticBag();
        CallerInfoAttributeFacts.Validate(provider, diagnostics, parameter, Syntax(), Attribute("CallerArgumentExpression", "value"));
        Assert.True(diagnostics.IsEmpty);
        Assert.False(provider.IncludeUserDefined);
    }

    private static void AssertDiagnostic(Provider provider, Symbol owner, AttributeData attribute, string id)
    {
        var syntax = Syntax();
        var diagnostics = new DiagnosticBag();
        CallerInfoAttributeFacts.Validate(provider, diagnostics, owner, syntax, attribute);
        var diagnostic = Assert.Single(diagnostics.AsEnumerable());
        Assert.Equal(id, diagnostic.Id);
        Assert.Equal(syntax.Span, diagnostic.Location.SourceSpan);
    }

    private static AttributeSyntax Syntax()
        => SyntaxTree.ParseText("[Tag] class C {}").GetRoot().DescendantNodes().OfType<AttributeSyntax>().Single();

    private static AttributeData Attribute(string name, string? sibling = null)
        => new(Named("Runtime", "System.Runtime.CompilerServices." + name + "Attribute"),
            Make<MethodSymbol>(SymbolKind.Method, ".ctor"),
            sibling is null ? [] : [TypedConstant.CreatePrimitive(null, sibling)], [], null);

    private static ParameterSymbol Parameter(Provider provider)
    {
        var parameter = Make<ParameterSymbol>(SymbolKind.Parameter, "text");
        Set(parameter, "get_Type", provider.Type);
        Set(parameter, "get_HasExplicitDefaultValue", true);
        return parameter;
    }

    private sealed class Provider : CallerInfoValidationServices
    {
        public NamedTypeSymbol Type { get; } = Named("Runtime", "System.String");
        public Conversion Conversion { get; set; } = new(isIdentity: true, isImplicit: true);
        public ImmutableArray<AttributeData> Attributes { get; set; } = [];
        public int TypeRequests { get; private set; }
        public int AttributeRequests { get; private set; }
        public SpecialType RequestedType { get; private set; }
        public bool IncludeUserDefined { get; private set; }

        public Provider()
        {
            Set(Type, "get_Arity", 0);
            Set(Type, "get_IsUnionCase", false);
            Set(Type, "get_IsUnion", false);
            Set(Type, "get_IsSealedHierarchy", false);
            Set(Type, "get_SpecialType", SpecialType.System_String);
        }

        public NamedTypeSymbol GetSpecialType(SpecialType specialType)
        {
            TypeRequests++;
            RequestedType = specialType;
            return Type;
        }

        public Conversion ClassifyConversion(TypeSymbol source, TypeSymbol destination, bool includeUserDefined)
        {
            IncludeUserDefined = includeUserDefined;
            return Conversion;
        }

        public ImmutableArray<AttributeData> GetAttributesForUsageValidation(Symbol symbol)
        {
            AttributeRequests++;
            return Attributes;
        }
    }
}
