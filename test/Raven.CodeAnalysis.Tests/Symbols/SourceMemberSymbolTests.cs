using System.Collections.Immutable;

using Raven.CodeAnalysis.Symbols;
using Raven.CodeAnalysis.Syntax;

using static Raven.CodeAnalysis.Tests.Symbols.MetadataSymbolFixture;

namespace Raven.CodeAnalysis.Tests.Symbols;

[Trait("Target", "Shared")]
public sealed class SourceMemberSymbolTests
{
    [Fact]
    public void SourceEventsUseEventVisitorDispatch()
    {
        var symbol = new SourceEventSymbol("Changed", Type(), Owner(), null, null, [], []);
        Assert.Equal("event", symbol.Accept(new ResultVisitor()));
        var visitor = new Visitor();
        symbol.Accept(visitor);
        Assert.True(visitor.Visited);
    }

    [Fact]
    public void PropertyAttributesRefreshAfterRequiredStateChanges()
    {
        var property = new Property(new Services());
        Assert.Empty(property.GetAttributes());
        property.MarkAsRequired();
        Assert.Equal("RequiredMemberAttribute", Assert.Single(property.GetAttributes()).AttributeClass!.Name);
    }

    [Fact]
    public void FieldAttributesRefreshAfterAssociationAndRequiredStateChanges()
    {
        var services = new Services();
        var field = new Field(services);
        Assert.Empty(field.GetAttributes());
        var property = new Property(services);
        property.SetBackingField(field);
        Assert.Same(property, field.AssociatedSymbol);
        Assert.Same(field, property.BackingField);
        Assert.Contains(field.GetAttributes(), attribute => attribute.AttributeClass!.Name == "CompilerGeneratedAttribute");
        field.MarkAsRequired();
        Assert.Contains(field.GetAttributes(), attribute => attribute.AttributeClass!.Name == "RequiredMemberAttribute");
    }

    [Fact]
    public void DebuggerBrowsableUsesEnumTypedConstant()
    {
        var services = new Services();
        var field = new Field(services, [Declaration("func Value() {}")]);
        field.SetAssociatedProperty(new Property(services));
        var attribute = Assert.Single(field.GetAttributes(), item => item.AttributeClass!.Name == "DebuggerBrowsableAttribute");
        var argument = Assert.Single(attribute.ConstructorArguments);
        Assert.Equal(TypedConstantKind.Enum, argument.Kind);
        Assert.Equal(0, argument.Value);
        Assert.Equal("DebuggerBrowsableState", argument.Type!.Name);
    }

    [Theory]
    [InlineData(false)]
    [InlineData(true)]
    public void DerivedAttributeCachesRefreshAfterAddingDeclarations(bool field)
    {
        var services = new Services();
        SourceSymbol symbol = field ? new Field(services) : new Property(services);
        Assert.Empty(symbol.GetAttributes());
        var reference = Declaration("[Tag] func Value() {}");
        symbol.AddDeclaration(reference.GetSyntax().GetLocation(), reference);
        Assert.Equal("TagAttribute", Assert.Single(symbol.GetAttributes()).AttributeClass!.Name);
    }

    [Theory]
    [InlineData(false)]
    [InlineData(true)]
    public void DeferredSourceCoreDoesNotPoisonDerivedAttributeCaches(bool field)
    {
        var services = new Services { UsesDeferredSourceAttributes = true, SourceDeclarationsComplete = false };
        SourceSymbol symbol;
        if (field)
        {
            var value = new Field(services);
            value.MarkAsRequired();
            symbol = value;
        }
        else
        {
            var value = new Property(services);
            value.MarkAsRequired();
            symbol = value;
        }
        Assert.Empty(symbol.GetAttributes());
        services.SourceDeclarationsComplete = true;
        Assert.Equal("RequiredMemberAttribute", Assert.Single(symbol.GetAttributes()).AttributeClass!.Name);
    }

    [Fact]
    public void PropertyAccessorsStorageKindsAndExplicitMutabilityRemainIndependent()
    {
        var property = new Property(new Services());
        var setter = Make<MethodSymbol>(SymbolKind.Method, "set_Value");
        Set(setter, "get_MethodKind", MethodKind.PropertySet);
        property.SetAccessors(null, setter);
        Assert.True(property.IsMutable);
        Set(setter, "get_MethodKind", MethodKind.InitOnly);
        Assert.False(property.IsMutable);
        property.SetMutability(true);
        Assert.True(property.IsMutable);
        property.MarkSynthesizedBackingFieldAccessors();
        Assert.True(property.IsAutoProperty);
        property.MarkEmitAsFieldOnly();
        Assert.True(property.EmitAsFieldOnly);
        Assert.False(property.IsAutoProperty);
        property.MarkAsPartialDefinition();
        property.MarkAsPartialImplementation();
        Assert.True(property.HasPartialDefinition && property.HasPartialImplementation && property.IsPartialMember);
        property.MarkDeclaredInExtension(null);
        Assert.True(property.IsDeclaredInExtension);
        Assert.Null(property.ExtensionReceiverType);
    }

    [Fact]
    public void EventBackingFieldAccessorsAndExtensionStateRemainLinked()
    {
        var services = new Services();
        var symbol = new SourceEventSymbol("Changed", Type(), Owner(), null, null, [], [], isStatic: true, metadataName: "StoredChanged");
        var field = new Field(services);
        var add = Make<MethodSymbol>(SymbolKind.Method, "add_Changed");
        symbol.SetAccessors(add, null);
        symbol.SetBackingField(field);
        Assert.Same(add, symbol.AddMethod);
        Assert.Null(symbol.RemoveMethod);
        Assert.Same(symbol, field.AssociatedSymbol);
        Assert.True(symbol.IsAutoEvent && symbol.IsStatic);
        Assert.Equal("StoredChanged", symbol.MetadataName);
        symbol.MarkAsPartialDefinition();
        symbol.MarkAsPartialImplementation();
        Assert.True(symbol.IsPartialMember && symbol.HasPartialDefinition && symbol.HasPartialImplementation);
        var receiver = Type();
        symbol.MarkDeclaredInExtension(receiver);
        Assert.Same(receiver, symbol.ExtensionReceiverType);
        Assert.True(symbol.IsDeclaredInExtension);
    }

    private static SyntaxReference Declaration(string text)
        => SyntaxTree.ParseText(text).GetRoot().DescendantNodes().OfType<FunctionStatementSyntax>().Single().GetReference();
    private static TypeSymbol Type() => Make<TypeSymbol>(SymbolKind.Type, "Value");
    private static Symbol Owner() => Make<Symbol>(SymbolKind.Method, "Owner");

    private sealed class Property : SourcePropertySymbol, SourceDeclarationServiceProvider
    {
        public Property(Services services) : base("Value", SourceMemberSymbolTests.Type(), Owner(), null, null, [], []) => DeclarationServices = services;
        public SourceDeclarationServices DeclarationServices { get; }
    }

    private sealed class Field : SourceFieldSymbol, SourceDeclarationServiceProvider
    {
        public Field(Services services, SyntaxReference[]? references = null)
            : base("value", SourceMemberSymbolTests.Type(), false, true, false, null, Owner(), null, null, [], references ?? []) => DeclarationServices = services;
        public SourceDeclarationServices DeclarationServices { get; }
    }

    private sealed class ResultVisitor : SymbolVisitor<string>
    {
        public override string DefaultVisit(Symbol symbol) => "default";
        public override string VisitEvent(EventSymbol symbol) => "event";
    }

    private sealed class Visitor : SymbolVisitor
    {
        public bool Visited { get; private set; }
        public override void VisitEvent(EventSymbol symbol) => Visited = true;
    }

    private sealed class Services : SourceDeclarationServices
    {
        public ImmutableArray<Symbol> GetFunctionContractMembers(TypeSymbol owner) => [];

        private readonly Dictionary<string, NamedTypeSymbol> _types = new();
        public SyntaxTree[] SyntaxTrees => [];
        public bool UsesDeferredSourceAttributes { get; init; }
        public bool SourceDeclarationsComplete { get; set; } = true;
        public SourceAttributeDeclarationContext GetAttributeDeclarationContext(Symbol owner)
            => new(false, false, false, owner is SourcePropertySymbol, owner is SourceEventSymbol, false);
        public bool TryGetCurrentDeclaringSyntax(SyntaxReference reference, out SyntaxNode syntax)
        {
            syntax = reference.GetSyntax();
            return true;
        }
        public SourceAttributeBinding? BindAttribute(Symbol owner, AttributeSyntax syntax)
            => new(new AttributeData(GetTypeByMetadataName("TagAttribute"), null, [], [], syntax.GetReference()), syntax, new DiagnosticBag());
        public void PublishAttributeDiagnostics(SourceAttributeBinding attribute) => throw new InvalidOperationException();
        public NamedTypeSymbol? GetTypeByMetadataName(string metadataName)
        {
            if (!SourceDeclarationsComplete || metadataName == "System.AttributeUsageAttribute") return null;
            if (_types.TryGetValue(metadataName, out var found)) return found;
            var type = Named("Test", metadataName);
            Set(type, "get_TypeParameters", ImmutableArray<TypeParameterSymbol>.Empty);
            Set(type, "get_Arity", 0);
            if (metadataName.EndsWith("DebuggerBrowsableState", StringComparison.Ordinal)) Set(type, "get_TypeKind", TypeKind.Enum);
            var constructor = Make<MethodSymbol>(SymbolKind.Method, ".ctor");
            Set(constructor, "get_IsStatic", false);
            Set(constructor, "get_Parameters", metadataName.EndsWith("DebuggerBrowsableAttribute", StringComparison.Ordinal)
                ? ImmutableArray.Create(Make<ParameterSymbol>(SymbolKind.Parameter, "state")) : ImmutableArray<ParameterSymbol>.Empty);
            Set(type, "get_Constructors", ImmutableArray.Create(constructor));
            _types.Add(metadataName, type);
            return type;
        }
        public bool IsPrimaryConstructor(Symbol symbol) => false;
        public bool IsNamespaceContainer(Symbol symbol) => false;
        public NamedTypeSymbol GetSpecialType(SpecialType specialType) => throw new NotSupportedException();
        public Conversion ClassifyConversion(TypeSymbol source, TypeSymbol destination, bool includeUserDefined) => throw new NotSupportedException();
        public ImmutableArray<AttributeData> GetAttributesForUsageValidation(Symbol symbol) => [];
    }
}
