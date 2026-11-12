using System.Collections.Immutable;

using Raven.CodeAnalysis.Symbols;

using static Raven.CodeAnalysis.Tests.Symbols.MetadataSymbolFixture;

namespace Raven.CodeAnalysis.Tests.Symbols;

[Trait("Target", "Shared")]
public sealed class RefTypeContractTests
{
    [Fact]
    public void DetachedElementKeepsOptionalOwners()
    {
        var element = Element(detached: true);
        var reference = new RefTypeSymbol(element);
        Assert.Same(element, reference.ElementType);
        Assert.Null(reference.ContainingSymbol);
        Assert.Null(reference.ContainingAssembly);
        Assert.Null(reference.ContainingModule);
        Assert.Equal("Value&", reference.Name);
        Assert.Equal("Value&", reference.MetadataName);
        Assert.Empty(reference.GetAttributes());
        Assert.Null(reference.GetDocumentationComment());
    }

    [Fact]
    public void MissingElementIsRejectedBeforeReadingItsOwner()
    {
        var error = Assert.Throws<ArgumentNullException>(() => new RefTypeSymbol(null!));
        Assert.Equal("elementType", error.ParamName);
    }

    [Fact]
    public void EqualElementSymbolsHaveEqualWrapperHashes()
    {
        var first = new RefTypeSymbol(Element());
        var second = new RefTypeSymbol(Element());
        Assert.NotSame(first.ElementType, second.ElementType);
        Assert.True(SymbolEqualityComparer.Default.Equals(first.ElementType, second.ElementType));
        Assert.True(first.Equals((object)second));
        Assert.Equal(SymbolEqualityComparer.Default.GetHashCode(first.ElementType), first.GetHashCode());
        Assert.Equal(first.GetHashCode(), second.GetHashCode());
        Assert.Single(new HashSet<RefTypeSymbol> { first, second });
    }

    [Fact]
    public void TypedEqualityAndOperatorsAgreeWithObjectEquality()
    {
        var first = new RefTypeSymbol(Element());
        var second = new RefTypeSymbol(Element());
        Assert.True(Assert.IsAssignableFrom<IEquatable<RefTypeSymbol>>(first).Equals(second));
        Assert.True(first == second);
        Assert.False(first != second);
        RefTypeSymbol? absent = null;
        Assert.False(first == absent);
        Assert.True(first != absent);
        Assert.True(absent == (RefTypeSymbol?)null);
        Assert.False(absent == first);
        Assert.False(first.Equals((RefTypeSymbol?)null));
        var differentElement = Element();
        Set(differentElement, "get_SpecialType", SpecialType.System_UInt32);
        Set(differentElement, "get_Name", "UnsignedValue");
        Set(differentElement, "get_MetadataName", "UnsignedValue");
        var different = new RefTypeSymbol(differentElement);
        Assert.False(first == different);
        Assert.True(first != different);
    }

    [Fact]
    public void VisitorsUseDefaultDispatchWithoutReenteringVisit()
    {
        var reference = new RefTypeSymbol(Element());
        var visitor = new ReentryGuardVisitor();
        reference.Accept(visitor);
        Assert.Equal(1, visitor.DefaultVisits);
        Assert.Equal(0, visitor.Reentries);
        var generic = new ReentryGuardGenericVisitor();
        Assert.Same(reference, reference.Accept(generic));
        Assert.Equal(0, generic.Reentries);
    }

    [Fact]
    public void MembersAndLookupAreForwardedToTheElement()
    {
        var element = Element();
        var field = Make<FieldSymbol>(SymbolKind.Field, "Field");
        var nested = Make<NamedTypeSymbol>(SymbolKind.Type, "Nested", TypeKind.Class);
        var members = ImmutableArray.Create<Symbol>(field, nested);
        Set(element, "GetMembers", (Func<object?[]?, object?>)(arguments =>
            arguments is { Length: 1 }
                ? members.Where(member => member.Name == (string)arguments[0]!).ToImmutableArray()
                : members));
        var reference = new RefTypeSymbol(element);
        Assert.Equal(members, reference.GetMembers());
        Assert.Same(nested, reference.LookupType("Nested"));
        Assert.Null(reference.LookupType("Field"));
        Assert.True(reference.IsMemberDefined("Field", out var found));
        Assert.Same(field, found);
        Assert.False(reference.IsMemberDefined("Missing", out var missing));
        Assert.Null(missing);
        Assert.Same(field, Assert.Single(reference.ResolveMembers("Field")));
    }

    [Fact]
    public void NamespaceOwnershipIsPreservedWithoutSourceDeclarationState()
    {
        var element = Element();
        var owner = Make<NamespaceSymbol>(SymbolKind.Namespace, "Owner");
        var assembly = Make<AssemblySymbol>(SymbolKind.Assembly, "Assembly");
        var module = Make<ModuleSymbol>(SymbolKind.Module, "Module");
        Set(owner, "get_ContainingAssembly", assembly);
        Set(owner, "get_ContainingModule", module);
        Set(element, "get_ContainingNamespace", owner);
        var reference = new RefTypeSymbol(element);
        Assert.Same(owner, reference.ContainingNamespace);
        Assert.Same(assembly, reference.ContainingAssembly);
        Assert.Same(module, reference.ContainingModule);
        Assert.Empty(reference.DeclaringSyntaxReferences);
        Assert.Empty(reference.GetAttributes());
        Assert.Null(reference.GetDocumentationComment());
        Assert.True(reference.IsImplicitlyDeclared);
    }

    private sealed class ReentryGuardVisitor : SymbolVisitor
    {
        public int Reentries { get; private set; }
        public int DefaultVisits { get; private set; }
        public override void Visit(Symbol symbol)
        {
            if (++Reentries > 1)
                throw new InvalidOperationException("Accept reentered Visit instead of dispatching.");
            base.Visit(symbol);
        }
        public override void DefaultVisit(Symbol symbol) => DefaultVisits++;
    }

    private sealed class ReentryGuardGenericVisitor : SymbolVisitor<Symbol>
    {
        public int Reentries { get; private set; }
        public override Symbol? Visit(Symbol? symbol)
        {
            if (++Reentries > 1)
                throw new InvalidOperationException("Accept reentered Visit instead of dispatching.");
            return base.Visit(symbol);
        }
        public override Symbol DefaultVisit(Symbol symbol) => symbol;
    }

    private static NamedTypeSymbol Element(bool detached = false)
    {
        var element = Make<NamedTypeSymbol>(SymbolKind.Type, "Value", TypeKind.Struct);
        Set(element, "get_SpecialType", SpecialType.System_Int32);
        Set(element, "get_BaseType", null);
        if (!detached)
            Set(element, "get_ContainingSymbol", Make<NamespaceSymbol>(SymbolKind.Namespace, "Owner"));
        return element;
    }
}
