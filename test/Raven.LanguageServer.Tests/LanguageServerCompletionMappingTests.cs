using System.Collections.Immutable;

using OmniSharp.Extensions.LanguageServer.Protocol.Models;

using Raven.CodeAnalysis;
using Raven.CodeAnalysis.Documentation;
using Raven.CodeAnalysis.Symbols;
using Raven.CodeAnalysis.Syntax;
using Raven.CodeAnalysis.Text;
using Raven.LanguageServer;

using RavenLocation = Raven.CodeAnalysis.Location;
using RavenSymbolKind = Raven.CodeAnalysis.SymbolKind;

namespace Raven.LanguageServer.Tests;

public class LanguageServerCompletionMappingTests
{
    [Fact]
    public void ToLspCompletion_MethodCompletion_UsesSnippetAndCaretPlaceholder()
    {
        var text = SourceText.From("counter.Inc");
        var item = new Raven.CodeAnalysis.CompletionItem(
            DisplayText: "Increment",
            InsertionText: "Increment()",
            ReplacementSpan: new TextSpan(8, 3),
            CursorOffset: "Increment()".Length - 1);

        var mapped = CompletionItemMapper.ToLspCompletion(item, text);

        mapped.InsertTextFormat.ShouldBe(InsertTextFormat.Snippet);
        mapped.InsertText.ShouldBe("Increment($0)");
        mapped.CommitCharacters.ShouldNotBeNull();
        mapped.CommitCharacters!.ShouldContain(".");
        mapped.CommitCharacters.ShouldContain("(");
    }

    [Fact]
    public void ToLspCompletion_PlainCompletion_UsesPlainText()
    {
        var text = SourceText.From("counter.Len");
        var item = new Raven.CodeAnalysis.CompletionItem(
            DisplayText: "Length",
            InsertionText: "Length",
            ReplacementSpan: new TextSpan(8, 3),
            CursorOffset: null);

        var mapped = CompletionItemMapper.ToLspCompletion(item, text);

        mapped.InsertTextFormat.ShouldBe(InsertTextFormat.PlainText);
        mapped.InsertText.ShouldBe("Length");
    }

    [Fact]
    public void ToLspCompletion_WildcardImportCompletion_UsesOperatorKindAndTopSort()
    {
        var text = SourceText.From("import System.");
        var item = new Raven.CodeAnalysis.CompletionItem(
            DisplayText: "*",
            InsertionText: "*",
            ReplacementSpan: new TextSpan(text.Length, 0),
            Description: "Import all accessible members");

        var mapped = CompletionItemMapper.ToLspCompletion(item, text);

        mapped.Kind.ShouldBe(CompletionItemKind.Operator);
        mapped.Detail.ShouldBe("Import all accessible members");
        mapped.SortText.ShouldBe("00_*");
        mapped.InsertText.ShouldBe("*");
    }

    [Fact]
    public void ToLspCompletion_ExtensionMethodCompletion_AddsExtensionLabelDetail()
    {
        var text = SourceText.From("widget.");
        var item = new Raven.CodeAnalysis.CompletionItem(
            DisplayText: "Double",
            InsertionText: "Double()",
            ReplacementSpan: new TextSpan(7, 0),
            CursorOffset: "Double()".Length - 1,
            Symbol: new FakeMethodSymbol("Double", isExtensionMethod: true));

        var mapped = CompletionItemMapper.ToLspCompletion(item, text);

        mapped.LabelDetails.ShouldNotBeNull();
        mapped.LabelDetails!.Detail.ShouldBe(" (extension)");
    }

    [Fact]
    public void ToLspCompletion_DelegateTypeCompletion_UsesTypeIconInsteadOfFunctionIcon()
    {
        var text = SourceText.From("Act");
        var item = new Raven.CodeAnalysis.CompletionItem(
            DisplayText: "Action",
            InsertionText: "Action",
            ReplacementSpan: new TextSpan(0, 3),
            Symbol: new FakeNamedTypeSymbol("Action", TypeKind.Delegate));

        var mapped = CompletionItemMapper.ToLspCompletion(item, text);

        mapped.Kind.ShouldBe(CompletionItemKind.Interface);
        mapped.Kind.ShouldNotBe(CompletionItemKind.Function);
    }

    [Fact]
    public void ToLspCompletion_SymbolCompletion_UsesFullyQualifiedContainerDescription()
    {
        var text = SourceText.From("Lis");
        var global = FakeNamespaceSymbol.Global;
        var system = new FakeNamespaceSymbol("System", global);
        var collections = new FakeNamespaceSymbol("Collections", system);
        var generic = new FakeNamespaceSymbol("Generic", collections);
        var item = new Raven.CodeAnalysis.CompletionItem(
            DisplayText: "List",
            InsertionText: "List()",
            ReplacementSpan: new TextSpan(0, 3),
            CursorOffset: "List()".Length - 1,
            Symbol: new FakeValueSymbol("List", generic, generic));

        var mapped = CompletionItemMapper.ToLspCompletion(item, text);

        mapped.LabelDetails.ShouldNotBeNull();
        mapped.LabelDetails!.Description.ShouldBe("System.Collections.Generic");
    }

    private abstract class FakeSymbol : ISymbol
    {
        protected FakeSymbol(
            RavenSymbolKind kind,
            string name,
            ISymbol? containingSymbol = null,
            INamespaceSymbol? containingNamespace = null)
        {
            Kind = kind;
            Name = name;
            MetadataName = name;
            ContainingSymbol = containingSymbol;
            ContainingNamespace = containingNamespace;
        }

        public RavenSymbolKind Kind { get; }
        public string Name { get; }
        public string MetadataName { get; }
        public ISymbol? ContainingSymbol { get; }
        public IAssemblySymbol? ContainingAssembly => null;
        public IModuleSymbol? ContainingModule => null;
        public INamedTypeSymbol? ContainingType => null;
        public INamespaceSymbol? ContainingNamespace { get; }
        public ImmutableArray<RavenLocation> Locations => ImmutableArray<RavenLocation>.Empty;
        public Accessibility DeclaredAccessibility => Accessibility.Public;
        public ImmutableArray<SyntaxReference> DeclaringSyntaxReferences => ImmutableArray<SyntaxReference>.Empty;
        public bool IsImplicitlyDeclared => true;
        public bool IsStatic => false;
        public ISymbol UnderlyingSymbol => this;
        public bool IsAlias => false;
        public ImmutableArray<AttributeData> GetAttributes() => ImmutableArray<AttributeData>.Empty;
        public DocumentationComment? GetDocumentationComment() => null;
        public string? ToMetadataName() => MetadataName;
        public INamespaceSymbol? LookupNamespace(string name) => null;
        public bool Equals(ISymbol? other, SymbolEqualityComparer comparer) => ReferenceEquals(this, other);
        public bool Equals(ISymbol? other) => ReferenceEquals(this, other);
        public override bool Equals(object? obj) => ReferenceEquals(this, obj);
        public override int GetHashCode() => HashCode.Combine(Kind, Name);
        public void Accept(SymbolVisitor visitor) => visitor.DefaultVisit(this);
        public TResult Accept<TResult>(SymbolVisitor<TResult> visitor) => visitor.DefaultVisit(this);
    }

    private sealed class FakeMethodSymbol : FakeSymbol, IMethodSymbol
    {
        public FakeMethodSymbol(string name, bool isExtensionMethod)
            : base(RavenSymbolKind.Method, name)
        {
            IsExtensionMethod = isExtensionMethod;
        }

        public MethodKind MethodKind => MethodKind.Ordinary;
        public ITypeSymbol ReturnType => null!;
        public ImmutableArray<IParameterSymbol> Parameters => ImmutableArray<IParameterSymbol>.Empty;
        public ImmutableArray<AttributeData> GetReturnTypeAttributes() => ImmutableArray<AttributeData>.Empty;
        public IMethodSymbol? OriginalDefinition => this;
        public bool IsAbstract => false;
        public bool IsAsync => false;
        public bool IsCheckedBuiltin => false;
        public bool IsDefinition => true;
        public bool IsExtensionMethod { get; }
        public bool IsExtern => false;
        public bool IsUnsafe => false;
        public bool IsGenericMethod => false;
        public bool IsOverride => false;
        public bool IsReadOnly => false;
        public bool IsFinal => false;
        public bool IsVirtual => false;
        public bool IsIterator => false;
        public IteratorMethodKind IteratorKind => IteratorMethodKind.None;
        public ITypeSymbol? IteratorElementType => null;
        public ImmutableArray<IMethodSymbol> ExplicitInterfaceImplementations => ImmutableArray<IMethodSymbol>.Empty;
        public ImmutableArray<ITypeParameterSymbol> TypeParameters => ImmutableArray<ITypeParameterSymbol>.Empty;
        public ImmutableArray<ITypeSymbol> TypeArguments => ImmutableArray<ITypeSymbol>.Empty;
        public IMethodSymbol? ConstructedFrom => this;
        public bool SetsRequiredMembers => false;
        public IMethodSymbol Construct(params ITypeSymbol[] typeArguments) => this;
    }

    private sealed class FakeValueSymbol : FakeSymbol
    {
        public FakeValueSymbol(string name, ISymbol containingSymbol, INamespaceSymbol containingNamespace)
            : base(RavenSymbolKind.Local, name, containingSymbol, containingNamespace)
        {
        }
    }

    private sealed class FakeNamedTypeSymbol : FakeSymbol, INamedTypeSymbol
    {
        public FakeNamedTypeSymbol(string name, TypeKind typeKind)
            : base(RavenSymbolKind.Type, name)
        {
            TypeKind = typeKind;
        }

        public INamedTypeSymbol? BaseType => null;
        public ITypeSymbol? OriginalDefinition => this;
        public SpecialType SpecialType => SpecialType.None;
        public TypeKind TypeKind { get; }
        public ImmutableArray<INamedTypeSymbol> Interfaces => ImmutableArray<INamedTypeSymbol>.Empty;
        public ImmutableArray<INamedTypeSymbol> AllInterfaces => ImmutableArray<INamedTypeSymbol>.Empty;
        public bool IsNamespace => false;
        public bool IsType => true;
        public int Arity => 0;
        public ImmutableArray<IMethodSymbol> Constructors => ImmutableArray<IMethodSymbol>.Empty;
        public ImmutableArray<IMethodSymbol> InstanceConstructors => ImmutableArray<IMethodSymbol>.Empty;
        public IMethodSymbol? StaticConstructor => null;
        public INamedTypeSymbol UnderlyingTupleType => this;
        public ImmutableArray<IFieldSymbol> TupleElements => ImmutableArray<IFieldSymbol>.Empty;
        public ImmutableArray<ITypeSymbol> TypeArguments => ImmutableArray<ITypeSymbol>.Empty;
        public ImmutableArray<ITypeParameterSymbol> TypeParameters => ImmutableArray<ITypeParameterSymbol>.Empty;
        public ITypeSymbol? ConstructedFrom => this;
        public bool IsAbstract => false;
        public bool IsClosed => true;
        public bool IsGenericType => false;
        public bool IsUnboundGenericType => false;
        public ImmutableArray<ISymbol> GetMembers() => ImmutableArray<ISymbol>.Empty;
        public ImmutableArray<ISymbol> GetMembers(string name) => ImmutableArray<ISymbol>.Empty;
        public ITypeSymbol? LookupType(string name) => null;
        public bool IsMemberDefined(string name, out ISymbol? symbol)
        {
            symbol = null;
            return false;
        }

        public ITypeSymbol Construct(params ITypeSymbol[] typeArguments) => this;
    }

    private sealed class FakeNamespaceSymbol : FakeSymbol, INamespaceSymbol
    {
        public static FakeNamespaceSymbol Global { get; } = new(string.Empty, null, isGlobalNamespace: true);

        private readonly bool _isGlobalNamespace;

        public FakeNamespaceSymbol(string name, INamespaceSymbol? containingNamespace = null)
            : this(name, containingNamespace, isGlobalNamespace: false)
        {
        }

        private FakeNamespaceSymbol(string name, INamespaceSymbol? containingNamespace, bool isGlobalNamespace)
            : base(
                RavenSymbolKind.Namespace,
                name,
                containingSymbol: containingNamespace,
                containingNamespace: containingNamespace)
        {
            _isGlobalNamespace = isGlobalNamespace;
        }

        public bool IsGlobalNamespace => _isGlobalNamespace;
        public bool IsNamespace => true;
        public bool IsType => false;
        public ImmutableArray<INamespaceSymbol> NamespaceMembers => ImmutableArray<INamespaceSymbol>.Empty;
        public ImmutableArray<INamedTypeSymbol> TypeMembers => ImmutableArray<INamedTypeSymbol>.Empty;
        public ImmutableArray<ISymbol> GetMembers() => ImmutableArray<ISymbol>.Empty;
        public ImmutableArray<ISymbol> GetMembers(string name) => ImmutableArray<ISymbol>.Empty;
        public ITypeSymbol? LookupType(string name) => null;
        public bool IsMemberDefined(string name, out ISymbol? symbol)
        {
            symbol = null;
            return false;
        }
    }
}
