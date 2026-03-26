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

namespace Raven.Editor.Tests;

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

    private abstract class FakeSymbol : ISymbol
    {
        protected FakeSymbol(RavenSymbolKind kind, string name)
        {
            Kind = kind;
            Name = name;
            MetadataName = name;
        }

        public RavenSymbolKind Kind { get; }
        public string Name { get; }
        public string MetadataName { get; }
        public ISymbol? ContainingSymbol => null;
        public IAssemblySymbol? ContainingAssembly => null;
        public IModuleSymbol? ContainingModule => null;
        public INamedTypeSymbol? ContainingType => null;
        public INamespaceSymbol? ContainingNamespace => null;
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
}
