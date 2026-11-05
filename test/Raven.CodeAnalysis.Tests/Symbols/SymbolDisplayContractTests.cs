using System.Collections.Immutable;
using System.Reflection;

using Raven.CodeAnalysis;

using Xunit;

namespace Raven.CodeAnalysis.Tests.Symbols;

[Trait("Target", "Shared")]
public sealed class SymbolDisplayContractTests
{
    [Theory]
    [InlineData("Int32", SpecialType.System_Int32, "int")]
    [InlineData("String", SpecialType.System_String, "string")]
    [InlineData("Boolean", SpecialType.System_Boolean, "bool")]
    [InlineData("Unit", SpecialType.System_Unit, "()")]
    public void FrameworkTypesUseKeywords(string name, SpecialType special, string expected)
        => Assert.Equal(expected, Display(Type(name, special, Namespace("System"))));

    [Theory]
    [InlineData(1, null, "Widget[]")]
    [InlineData(2, null, "Widget[,]")]
    [InlineData(1, 10, "Widget[10]")]
    public void ArraysRetainRankAndFixedLength(int rank, int? length, string expected)
    {
        var array = Make<ArrayTypeSymbol>("array", TypeKind.Array);
        Set(array, "get_ElementType", Type("Widget"));
        Set(array, "get_Rank", rank);
        Set(array, "get_FixedLength", length);
        Assert.Equal(expected, Display(array));
    }

    [Fact]
    public void PointerUsesElementDisplay()
    {
        var pointer = Make<PointerTypeSymbol>("pointer", TypeKind.Pointer);
        Set(pointer, "get_PointedAtType", Type("Int32", SpecialType.System_Int32, Namespace("System")));
        Assert.Equal("*int", Display(pointer));
    }

    [Theory]
    [InlineData("Widget", "Widget")]
    [InlineData("class", "@class")]
    public void SimpleNamesPreserveKeywordEscaping(string name, string expected)
        => Assert.Equal(expected, SymbolExtensions.ToDisplayStringForDiagnostics(Type(name),
            SymbolDisplayFormat.MinimallyQualifiedFormat.WithMiscellaneousOptions(SymbolDisplayMiscellaneousOptions.UseSpecialTypes | SymbolDisplayMiscellaneousOptions.EscapeKeywordIdentifiers)));

    [Fact]
    public void ConstructedGenericArgumentsUseTheirOwnDisplay()
    {
        var definition = Type("Box");
        var box = Type("Box");
        Set(box, "get_Arity", 1);
        Set(box, "get_IsGenericType", true);
        var parameter = Make<TypeParameterSymbol>("T", TypeKind.TypeParameter);
        Set(parameter, "get_ContainingSymbol", definition);
        Set(box, "get_TypeParameters", ImmutableArray.Create(parameter));
        Set(box, "get_OriginalDefinition", definition);
        Set(box, "get_ConstructedFrom", definition);
        Set(box, "get_TypeArguments", ImmutableArray.Create<TypeSymbol>(Type("Int32", SpecialType.System_Int32, Namespace("System"))));
        Assert.Equal("Box<int>", Display(box));
    }

    [Fact]
    public void FullyQualifiedNamesIncludeNamespace()
    {
        var type = Type("Widget", ns: Namespace("Example"));
        var format = SymbolDisplayFormat.MinimallyQualifiedFormat.WithTypeQualificationStyle(SymbolDisplayTypeQualificationStyle.NameAndContainingTypesAndNamespaces);
        Assert.Equal("Example.Widget", SymbolExtensions.ToDisplayStringForDiagnostics(type, format));
    }

    [Theory]
    [InlineData(RefKind.None, "item: int")]
    [InlineData(RefKind.Ref, "ref item: int")]
    [InlineData(RefKind.Out, "out item: int")]
    [InlineData(RefKind.In, "in item: int")]
    public void ParameterRefKindsArePreserved(RefKind kind, string expected)
    {
        var parameter = Parameter(Type("Int32", SpecialType.System_Int32, Namespace("System")));
        Set(parameter, "get_RefKind", kind);
        Assert.Equal(expected, SymbolExtensions.ToDisplayString(parameter));
    }

    [Theory]
    [InlineData("String", SpecialType.System_String, "hello", "item: string = \"hello\"")]
    [InlineData("String", SpecialType.System_String, null, "item: string = null")]
    [InlineData("Int32", SpecialType.System_Int32, 42, "item: int = 42")]
    [InlineData("Int32", SpecialType.System_Int32, null, "item: int = default")]
    [InlineData("Boolean", SpecialType.System_Boolean, true, "item: bool = true")]
    public void ParameterDefaultsPreserveConstantSpelling(string name, SpecialType special, object? value, string expected)
    {
        var parameter = Parameter(Type(name, special, Namespace("System")));
        Set(parameter, "get_HasExplicitDefaultValue", true);
        Set(parameter, "get_ExplicitDefaultValue", value);
        Assert.Equal(expected, SymbolExtensions.ToDisplayString(parameter));
    }

    [Theory]
    [InlineData(MethodKind.Ordinary, "Run", "func Run(item: int) -> bool")]
    [InlineData(MethodKind.Constructor, ".ctor", "init(item: int) -> ()")]
    [InlineData(MethodKind.Ordinary, "Invoke", "func self(item: int) -> bool")]
    [InlineData(MethodKind.UserDefinedOperator, "op_Addition", "func +(item: int) -> bool")]
    public void MethodKindsUseTheirLanguageSpelling(MethodKind kind, string name, string expected)
    {
        var method = Method(name, kind);
        Set(method, "get_Parameters", ImmutableArray.Create(Parameter(Type("Int32", SpecialType.System_Int32, Namespace("System")))));
        Assert.Equal(expected, SymbolExtensions.ToDisplayString(method));
    }

    [Theory]
    [InlineData(Accessibility.Public, "")]
    [InlineData(Accessibility.ProtectedOrInternal, "protected internal")]
    [InlineData(Accessibility.ProtectedAndInternal, "private protected")]
    public void AccessibilityAndModifiersAreIncludedWhenRequested(Accessibility accessibility, string expected)
    {
        var method = Method("Run");
        Set(method, "get_DeclaredAccessibility", accessibility);
        Set(method, "get_IsStatic", true);
        Set(method, "get_IsAsync", true);
        var format = SymbolDisplayFormat.RavenSignatureFormat.WithMemberOptions(SymbolDisplayFormat.RavenSignatureFormat.MemberOptions | SymbolDisplayMemberOptions.IncludeAccessibility);
        Assert.Equal((expected.Length == 0 ? "" : expected + " ") + "static async func Run() -> bool", SymbolExtensions.ToDisplayString(method, format));
    }

    [Theory]
    [InlineData(false, "const Count: int = 42")]
    [InlineData(true, "static readonly field Count: int")]
    public void FieldsPreserveConstantsAndStorageModifiers(bool readOnly, string expected)
    {
        var field = Make<FieldSymbol>("Count");
        Set(field, "get_Kind", SymbolKind.Field);
        Set(field, "get_Type", Type("Int32", SpecialType.System_Int32, Namespace("System")));
        Set(field, "get_IsConst", !readOnly);
        Set(field, "get_IsStatic", true);
        Set(field, "get_IsReadOnly", readOnly);
        Set(field, "GetConstantValue", 42);
        Assert.Equal(expected, SymbolExtensions.ToDisplayString(field));
    }

    [Theory]
    [InlineData(false, "val count: int")]
    [InlineData(true, "var count: int")]
    public void LocalBindingDisplayPreservesMutability(bool mutable, string expected)
    {
        var local = Make<LocalSymbol>("count");
        Set(local, "get_Kind", SymbolKind.Local);
        Set(local, "get_Type", Type("Int32", SpecialType.System_Int32, Namespace("System")));
        Set(local, "get_IsConst", false);
        Set(local, "get_IsMutable", mutable);
        Assert.Equal(expected, SymbolExtensions.ToDisplayString(local));
    }

    [Theory]
    [InlineData(false, "namespace Example")]
    [InlineData(true, "module Example")]
    public void NamespaceAndModuleKindsAreExplicit(bool module, string expected)
    {
        var ns = Namespace("Example");
        Set(ns, "get_IsModule", module);
        var format = SymbolDisplayFormat.MinimallyQualifiedFormat.WithKindOptions(SymbolDisplayKindOptions.IncludeNamespaceKeyword);
        Assert.Equal(expected, SymbolExtensions.ToDisplayString(ns, format));
    }

    [Theory]
    [InlineData(false, "Value: int { get; }")]
    [InlineData(true, "Value: int { get; private init; }")]
    public void PropertiesShowAccessorKindsAndRestrictedAccessibility(bool hasSetter, string expected)
    {
        var property = Make<PropertySymbol>("Value");
        Set(property, "get_Kind", SymbolKind.Property);
        Set(property, "get_IsIndexer", false);
        Set(property, "get_ExplicitInterfaceImplementations", ImmutableArray<PropertySymbol>.Empty);
        Set(property, "get_IsStatic", false);
        Set(property, "get_Type", Type("Int32", SpecialType.System_Int32, Namespace("System")));
        Set(property, "get_GetMethod", Method("get_Value"));
        var setter = Method("set_Value", MethodKind.InitOnly);
        Set(setter, "get_DeclaredAccessibility", Accessibility.Private);
        Set(property, "get_SetMethod", hasSetter ? setter : null);
        var format = SymbolDisplayFormat.MinimallyQualifiedFormat.WithMemberOptions(SymbolDisplayFormat.MinimallyQualifiedFormat.MemberOptions | SymbolDisplayMemberOptions.IncludeAccessibility);
        Assert.Equal(expected, SymbolExtensions.ToDisplayString(property, format));
    }

    [Theory]
    [InlineData("Action", 0, "() -> ()")]
    [InlineData("Action", 1, "int -> ()")]
    [InlineData("Func", 1, "() -> int")]
    [InlineData("Func", 2, "int -> string")]
    [InlineData("Func", 3, "(int, string) -> bool")]
    public void FrameworkDelegatesUseFunctionSignatures(string name, int arity, string expected)
    {
        var callable = Type(name, ns: Namespace("System"));
        Set(callable, "get_TypeKind", TypeKind.Delegate);
        var arguments = new TypeSymbol[] { Type("Int32", SpecialType.System_Int32, Namespace("System")), Type("String", SpecialType.System_String, Namespace("System")), Type("Boolean", SpecialType.System_Boolean, Namespace("System")) };
        Set(callable, "get_TypeArguments", arguments.Take(arity).ToImmutableArray());
        Assert.Equal(expected, Display(callable));
    }

    [Theory]
    [InlineData(false, "Item1", "(int, string)")]
    [InlineData(true, "Item1", "(int, text: string)")]
    [InlineData(true, "count", "(count: int, text: string)")]
    public void TupleElementNamesFollowDisplayOptions(bool names, string firstName, string expected)
    {
        var tuple = Make<TupleTypeSymbol>("Tuple", TypeKind.Tuple);
        var first = Make<FieldSymbol>(firstName);
        Set(first, "get_Type", Type("Int32", SpecialType.System_Int32, Namespace("System")));
        var second = Make<FieldSymbol>("text");
        Set(second, "get_Type", Type("String", SpecialType.System_String, Namespace("System")));
        Set(tuple, "get_TupleElements", ImmutableArray.Create(first, second));
        var format = SymbolDisplayFormat.MinimallyQualifiedFormat;
        if (names) format = format.WithMiscellaneousOptions(format.MiscellaneousOptions | SymbolDisplayMiscellaneousOptions.IncludeTupleElementNames);
        Assert.Equal(expected, SymbolExtensions.ToDisplayStringForDiagnostics(tuple, format));
    }

    private static MethodSymbol Method(string name, MethodKind kind = MethodKind.Ordinary)
    {
        var method = Make<MethodSymbol>(name);
        Set(method, "get_Kind", SymbolKind.Method);
        Set(method, "get_MethodKind", kind);
        Set(method, "get_IsConstructor", kind == MethodKind.Constructor);
        Set(method, "get_ExplicitInterfaceImplementations", ImmutableArray<MethodSymbol>.Empty);
        Set(method, "get_IsLambda", false);
        Set(method, "get_TypeParameters", ImmutableArray<TypeParameterSymbol>.Empty);
        Set(method, "get_TypeArguments", ImmutableArray<TypeSymbol>.Empty);
        Set(method, "get_Parameters", ImmutableArray<ParameterSymbol>.Empty);
        Set(method, "get_ReturnType", kind == MethodKind.Constructor
            ? Type("Unit", SpecialType.System_Unit, Namespace("System"))
            : Type("Boolean", SpecialType.System_Boolean, Namespace("System")));
        foreach (var flag in new[] { "IsStatic", "IsAbstract", "IsFinal", "IsOverride", "IsVirtual", "IsAsync" })
            Set(method, "get_" + flag, false);
        return method;
    }

    private static ParameterSymbol Parameter(TypeSymbol type)
    {
        var parameter = Make<ParameterSymbol>("item");
        Set(parameter, "get_Kind", SymbolKind.Parameter);
        Set(parameter, "get_Type", type);
        Set(parameter, "get_HasImplicitName", false);
        Set(parameter, "get_BindingPattern", null);
        Set(parameter, "get_IsVarParams", false);
        Set(parameter, "get_RefKind", RefKind.None);
        Set(parameter, "get_HasExplicitDefaultValue", false);
        Set(parameter, "get_IsThis", false);
        Set(parameter, "get_DeclaringSyntaxReferences", ImmutableArray<SyntaxReference>.Empty);
        return parameter;
    }

    private static string Display(TypeSymbol symbol)
        => SymbolExtensions.ToDisplayStringForDiagnostics(symbol, SymbolDisplayFormat.MinimallyQualifiedFormat);

    private static NamedTypeSymbol Type(string name, SpecialType special = SpecialType.None, NamespaceSymbol? ns = null)
    {
        var type = Make<NamedTypeSymbol>(name, special == SpecialType.System_Unit ? TypeKind.Unit : TypeKind.Class);
        Set(type, "get_SpecialType", special);
        Set(type, "get_IsValueType", special is SpecialType.System_Int32 or SpecialType.System_Boolean or SpecialType.System_Unit);
        Set(type, "ToFullyQualifiedMetadataName", (ns is null || ns.IsGlobalNamespace ? "" : ns.Name + ".") + name);
        Set(type, "get_ContainingNamespace", ns ?? Namespace("", true));
        Set(type, "get_TypeArguments", ImmutableArray<TypeSymbol>.Empty);
        Set(type, "get_TypeParameters", ImmutableArray<TypeParameterSymbol>.Empty);
        Set(type, "get_Arity", 0);
        Set(type, "get_IsGenericType", false);
        Set(type, "get_OriginalDefinition", type);
        Set(type, "get_ConstructedFrom", type);
        Set(type, "get_IsUnion", false);
        return type;
    }

    private static NamespaceSymbol Namespace(string name, bool global = false)
    {
        var ns = Make<NamespaceSymbol>(name);
        Set(ns, "get_Kind", SymbolKind.Namespace);
        Set(ns, "get_IsNamespace", true);
        Set(ns, "get_IsGlobalNamespace", global);
        Set(ns, "get_ContainingNamespace", global ? null : Namespace("", true));
        return ns;
    }

    private static T Make<T>(string name, TypeKind kind = TypeKind.Class) where T : class
    {
        var value = DispatchProxy.Create<T, SymbolProxy>();
        Set(value, "get_Kind", SymbolKind.Type);
        Set(value, "get_Name", name);
        Set(value, "get_MetadataName", name);
        Set(value, "get_TypeKind", kind);
        Set(value, "get_SpecialType", SpecialType.None);
        Set(value, "get_ContainingType", null);
        Set(value, "get_ContainingNamespace", null);
        Set(value, "get_IsNamespace", false);
        Set(value, "get_IsAlias", false);
        Set(value, "get_DeclaredAccessibility", Accessibility.Public);
        Set(value, "get_DeclaringSyntaxReferences", ImmutableArray<SyntaxReference>.Empty);
        return value;
    }

    private static void Set(object target, string property, object? value)
        => ((SymbolProxy)target).Values[property] = value;

    public class SymbolProxy : DispatchProxy
    {
        public Dictionary<string, object?> Values { get; } = new();
        protected override object? Invoke(MethodInfo? method, object?[]? args)
            => Values.TryGetValue(method!.Name, out var value) ? value : throw new InvalidOperationException(method.Name);
    }
}
