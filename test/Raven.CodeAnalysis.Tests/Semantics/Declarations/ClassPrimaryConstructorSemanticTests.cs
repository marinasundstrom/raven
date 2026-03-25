using System.Linq;

using Raven.CodeAnalysis;
using Raven.CodeAnalysis.Symbols;
using Raven.CodeAnalysis.Syntax;

using Xunit;

namespace Raven.CodeAnalysis.Semantics.Tests;

public sealed class ClassPrimaryConstructorSemanticTests : CompilationTestBase
{
    [Fact]
    public void PrimaryConstructor_ParameterWithoutBindingKeyword_IsCapturedButNotPromoted()
    {
        var source = """
            class Person(name: string)
            {
                func GetName() -> string => name
            }
            """;

        var (compilation, tree) = CreateCompilation(source);
        var model = compilation.GetSemanticModel(tree);

        var identifier = tree.GetRoot().DescendantNodes()
            .OfType<IdentifierNameSyntax>()
            .Single(id => id.Identifier.ValueText == "name");

        var type = Assert.IsAssignableFrom<INamedTypeSymbol>(compilation.SourceGlobalNamespace.LookupType("Person"));
        Assert.Empty(type.GetMembers("name").OfType<IPropertySymbol>());
        Assert.Single(type.GetMembers("name").OfType<IFieldSymbol>());

        var parameterSyntax = tree.GetRoot().DescendantNodes().OfType<ParameterSyntax>().Single();
        var parameterSymbol = Assert.IsAssignableFrom<IParameterSymbol>(model.GetDeclaredSymbol(parameterSyntax));

        var symbol = model.GetSymbolInfo(identifier).Symbol;
        Assert.Same(parameterSymbol, symbol);
        Assert.Empty(compilation.GetDiagnostics());
    }

    [Fact]
    public void PrimaryConstructor_CapturedParameterIdentifierAccess_BindsToParameterSymbol()
    {
        var source = """
            class Foo(b: int) {
                func self(factor: int) -> int {
                    return b * factor
                }
            }
            """;

        var (compilation, tree) = CreateCompilation(source);
        var model = compilation.GetSemanticModel(tree);

        var methodIdentifier = tree.GetRoot().DescendantNodes()
            .OfType<IdentifierNameSyntax>()
            .First(id => id.Identifier.ValueText == "b");
        var parameterSyntax = tree.GetRoot().DescendantNodes().OfType<ParameterSyntax>().First();
        var parameterSymbol = Assert.IsAssignableFrom<IParameterSymbol>(model.GetDeclaredSymbol(parameterSyntax));

        var methodSymbol = model.GetSymbolInfo(methodIdentifier).Symbol;
        Assert.Same(parameterSymbol, methodSymbol);
        Assert.Empty(compilation.GetDiagnostics());
    }

    [Fact]
    public void PrimaryConstructor_ValParameter_PromotesToReadOnlyProperty()
    {
        var source = """
            class Person(val Name: string)
            {
                func GetName() -> string => Name
            }
            """;

        var (compilation, tree) = CreateCompilation(source);
        var model = compilation.GetSemanticModel(tree);

        var type = Assert.IsAssignableFrom<INamedTypeSymbol>(compilation.SourceGlobalNamespace.LookupType("Person"));
        var property = Assert.IsAssignableFrom<IPropertySymbol>(Assert.Single(type.GetMembers("Name")));
        Assert.False(property.IsMutable);
        Assert.Equal(MethodKind.InitOnly, property.SetMethod?.MethodKind);

        var parameterSyntax = tree.GetRoot().DescendantNodes().OfType<ParameterSyntax>().Single();
        var parameterSymbol = Assert.IsAssignableFrom<IParameterSymbol>(model.GetDeclaredSymbol(parameterSyntax));
        Assert.False(parameterSymbol.IsMutable);

        var identifier = tree.GetRoot().DescendantNodes()
            .OfType<IdentifierNameSyntax>()
            .Single(id => id.Identifier.ValueText == "Name");
        Assert.Same(parameterSymbol, model.GetSymbolInfo(identifier).Symbol);

        Assert.Empty(compilation.GetDiagnostics());
    }

    [Fact]
    public void PrimaryConstructor_VarParameter_PromotesToMutableProperty()
    {
        var source = """
            class Counter(var Count: int)
            {
                func Increment() {
                    Count = Count + 1
                }
            }
            """;

        var (compilation, tree) = CreateCompilation(source);
        var model = compilation.GetSemanticModel(tree);

        var type = Assert.IsAssignableFrom<INamedTypeSymbol>(compilation.SourceGlobalNamespace.LookupType("Counter"));
        var property = Assert.IsAssignableFrom<IPropertySymbol>(Assert.Single(type.GetMembers("Count")));
        Assert.True(property.IsMutable);
        Assert.Equal(MethodKind.PropertySet, property.SetMethod?.MethodKind);

        var parameterSyntax = tree.GetRoot().DescendantNodes().OfType<ParameterSyntax>().Single();
        var parameterSymbol = Assert.IsAssignableFrom<IParameterSymbol>(model.GetDeclaredSymbol(parameterSyntax));
        Assert.True(parameterSymbol.IsMutable);

        var propertyIdentifiers = tree.GetRoot().DescendantNodes()
            .OfType<IdentifierNameSyntax>()
            .Where(id => id.Identifier.ValueText == "Count")
            .ToArray();
        Assert.All(propertyIdentifiers, id => Assert.Same(parameterSymbol, model.GetSymbolInfo(id).Symbol));

        Assert.Empty(compilation.GetDiagnostics());
    }

    [Fact]
    public void PrimaryConstructor_ComplexParameterTypes_BindViaBindTypeSyntaxPath()
    {
        var source = """
            class Pipeline(callback: (int, string) -> bool, pair: (left: int, right: string))
            {
                func Accepts() -> () { }
            }
            """;

        var (compilation, tree) = CreateCompilation(source, new CompilationOptions(OutputKind.DynamicallyLinkedLibrary));
        var model = compilation.GetSemanticModel(tree);
        var parameters = tree.GetRoot().DescendantNodes().OfType<ParameterSyntax>().ToArray();

        Assert.Empty(compilation.GetDiagnostics());

        var callbackParameter = Assert.IsAssignableFrom<IParameterSymbol>(model.GetDeclaredSymbol(parameters[0]));
        var callbackType = Assert.IsAssignableFrom<INamedTypeSymbol>(callbackParameter.Type);
        Assert.Equal(TypeKind.Delegate, callbackType.TypeKind);
        var invoke = callbackType.GetDelegateInvokeMethod();
        Assert.NotNull(invoke);
        Assert.Equal(SpecialType.System_Boolean, invoke!.ReturnType.SpecialType);
        Assert.Equal(2, invoke.Parameters.Length);
        Assert.Equal(SpecialType.System_Int32, invoke.Parameters[0].Type.SpecialType);
        Assert.Equal(SpecialType.System_String, invoke.Parameters[1].Type.SpecialType);

        var pairParameter = Assert.IsAssignableFrom<IParameterSymbol>(model.GetDeclaredSymbol(parameters[1]));
        var tupleType = Assert.IsAssignableFrom<ITupleTypeSymbol>(pairParameter.Type);
        Assert.Equal("left", tupleType.TupleElements[0].Name);
        Assert.Equal(SpecialType.System_Int32, tupleType.TupleElements[0].Type.SpecialType);
        Assert.Equal("right", tupleType.TupleElements[1].Name);
        Assert.Equal(SpecialType.System_String, tupleType.TupleElements[1].Type.SpecialType);
    }

    [Fact]
    public void PrimaryConstructor_PromotedParameterAccessibility_AppliesToSynthesizedProperty()
    {
        var source = """
            class Person(internal var Name: string)
            {
                func GetName() -> string => Name
            }
            """;

        var (compilation, tree) = CreateCompilation(source);
        _ = compilation.GetSemanticModel(tree);

        var type = Assert.IsAssignableFrom<INamedTypeSymbol>(compilation.SourceGlobalNamespace.LookupType("Person"));
        var property = Assert.IsAssignableFrom<IPropertySymbol>(Assert.Single(type.GetMembers("Name")));

        Assert.Equal(Accessibility.Internal, property.DeclaredAccessibility);
        Assert.Equal(Accessibility.Internal, property.GetMethod?.DeclaredAccessibility);
        Assert.Equal(Accessibility.Internal, property.SetMethod?.DeclaredAccessibility);
        Assert.Empty(compilation.GetDiagnostics());
    }

    [Fact]
    public void PrimaryConstructor_AccessibilityOnNonPromotedParameter_ReportsDiagnostic()
    {
        var source = """
            class Person(private name: string)
            {
                func GetName() -> string => name
            }
            """;

        var (compilation, _) = CreateCompilation(source);
        var diagnostics = compilation.GetDiagnostics();

        Assert.Contains(diagnostics, d => d.Descriptor == CompilerDiagnostics.ModifierNotValidOnMember);
    }

    [Fact]
    public void PrimaryConstructor_AccessibilityOnByRefParameter_ReportsDiagnostic()
    {
        var source = """
            class Counter(private ref value: int)
            {
            }
            """;

        var (compilation, _) = CreateCompilation(source);
        var diagnostics = compilation.GetDiagnostics();

        Assert.Contains(diagnostics, d => d.Descriptor == CompilerDiagnostics.ModifierNotValidOnMember);
    }

    [Fact]
    public void PrivatePromotedPrimaryConstructorParameter_MemberAccess_ResolvesToPropertySymbol()
    {
        var source = """
            class Foo(private var name: string) {
                func Test() -> string => self.name
            }
            """;

        var (compilation, tree) = CreateCompilation(source);
        var model = compilation.GetSemanticModel(tree);

        var memberAccess = tree.GetRoot().DescendantNodes()
            .OfType<MemberAccessExpressionSyntax>()
            .Single();

        var symbol = model.GetSymbolInfo(memberAccess).Symbol;
        var property = Assert.IsAssignableFrom<IPropertySymbol>(symbol);
        Assert.Equal(Accessibility.Private, property.DeclaredAccessibility);
        Assert.Equal("name", property.Name);
    }

    [Fact]
    public void PrimaryConstructor_SynthesizesPublicConstructor()
    {
        var source = """
            class Foo(value: int) {
            }
            """;

        var (compilation, tree) = CreateCompilation(source);
        var model = compilation.GetSemanticModel(tree);

        var typeDeclaration = tree.GetRoot().DescendantNodes().OfType<ClassDeclarationSyntax>().Single();
        var type = Assert.IsAssignableFrom<INamedTypeSymbol>(model.GetDeclaredSymbol(typeDeclaration));
        var constructor = Assert.Single(type.InstanceConstructors.Where(c => c.MethodKind == MethodKind.Constructor));

        Assert.Equal(Accessibility.Public, constructor.DeclaredAccessibility);
        Assert.Empty(compilation.GetDiagnostics());
    }

    [Fact]
    public void PrimaryConstructor_WithPromotedParameters_SynthesizesDeconstruct()
    {
        var source = """
            class Person(val Id: int, val Name: string) {
            }
            """;

        var (compilation, tree) = CreateCompilation(source);
        var model = compilation.GetSemanticModel(tree);

        var typeDeclaration = tree.GetRoot().DescendantNodes().OfType<ClassDeclarationSyntax>().Single();
        var type = Assert.IsAssignableFrom<INamedTypeSymbol>(model.GetDeclaredSymbol(typeDeclaration));
        var deconstruct = Assert.Single(
            type.GetMembers("Deconstruct").OfType<IMethodSymbol>()
                .Where(method => method.Parameters.Length == 2));

        Assert.Equal(SpecialType.System_Unit, deconstruct.ReturnType.SpecialType);
        Assert.All(deconstruct.Parameters, parameter => Assert.Equal(RefKind.Out, parameter.RefKind));
        Assert.Empty(compilation.GetDiagnostics());
    }

    [Fact]
    public void NominalDeconstructionPattern_BindsAgainstPrimaryConstructorClassDeconstruct()
    {
        var source = """
            val value: object = Person(1, "Ada");

            val result = value match {
                Person(1, val name) => name
                _ => ""
            };

            class Person(val Id: int, val Name: string);
            """;

        var (compilation, tree) = CreateCompilation(source);
        var model = compilation.GetSemanticModel(tree);

        var recordPattern = tree.GetRoot().DescendantNodes().OfType<NominalDeconstructionPatternSyntax>().Single();
        var boundPattern = Assert.IsType<BoundDeconstructPattern>(model.GetBoundNode(recordPattern));

        Assert.Equal(2, boundPattern.Arguments.Length);
        Assert.Equal("Deconstruct", boundPattern.DeconstructMethod.Name);
        Assert.Empty(compilation.GetDiagnostics());
    }
}
