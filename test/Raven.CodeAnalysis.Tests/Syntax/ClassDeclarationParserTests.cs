using System.Linq;

using Raven.CodeAnalysis;
using Raven.CodeAnalysis.Syntax;
using Raven.CodeAnalysis.Testing;

namespace Raven.CodeAnalysis.Syntax.Parser.Tests;

public class ClassDeclarationParserTests : DiagnosticTestBase
{
    [Fact]
    public void ClassDeclaration_WithPrimaryConstructor_ParsesParameterList()
    {
        var source = "class Person(name: string, age: int) {}";
        var tree = SyntaxTree.ParseText(source);
        var root = tree.GetRoot();

        var declaration = Assert.IsType<ClassDeclarationSyntax>(Assert.Single(root.Members));

        Assert.NotNull(declaration.ParameterList);
        Assert.Equal(2, declaration.ParameterList!.Parameters.Count);
        Assert.Empty(tree.GetDiagnostics());
    }

    [Fact]
    public void ClassDeclaration_WithPrimaryConstructorParameterAccessibility_ParsesAccessibilityKeyword()
    {
        var source = "class Person(private var Name: string) {}";
        var tree = SyntaxTree.ParseText(source);
        var root = tree.GetRoot();

        var declaration = Assert.IsType<ClassDeclarationSyntax>(Assert.Single(root.Members));
        var parameter = Assert.Single(declaration.ParameterList!.Parameters);

        Assert.Equal(SyntaxKind.PrivateKeyword, parameter.AccessibilityKeyword.Kind);
        Assert.Equal(SyntaxKind.VarKeyword, parameter.BindingKeyword.Kind);
        Assert.Empty(tree.GetDiagnostics());
    }

    [Fact]
    public void ClassDeclaration_WithTypeParameters_ParsesTypeParameterList()
    {
        var source = "class Box<TValue, TOther> {}";
        var tree = SyntaxTree.ParseText(source);
        var root = tree.GetRoot();

        var declaration = Assert.IsType<ClassDeclarationSyntax>(Assert.Single(root.Members));

        Assert.NotNull(declaration.TypeParameterList);
        Assert.Equal(2, declaration.TypeParameterList!.Parameters.Count);
        Assert.Empty(tree.GetDiagnostics());
    }

    [Fact]
    public void InterfaceDeclaration_WithVariantTypeParameters_ParsesVarianceModifiers()
    {
        var source = "interface Producer<out T, in U> {}";
        var tree = SyntaxTree.ParseText(source);
        var root = tree.GetRoot();

        var declaration = Assert.IsType<InterfaceDeclarationSyntax>(Assert.Single(root.Members));
        var parameters = declaration.TypeParameterList!.Parameters;

        Assert.Equal(SyntaxKind.OutKeyword, parameters[0].VarianceKeyword.Kind);
        Assert.Equal(SyntaxKind.InKeyword, parameters[1].VarianceKeyword.Kind);
        Assert.Empty(tree.GetDiagnostics());
    }

    [Fact]
    public void Constructor_WithExpressionBody_ParsesExpressionBody()
    {
        var source = """
            class Person {
                public init()
                    => Console.WriteLine("Hello")
            }
            """;

        var tree = SyntaxTree.ParseText(source);
        var ctor = tree.GetRoot().DescendantNodes().OfType<ConstructorDeclarationSyntax>().Single();

        Assert.Null(ctor.Body);
        Assert.NotNull(ctor.ExpressionBody);
        Assert.True(ctor.ExpressionBody!.ArrowToken.IsKind(SyntaxKind.FatArrowToken));
        Assert.Empty(tree.GetDiagnostics());
    }

    [Fact]
    public void Constructor_WithInitializerAndExpressionBody_ParsesBoth()
    {
        var source = """
            class Derived : Base {
                public init(): base(1)
                    => Foo()
            }
            """;

        var tree = SyntaxTree.ParseText(source);
        var ctor = tree.GetRoot().DescendantNodes().OfType<ConstructorDeclarationSyntax>().Single();

        Assert.NotNull(ctor.Initializer);
        Assert.NotNull(ctor.ExpressionBody);
        Assert.True(ctor.ExpressionBody!.ArrowToken.IsKind(SyntaxKind.FatArrowToken));
        Assert.Empty(tree.GetDiagnostics());
    }

    [Fact]
    public void NamedConstructorSyntax_ProducesDiagnostics()
    {
        var source = """
            class Person {
                public init WithName(name: string) => Person()
            }
            """;

        var tree = SyntaxTree.ParseText(source);
        Assert.NotEmpty(tree.GetDiagnostics());
    }

    [Fact]
    public void Constructor_WithExpressionBody_FollowedByMethod_ParsesBothMembers()
    {
        var source = """
            class Foo {
                public init() => Console.WriteLine("Init")
                public func Dispose() -> unit {}
            }
            """;

        var tree = SyntaxTree.ParseText(source);
        var @class = tree.GetRoot().DescendantNodes().OfType<ClassDeclarationSyntax>().Single();

        var members = @class.Members;

        Assert.Collection(
            members,
            member => Assert.IsType<ConstructorDeclarationSyntax>(member),
            member => Assert.IsType<MethodDeclarationSyntax>(member));

        Assert.Empty(tree.GetDiagnostics());
    }

    [Fact]
    public void ClassDeclaration_WithAttributeList_ParsesAttributes()
    {
        var source = "[Obsolete] class Widget {}";
        var tree = SyntaxTree.ParseText(source);
        var root = tree.GetRoot();

        var declaration = Assert.IsType<ClassDeclarationSyntax>(Assert.Single(root.Members));

        var attributeList = Assert.Single(declaration.AttributeLists);
        var attribute = Assert.Single(attributeList.Attributes);

        var name = Assert.IsType<IdentifierNameSyntax>(attribute.Name);
        Assert.Equal("Obsolete", name.Identifier.Text);
        Assert.Empty(tree.GetDiagnostics());
    }

    [Fact]
    public void MethodDeclaration_WithAttributeList_ParsesAttributes()
    {
        var source = """
            class Widget {
                [Inline]
                public func Do() -> unit {}
            }
            """;

        var tree = SyntaxTree.ParseText(source);
        var method = tree.GetRoot().DescendantNodes().OfType<MethodDeclarationSyntax>().Single();

        var attributeList = Assert.Single(method.AttributeLists);
        var attribute = Assert.Single(attributeList.Attributes);

        var name = Assert.IsType<IdentifierNameSyntax>(attribute.Name);
        Assert.Equal("Inline", name.Identifier.Text);
        Assert.Empty(tree.GetDiagnostics());
    }

    [Fact]
    public void SelfMember_WithGenericReturnType_ParsesWithoutSyntaxErrors()
    {
        var source = """
            class Foo {
                public func self(flag: bool) -> Task<Option<int>> {
                    return Test(flag)
                }
            }
            """;

        var tree = SyntaxTree.ParseText(source);
        var root = tree.GetRoot();
        var declaration = Assert.Single(root.Members.OfType<ClassDeclarationSyntax>());

        Assert.NotNull(declaration);
        Assert.Empty(tree.GetDiagnostics());
    }

    [Fact]
    public void MethodDeclaration_WithArrowAndMissingReturnType_ReportsIdentifierExpected()
    {
        var source = """
            class Foo {
                public func M() -> {}
            }
            """;

        var tree = SyntaxTree.ParseText(source);
        var method = tree.GetRoot().DescendantNodes().OfType<MethodDeclarationSyntax>().Single();

        Assert.NotNull(method.ReturnType);
        var typeSyntax = Assert.IsType<IdentifierNameSyntax>(method.ReturnType!.Type);
        Assert.True(typeSyntax.Identifier.IsMissing);

        var diagnostic = Assert.Single(tree.GetDiagnostics());
        Assert.Equal(CompilerDiagnostics.IdentifierExpected.Id, diagnostic.Descriptor.Id);
    }

    [Fact]
    public void MethodDeclaration_WithFuncKeyword_ParsesWithoutSyntaxErrors()
    {
        var source = """
            class Calculator {
                public func Add(x: int, y: int) -> int => x + y
            }
            """;

        var tree = SyntaxTree.ParseText(source);
        var method = tree.GetRoot().DescendantNodes().OfType<MethodDeclarationSyntax>().Single();

        Assert.Equal(SyntaxKind.FuncKeyword, method.FuncKeyword.Kind);
        Assert.Equal("Add", method.Identifier.ValueText);
        Assert.Empty(tree.GetDiagnostics());
    }

    [Fact]
    public void ExtensionMethod_WithFuncKeyword_ParsesWithoutSyntaxErrors()
    {
        var source = """
            extension NumberOps for int {
                public func AddOne() -> int => self + 1
            }
            """;

        var tree = SyntaxTree.ParseText(source);
        var method = tree.GetRoot().DescendantNodes().OfType<MethodDeclarationSyntax>().Single();

        Assert.Equal(SyntaxKind.FuncKeyword, method.FuncKeyword.Kind);
        Assert.Equal("AddOne", method.Identifier.ValueText);
        Assert.Empty(tree.GetDiagnostics());
    }

    [Fact]
    public void SelfInvocation_WithFuncKeyword_ParsesWithoutSyntaxErrors()
    {
        var source = """
            class Foo {
                public func self(flag: bool) -> Task<Option<int>> {
                    return Test(flag)
                }
            }
            """;

        var tree = SyntaxTree.ParseText(source);
        var method = tree.GetRoot().DescendantNodes().OfType<MethodDeclarationSyntax>().Single();

        Assert.Equal(SyntaxKind.FuncKeyword, method.FuncKeyword.Kind);
        Assert.Equal(SyntaxKind.SelfKeyword, method.Identifier.Kind);
        Assert.Empty(tree.GetDiagnostics());
    }

    [Fact]
    public void SelfIndexer_WithVarKeyword_ParsesWithoutSyntaxErrors()
    {
        var source = """
            class Foo {
                public var self[index: int]: string {
                    get => "x"
                }
            }
            """;

        var tree = SyntaxTree.ParseText(source);
        var indexer = tree.GetRoot().DescendantNodes().OfType<IndexerDeclarationSyntax>().Single();

        Assert.Equal(SyntaxKind.SelfKeyword, indexer.Identifier.Kind);
        Assert.Equal(SyntaxKind.VarKeyword, indexer.BindingKeyword.Kind);
        Assert.Empty(tree.GetDiagnostics());
    }

    [Fact]
    public void MethodDeclaration_WithoutFuncKeyword_ParsesAsIncompleteMember()
    {
        var source = """
            class Calculator {
                Add(x: int, y: int) -> int => x + y
            }
            """;

        var tree = SyntaxTree.ParseText(source);
        var @class = Assert.Single(tree.GetRoot().DescendantNodes().OfType<ClassDeclarationSyntax>());
        Assert.DoesNotContain(@class.Members, m => m is MethodDeclarationSyntax);
        Assert.Contains(@class.Members, m => m is IncompleteMemberDeclarationSyntax);
    }

    [Fact]
    public void PropertyDeclaration_WithVarKeyword_ParsesAsStoredProperty()
    {
        var source = """
            class Counter {
                var Count: int = 0
            }
            """;

        var tree = SyntaxTree.ParseText(source);
        var @class = tree.GetRoot().DescendantNodes().OfType<ClassDeclarationSyntax>().Single();

        var property = Assert.IsType<PropertyDeclarationSyntax>(Assert.Single(@class.Members));
        Assert.True(property.BindingKeyword.IsKind(SyntaxKind.VarKeyword));
        Assert.NotNull(property.Initializer);
        Assert.NotNull(property.AccessorList);
        Assert.Equal(2, property.AccessorList!.Accessors.Count);
        Assert.Empty(tree.GetDiagnostics());
    }

    [Fact]
    public void PropertyDeclaration_WithValKeywordAndExpressionBody_ParsesComputedProperty()
    {
        var source = """
            class Customer(name: string) {
                val Name: string => name
            }
            """;

        var tree = SyntaxTree.ParseText(source);
        var @class = tree.GetRoot().DescendantNodes().OfType<ClassDeclarationSyntax>().Single();

        var property = Assert.IsType<PropertyDeclarationSyntax>(Assert.Single(@class.Members));
        Assert.True(property.BindingKeyword.IsKind(SyntaxKind.ValKeyword));
        Assert.NotNull(property.ExpressionBody);
        Assert.Null(property.Initializer);
        Assert.Empty(tree.GetDiagnostics());
    }

    [Fact]
    public void FieldDeclaration_WithFieldKeywordAndReadonlyModifier_ParsesField()
    {
        var source = """
            class Customer(name: string) {
                private readonly field _name: string = name
            }
            """;

        var tree = SyntaxTree.ParseText(source);
        var @class = tree.GetRoot().DescendantNodes().OfType<ClassDeclarationSyntax>().Single();

        var field = Assert.IsType<FieldDeclarationSyntax>(Assert.Single(@class.Members));
        Assert.Contains(field.Modifiers, token => token.IsKind(SyntaxKind.ReadonlyKeyword));
        Assert.Equal(SyntaxKind.FieldKeyword, field.FieldKeyword.Kind);
        Assert.Empty(tree.GetDiagnostics());
    }

    [Fact]
    public void FieldDeclaration_WithConstKeyword_ParsesAsConstFieldDeclaration()
    {
        var source = """
            class Constants {
                const Pi: double = 3.14
            }
            """;

        var tree = SyntaxTree.ParseText(source);
        var @class = tree.GetRoot().DescendantNodes().OfType<ClassDeclarationSyntax>().Single();

        var field = Assert.IsType<ConstDeclarationSyntax>(Assert.Single(@class.Members));
        Assert.Equal(SyntaxKind.ConstKeyword, field.ConstKeyword.Kind);
        Assert.Empty(tree.GetDiagnostics());
    }

    [Fact]
    public void InitDeclaration_WithoutParameterList_ParsesAsParameterlessConstructorDeclaration()
    {
        var source = """
            class C {
                init {
                }
            }
            """;

        var tree = SyntaxTree.ParseText(source);
        var @class = tree.GetRoot().DescendantNodes().OfType<ClassDeclarationSyntax>().Single();

        var ctor = Assert.IsType<ParameterlessConstructorDeclarationSyntax>(Assert.Single(@class.Members));
        Assert.Equal(SyntaxKind.InitKeyword, ctor.InitKeyword.Kind);
        Assert.NotNull(ctor.Body);
        Assert.Empty(tree.GetDiagnostics());
    }

    [Fact]
    public void PrimaryInitializerBlock_ParsesAsDedicatedSyntaxNode()
    {
        var source = """
            class C(name: string) {
                {
                    val x = name
                }
            }
            """;

        var tree = SyntaxTree.ParseText(source);
        var @class = tree.GetRoot().DescendantNodes().OfType<ClassDeclarationSyntax>().Single();

        var initBlock = Assert.IsType<InitializerBlockDeclarationSyntax>(Assert.Single(@class.Members));
        Assert.NotNull(initBlock.Body);
        Assert.Empty(tree.GetDiagnostics());
    }

    [Fact]
    public void FinallyDeclaration_ParsesAsDedicatedSyntaxNode()
    {
        var source = """
            class C {
                finally {
                }
            }
            """;

        var tree = SyntaxTree.ParseText(source);
        var @class = tree.GetRoot().DescendantNodes().OfType<ClassDeclarationSyntax>().Single();

        var finalDecl = Assert.IsType<FinallyDeclarationSyntax>(Assert.Single(@class.Members));
        Assert.Equal(SyntaxKind.FinallyKeyword, finalDecl.FinallyKeyword.Kind);
        Assert.NotNull(finalDecl.Body);
        Assert.Empty(tree.GetDiagnostics());
    }

}
