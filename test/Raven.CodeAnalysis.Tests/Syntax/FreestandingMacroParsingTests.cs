using System.Linq;

using Raven.CodeAnalysis.Syntax;

using Xunit;

namespace Raven.CodeAnalysis.Syntax.Tests;

public sealed class FreestandingMacroParsingTests
{
    [Fact]
    public void FreestandingMacroExpression_ParsesBangIdentifierAndArguments()
    {
        var tree = SyntaxTree.ParseText("""
            func Main() -> int => add!(1, right: 2)
            """);

        var expression = tree.GetRoot()
            .DescendantNodes()
            .OfType<FreestandingMacroExpressionSyntax>()
            .Single();

        Assert.True(expression.TryGetMacroName(out var macroName));
        Assert.Equal("add", macroName);
        Assert.Equal(SyntaxKind.ExclamationToken, expression.ExclamationToken.Kind);
        Assert.Equal(2, expression.ArgumentList.Arguments.Count);
        Assert.Equal("right", expression.ArgumentList.Arguments[1].NameColon?.Name.Identifier.ValueText);
        Assert.Empty(tree.GetDiagnostics());
    }

    [Fact]
    public void FreestandingMacroInvocation_AtCompilationUnitScopeKeepsStatementEnvelope()
    {
        var tree = SyntaxTree.ParseText("""
            GenerateModels! {
                model User
            }

            class Existing {}
            """);

        var root = tree.GetRoot();
        var globalStatement = Assert.IsType<GlobalStatementSyntax>(root.Members[0]);
        var statement = Assert.IsType<ExpressionStatementSyntax>(globalStatement.Statement);
        var invocation = Assert.IsType<FreestandingMacroExpressionSyntax>(statement.Expression);

        Assert.Equal("GenerateModels", invocation.Name.ToString());
        Assert.Contains("model User", Assert.IsType<MacroTokenTreeSyntax>(invocation.TokenTree).BodyToken.Text);
        Assert.IsType<ClassDeclarationSyntax>(root.Members[1]);
        Assert.Empty(tree.GetDiagnostics());
    }

    [Fact]
    public void FreestandingMacroInvocation_InNamespaceKeepsStatementEnvelope()
    {
        var tree = SyntaxTree.ParseText("""
            namespace Models {
                Tools.Generate!(2, Visibility: "public") {
                    model User
                }
            }
            """);

        var globalStatement = tree.GetRoot()
            .DescendantNodes()
            .OfType<GlobalStatementSyntax>()
            .Single();
        var statement = Assert.IsType<ExpressionStatementSyntax>(globalStatement.Statement);
        var invocation = Assert.IsType<FreestandingMacroExpressionSyntax>(statement.Expression);

        Assert.Equal("Tools.Generate", invocation.Name.ToString());
        Assert.Equal(2, invocation.ArgumentList.Arguments.Count);
        Assert.Equal("Visibility", invocation.ArgumentList.Arguments[1].NameColon?.Name.Identifier.ValueText);
        Assert.NotNull(invocation.TokenTree);
        Assert.Empty(tree.GetDiagnostics());
    }

    [Fact]
    public void FreestandingMacroMember_ParsesInTypeBody()
    {
        var tree = SyntaxTree.ParseText("""
            class Model {
                GenerateProperties! { Id, Name }
            }
            """);

        var type = Assert.IsType<ClassDeclarationSyntax>(tree.GetRoot().Members.Single());
        var invocation = Assert.IsType<FreestandingMacroMemberDeclarationSyntax>(type.Members.Single());

        Assert.Equal("GenerateProperties", invocation.Name.ToString());
        Assert.Contains("Id, Name", Assert.IsType<MacroTokenTreeSyntax>(invocation.TokenTree).BodyToken.Text);
        Assert.Empty(tree.GetDiagnostics());
    }

    [Fact]
    public void FreestandingMacroDeclaration_ParsesFunctionShapedCarrier()
    {
        var tree = SyntaxTree.ParseText("""
            component! Foo(x: int) {
                render x
            }
            """);

        var declaration = Assert.IsType<FreestandingMacroDeclarationSyntax>(
            tree.GetRoot().Members.Single());

        Assert.Equal("component", declaration.Name.ToString());
        Assert.Equal("Foo", declaration.Identifier.ValueText);
        var parameter = Assert.Single(Assert.IsType<ParameterListSyntax>(declaration.ParameterList).Parameters);
        Assert.Equal("x", parameter.Identifier.ValueText);
        Assert.Equal("int", parameter.TypeAnnotation?.Type.ToString());
        Assert.Contains("render x", declaration.TokenTree.BodyToken.Text);
        Assert.Empty(tree.GetDiagnostics());
    }

    [Fact]
    public void FreestandingMacroDeclaration_DistinguishesMacroTypeArgumentsFromDeclaredTypeParameters()
    {
        var tree = SyntaxTree.ParseText("""
            component<Blazor>! Counter<T>(initial: T)
                : ComponentBase, IRenderable<T>
                where T: Entity
                permits SpecializedCounter
            {
                render initial
            }
            """);

        var declaration = Assert.IsType<FreestandingMacroDeclarationSyntax>(
            tree.GetRoot().Members.Single());
        var macroName = Assert.IsType<GenericNameSyntax>(declaration.Name);
        var declaredTypeParameters = Assert.IsType<TypeParameterListSyntax>(
            declaration.TypeParameterList);
        var baseList = Assert.IsType<BaseListSyntax>(declaration.BaseList);

        Assert.Equal("Blazor", Assert.Single(macroName.TypeArgumentList.Arguments).ToString());
        Assert.Equal("T", Assert.Single(declaredTypeParameters.Parameters).Identifier.ValueText);
        Assert.Equal(2, baseList.Types.Count);
        Assert.IsType<MacroBaseListSuffixSyntax>(declaration.Suffix);
        Assert.Equal("T", Assert.Single(declaration.ConstraintClauses).TypeParameter.ToString());
        Assert.Equal("SpecializedCounter", Assert.Single(declaration.PermitsClause!.Types).ToString());
        Assert.Contains("render initial", declaration.TokenTree!.BodyToken.Text);
        Assert.Empty(tree.GetDiagnostics());
    }

    [Fact]
    public void FreestandingMacroDeclaration_ParsesCallableReturnAndConstraints()
    {
        var tree = SyntaxTree.ParseText("""
            consumer! Handle<T>(message: T) -> Task<Result>
                where T: Message
            {
                consume message
            }
            """);

        var declaration = Assert.IsType<FreestandingMacroDeclarationSyntax>(
            tree.GetRoot().Members.Single());

        Assert.Equal("Task<Result>", declaration.ReturnType!.Type.ToString());
        Assert.IsType<MacroReturnTypeSuffixSyntax>(declaration.Suffix);
        Assert.Null(declaration.BaseList);
        Assert.Null(declaration.PermitsClause);
        Assert.Single(declaration.ConstraintClauses);
        Assert.Empty(tree.GetDiagnostics());
    }

    [Fact]
    public void FreestandingMacroDeclaration_AllowsMarkerHeaderWithoutBody()
    {
        var tree = SyntaxTree.ParseText("""
            marker! GeneratedMember

            class Existing {}
            """);
        var declaration = Assert.IsType<FreestandingMacroDeclarationSyntax>(
            tree.GetRoot().Members[0]);

        Assert.Equal("GeneratedMember", declaration.Identifier.ValueText);
        Assert.Null(declaration.TokenTree);
        Assert.IsType<ClassDeclarationSyntax>(tree.GetRoot().Members[1]);
        Assert.Empty(tree.GetDiagnostics());
    }

    [Fact]
    public void FreestandingMacroDeclaration_PreservesIncompleteDeclaredTypeParameterList()
    {
        var tree = SyntaxTree.ParseText("component! Counter<T");
        var declaration = Assert.IsType<FreestandingMacroDeclarationSyntax>(
            tree.GetRoot().Members.Single());
        var typeParameters = Assert.IsType<TypeParameterListSyntax>(
            declaration.TypeParameterList);

        Assert.Equal("T", Assert.Single(typeParameters.Parameters).Identifier.ValueText);
        Assert.True(typeParameters.GreaterThanToken.IsMissing);
        Assert.Null(declaration.TokenTree);
        Assert.NotEmpty(tree.GetDiagnostics());
    }

    [Fact]
    public void IdentifierExpressionHeaderAtCompilationUnitScope_RemainsGlobalStatementWhenTailIsExpressionSyntax()
    {
        var tree = SyntaxTree.ParseText("process! operation + 1");
        var globalStatement = Assert.IsType<GlobalStatementSyntax>(tree.GetRoot().Members.Single());
        var statement = Assert.IsType<ExpressionStatementSyntax>(globalStatement.Statement);
        var invocation = Assert.IsType<FreestandingMacroExpressionSyntax>(statement.Expression);

        Assert.IsType<ExpressionHeaderMacroCarrierSyntax>(invocation.Carrier);
        Assert.Empty(tree.GetDiagnostics());
    }

    [Fact]
    public void FreestandingMacroDeclaration_PreservesAttributesAndModifiersInTypeBody()
    {
        var tree = SyntaxTree.ParseText("""
            class Container {
                [Example]
                public component! Header(title: string) {
                    h1 title
                }
            }
            """);

        var type = Assert.IsType<ClassDeclarationSyntax>(tree.GetRoot().Members.Single());
        var declaration = Assert.IsType<FreestandingMacroDeclarationSyntax>(type.Members.Single());

        Assert.Single(declaration.AttributeLists);
        Assert.Contains(declaration.Modifiers, static modifier => modifier.IsKind(SyntaxKind.PublicKeyword));
        Assert.Equal("Header", declaration.Identifier.ValueText);
        Assert.Contains("h1 title", declaration.TokenTree.BodyToken.Text);
        Assert.Empty(tree.GetDiagnostics());
    }

    [Fact]
    public void FreestandingMacroDeclaration_ParsesInNamespace()
    {
        var tree = SyntaxTree.ParseText("""
            namespace Components {
                component! Header() {
                }
            }
            """);

        var declaration = tree.GetRoot()
            .DescendantNodes()
            .OfType<FreestandingMacroDeclarationSyntax>()
            .Single();

        Assert.Equal("component", declaration.Name.ToString());
        Assert.Equal("Header", declaration.Identifier.ValueText);
        Assert.Empty(tree.GetDiagnostics());
    }

    [Fact]
    public void FreestandingMacroDeclaration_ParsesAsMemberInFileScopedNamespace()
    {
        var tree = SyntaxTree.ParseText("""
            namespace Components;

            component! Header() {
            }
            """);
        var root = Assert.IsType<CompilationUnitSyntax>(tree.GetRoot());
        var fileScopedNamespace = Assert.IsType<FileScopedNamespaceDeclarationSyntax>(
            Assert.Single(root.Members));
        var declaration = Assert.IsType<FreestandingMacroDeclarationSyntax>(
            Assert.Single(fileScopedNamespace.Members));

        Assert.Equal("component", declaration.Name.ToString());
        Assert.Equal("Header", declaration.Identifier.ValueText);
        Assert.Empty(declaration.Ancestors().OfType<GlobalStatementSyntax>());
        Assert.Empty(tree.GetDiagnostics());
    }

    [Theory]
    [InlineData(false)]
    [InlineData(true)]
    public void FreestandingMacroDeclaration_RemainsMemberAmongTopLevelStatements(
        bool useFileScopedNamespace)
    {
        var source = (useFileScopedNamespace ? "namespace Components;\n\n" : string.Empty) + """
            let before = 1

            component! Header() {
            }

            let after = 2
            """;
        var tree = SyntaxTree.ParseText(source);
        var declaration = Assert.Single(
            tree.GetRoot().DescendantNodes().OfType<FreestandingMacroDeclarationSyntax>());

        if (useFileScopedNamespace)
            Assert.IsType<FileScopedNamespaceDeclarationSyntax>(declaration.Parent);
        else
            Assert.IsType<CompilationUnitSyntax>(declaration.Parent);

        Assert.Empty(declaration.Ancestors().OfType<GlobalStatementSyntax>());
        Assert.Equal(2, tree.GetRoot().DescendantNodes().OfType<GlobalStatementSyntax>().Count());
        Assert.Empty(tree.GetDiagnostics());
    }

    [Fact]
    public void FreestandingMacroDeclaration_UnterminatedBodyReportsMissingBrace()
    {
        var tree = SyntaxTree.ParseText("""
            component! Header(title: string) {
                h1 title
            """);

        var declaration = tree.GetRoot()
            .DescendantNodes()
            .OfType<FreestandingMacroDeclarationSyntax>()
            .Single();

        Assert.True(declaration.TokenTree.CloseBraceToken.IsMissing);
        Assert.Contains(tree.GetDiagnostics(), static diagnostic => diagnostic.Id == "RAV1003");
    }

    [Fact]
    public void FreestandingMacroMember_UnterminatedBodyReportsMissingBrace()
    {
        var tree = SyntaxTree.ParseText("""
            class Model {
                GenerateProperties! {
                    Id, Name
            """);

        var invocation = tree.GetRoot()
            .DescendantNodes()
            .OfType<FreestandingMacroMemberDeclarationSyntax>()
            .Single();

        Assert.True(Assert.IsType<MacroTokenTreeSyntax>(invocation.TokenTree).CloseBraceToken.IsMissing);
        Assert.Contains(tree.GetDiagnostics(), static diagnostic => diagnostic.Id == "RAV1003");
    }

    [Fact]
    public void BareBangExpression_IsNotReclassifiedAsMacroMember()
    {
        var tree = SyntaxTree.ParseText("value!");

        Assert.Empty(tree.GetRoot().DescendantNodes().OfType<FreestandingMacroMemberDeclarationSyntax>());
        Assert.Contains(tree.GetRoot().DescendantNodes(), static node =>
            node is PostfixOperatorExpressionSyntax { Kind: SyntaxKind.SuppressNullableWarningExpression });
    }

    [Fact]
    public void HashDirective_IsNotParsedAsFreestandingMacroExpression()
    {
        var tree = SyntaxTree.ParseText("""
            #pragma warning disable RAV0001
            func Main() -> int => 1
            """);

        Assert.Empty(tree.GetRoot().DescendantNodes().OfType<FreestandingMacroExpressionSyntax>());
    }

    [Fact]
    public void HashInvocation_IsNotParsedAsFreestandingMacroExpression()
    {
        var tree = SyntaxTree.ParseText("func Main() -> int => #answer()");

        Assert.Empty(tree.GetRoot().DescendantNodes().OfType<FreestandingMacroExpressionSyntax>());
        Assert.NotEmpty(tree.GetDiagnostics());
    }

    [Fact]
    public void TokenTreeMacroExpression_PreservesRawDslBody()
    {
        var tree = SyntaxTree.ParseText("""
            func Main() -> int => xml!{
                <root data="{not a Raven expression}">
                    {{ nested { content } }}
                </root>
            }
            """);

        var expression = tree.GetRoot()
            .DescendantNodes()
            .OfType<FreestandingMacroExpressionSyntax>()
            .Single();

        var tokenTree = Assert.IsType<MacroTokenTreeSyntax>(expression.TokenTree);
        Assert.Contains("<root data=\"{not a Raven expression}\">", tokenTree.ToFullString());
        Assert.Contains("{{ nested { content } }}", tokenTree.BodyToken.Text);
        Assert.Equal(SyntaxKind.MacroBodyToken, tokenTree.BodyToken.Kind);
        Assert.False(tokenTree.CloseBraceToken.IsMissing);
        Assert.Empty(tree.GetDiagnostics());
    }

    [Fact]
    public void TokenTreeMacroExpression_ParsesArgumentsAndRawBody()
    {
        var tree = SyntaxTree.ParseText("""
            func Main() -> int => repeat!(3, Label: "item") {
                custom content
            }
            """);

        var expression = tree.GetRoot()
            .DescendantNodes()
            .OfType<FreestandingMacroExpressionSyntax>()
            .Single();

        Assert.Equal(2, expression.ArgumentList.Arguments.Count);
        Assert.Equal("Label", expression.ArgumentList.Arguments[1].NameColon?.Name.Identifier.ValueText);
        Assert.Contains("custom content", Assert.IsType<MacroTokenTreeSyntax>(expression.TokenTree).BodyToken.Text);
        Assert.Empty(tree.GetDiagnostics());
    }

    [Fact]
    public void TokenTreeMacroExpression_AllowsCharactersOutsideRavenLexicalGrammar()
    {
        var tree = SyntaxTree.ParseText("""
            func Main() -> int => dsl!{
                `custom-key` ::= ⟨value⟩
            }
            """);

        var tokenTree = tree.GetRoot()
            .DescendantNodes()
            .OfType<MacroTokenTreeSyntax>()
            .Single();

        Assert.Contains("`custom-key` ::= ⟨value⟩", tokenTree.ToFullString());
        Assert.Empty(tree.GetDiagnostics());
    }

    [Fact]
    public void TokenTreeMacroExpression_UnterminatedBodyReportsMissingBrace()
    {
        var tree = SyntaxTree.ParseText("""
            func Main() -> int => dsl!{
                custom content
            """);

        var tokenTree = tree.GetRoot()
            .DescendantNodes()
            .OfType<MacroTokenTreeSyntax>()
            .Single();

        Assert.True(tokenTree.CloseBraceToken.IsMissing);
        Assert.Contains(tree.GetDiagnostics(), static diagnostic => diagnostic.Id == "RAV1003");
    }

    [Fact]
    public void FreestandingMacroExpression_ParsesDedicatedNodeAndPreservesRawBody()
    {
        var tree = SyntaxTree.ParseText("""
            func Main() -> int => quote! {
                left + right
            }
            """);

        var expression = tree.GetRoot()
            .DescendantNodes()
            .OfType<FreestandingMacroExpressionSyntax>()
            .Single();

        Assert.True(expression.TryGetMacroName(out var macroName));
        Assert.Equal("quote", macroName);
        Assert.Equal(SyntaxKind.ExclamationToken, expression.ExclamationToken.Kind);
        Assert.IsType<TokenTreeMacroCarrierSyntax>(expression.Carrier);
        Assert.Null(expression.ArgumentList);
        Assert.Contains("left + right", Assert.IsType<MacroTokenTreeSyntax>(expression.TokenTree).BodyToken.Text);
        Assert.Empty(tree.GetDiagnostics());
    }

    [Fact]
    public void FreestandingMacroExpression_ParsesExpressionHeaderWithoutBody()
    {
        var tree = SyntaxTree.ParseText("""
            func Process(operation: int) -> int => process! operation + 1
            """);

        var invocation = tree.GetRoot()
            .DescendantNodes()
            .OfType<FreestandingMacroExpressionSyntax>()
            .Single();
        var carrier = Assert.IsType<ExpressionHeaderMacroCarrierSyntax>(invocation.Carrier);

        Assert.IsType<InfixOperatorExpressionSyntax>(carrier.Expression);
        Assert.Null(carrier.TokenTree);
        Assert.Null(invocation.ArgumentList);
        Assert.Empty(tree.GetDiagnostics());
    }

    [Fact]
    public void FreestandingMacroExpression_ParsesExpressionHeaderAndRawBody()
    {
        var tree = SyntaxTree.ParseText("""
            func Describe(value: int) -> string {
                return match! value {
                    0 => "zero"
                    _ => "other"
                }
            }
            """);

        var invocation = tree.GetRoot()
            .DescendantNodes()
            .OfType<FreestandingMacroExpressionSyntax>()
            .Single();
        var carrier = Assert.IsType<ExpressionHeaderMacroCarrierSyntax>(invocation.Carrier);

        Assert.Equal("value", carrier.Expression.ToString());
        Assert.Contains("0 => \"zero\"", Assert.IsType<MacroTokenTreeSyntax>(carrier.TokenTree).BodyToken.Text);
        Assert.Empty(tree.GetDiagnostics());
    }

    [Fact]
    public void FreestandingMacroExpression_ExpressionHeaderCanOccupyStatementPosition()
    {
        var tree = SyntaxTree.ParseText("""
            func Check(value: int) {
                guard! value > 0 {
                    report failure
                }
            }
            """);

        var statement = tree.GetRoot()
            .DescendantNodes()
            .OfType<ExpressionStatementSyntax>()
            .Single();
        var invocation = Assert.IsType<FreestandingMacroExpressionSyntax>(statement.Expression);
        var carrier = Assert.IsType<ExpressionHeaderMacroCarrierSyntax>(invocation.Carrier);

        Assert.IsType<InfixOperatorExpressionSyntax>(carrier.Expression);
        Assert.NotNull(carrier.TokenTree);
        Assert.Empty(tree.GetDiagnostics());
    }

    [Fact]
    public void FreestandingMacroExpression_ParsesGenericNameArgumentsAndRawBody()
    {
        var tree = SyntaxTree.ParseText("""
            func Main() -> int => repeat<int>!(3, Label: "item") {
                custom content
            }
            """);

        var expression = tree.GetRoot()
            .DescendantNodes()
            .OfType<FreestandingMacroExpressionSyntax>()
            .Single();

        Assert.IsType<GenericNameSyntax>(expression.Name);
        Assert.Equal(2, expression.ArgumentList.Arguments.Count);
        Assert.Equal("Label", expression.ArgumentList.Arguments[1].NameColon?.Name.Identifier.ValueText);
        Assert.Contains("custom content", Assert.IsType<MacroTokenTreeSyntax>(expression.TokenTree).BodyToken.Text);
        Assert.Empty(tree.GetDiagnostics());
    }

    [Fact]
    public void FreestandingMacroExpression_ParsesArgumentStyleInvocationWithoutBody()
    {
        var tree = SyntaxTree.ParseText("""
            func Main() -> int => twice!(21)
            """);

        var expression = tree.GetRoot()
            .DescendantNodes()
            .OfType<FreestandingMacroExpressionSyntax>()
            .Single();

        Assert.Equal("twice", expression.Name.ToString());
        Assert.Single(expression.ArgumentList.Arguments);
        Assert.Null(expression.TokenTree);
        Assert.Empty(tree.GetDiagnostics());
    }

    [Fact]
    public void FreestandingMacroExpression_ParsesContextualKeywordName()
    {
        var tree = SyntaxTree.ParseText("""
            func Main() -> int => add!(20, Right: 22)
            """);

        var expression = tree.GetRoot()
            .DescendantNodes()
            .OfType<FreestandingMacroExpressionSyntax>()
            .Single();

        Assert.Equal("add", expression.Name.ToString());
        Assert.Equal(2, expression.ArgumentList.Arguments.Count);
        Assert.Null(expression.TokenTree);
        Assert.Empty(tree.GetDiagnostics());
    }

    [Fact]
    public void FreestandingMacroExpression_ParsesQualifiedGenericNameAndRawBody()
    {
        var tree = SyntaxTree.ParseText("""
            func Main() -> int => Raven.Macros.Compile<Func<int>>! {
                42
            }
            """);

        var expression = tree.GetRoot()
            .DescendantNodes()
            .OfType<FreestandingMacroExpressionSyntax>()
            .Single();

        Assert.IsType<QualifiedNameSyntax>(expression.Name);
        Assert.True(expression.TryGetMacroName(out var macroName));
        Assert.Equal("Raven.Macros.Compile", macroName);
        Assert.Contains("42", Assert.IsType<MacroTokenTreeSyntax>(expression.TokenTree).BodyToken.Text);
        Assert.Empty(tree.GetDiagnostics());
    }

    [Fact]
    public void FreestandingMacroExpression_ParsesQualifiedNameAndArguments()
    {
        var tree = SyntaxTree.ParseText("""
            func Main() -> int => Example.Macros.Answer!()
            """);

        var expression = tree.GetRoot()
            .DescendantNodes()
            .OfType<FreestandingMacroExpressionSyntax>()
            .Single();

        Assert.IsType<QualifiedNameSyntax>(expression.Name);
        Assert.True(expression.TryGetMacroName(out var macroName));
        Assert.Equal("Example.Macros.Answer", macroName);
        Assert.Empty(tree.GetDiagnostics());
    }

    [Fact]
    public void PostfixExclamationWithoutMacroBody_RemainsPostfixOperator()
    {
        var tree = SyntaxTree.ParseText("""
            func Main(value: string?) -> string => value!.ToString()
            """);

        Assert.Empty(tree.GetRoot().DescendantNodes().OfType<FreestandingMacroExpressionSyntax>());
        Assert.Contains(
            tree.GetRoot().DescendantNodes().OfType<PostfixOperatorExpressionSyntax>(),
            static expression => expression.Kind == SyntaxKind.SuppressNullableWarningExpression);
        Assert.Empty(tree.GetDiagnostics());
    }

    [Fact]
    public void PostfixExclamationBeforeOperator_RemainsPostfixOperator()
    {
        var tree = SyntaxTree.ParseText("""
            func Main(value: int?) -> int => value! + 1
            """);

        Assert.Empty(tree.GetRoot().DescendantNodes().OfType<FreestandingMacroExpressionSyntax>());
        Assert.Contains(
            tree.GetRoot().DescendantNodes().OfType<PostfixOperatorExpressionSyntax>(),
            static expression => expression.Kind == SyntaxKind.SuppressNullableWarningExpression);
        Assert.Empty(tree.GetDiagnostics());
    }

    [Fact]
    public void LineBreakAfterExclamation_DoesNotStartFreestandingMacroExpression()
    {
        var tree = SyntaxTree.ParseText("""
            func Main(value: string?) -> string {
                value!
                { value }
            }
            """);

        Assert.Empty(tree.GetRoot().DescendantNodes().OfType<FreestandingMacroExpressionSyntax>());
        Assert.Contains(
            tree.GetRoot().DescendantNodes().OfType<PostfixOperatorExpressionSyntax>(),
            static expression => expression.Kind == SyntaxKind.SuppressNullableWarningExpression);
    }
}
