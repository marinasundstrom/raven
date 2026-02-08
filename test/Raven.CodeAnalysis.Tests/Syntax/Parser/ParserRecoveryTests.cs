using System;
using System.IO;
using System.Linq;
using System.Threading.Tasks;

using Raven.CodeAnalysis.Syntax;
using Raven.CodeAnalysis.Syntax.InternalSyntax.Parser;

using Xunit;

namespace Raven.CodeAnalysis.Syntax.Parser.Tests;

public class ParserRecoveryTests
{
    [Fact]
    public void StatementBlock_MissingCloseBrace_ProducesMissingToken()
    {
        var lexer = new Lexer(new StringReader("{ let x = 1"));
        var context = new BaseParseContext(lexer);
        var parser = new StatementSyntaxParser(context);

        var block = Assert.IsType<BlockStatementSyntax>(parser.ParseStatement().CreateRed());

        Assert.True(block.CloseBraceToken.IsMissing);
        Assert.True(block.Statements.Count > 0);
    }

    [Fact]
    public void ExpressionBlock_MissingCloseBrace_ProducesMissingToken()
    {
        var lexer = new Lexer(new StringReader("{ 1"));
        var context = new BaseParseContext(lexer);
        var parser = new ExpressionSyntaxParser(context);

        var block = (BlockSyntax)parser.ParseBlockSyntax().CreateRed();

        Assert.True(block.CloseBraceToken.IsMissing);
        Assert.Single(block.Statements);
    }

    [Fact]
    public void InvocationArgumentList_TrailingCommaBeforeCloseParen_Recovers()
    {
        var lexer = new Lexer(new StringReader("Foo(1, )"));
        var context = new BaseParseContext(lexer);
        var parser = new ExpressionSyntaxParser(context);

        var expression = (ExpressionSyntax)parser.ParseExpression().CreateRed();
        var invocation = Assert.IsType<InvocationExpressionSyntax>(expression);

        Assert.False(invocation.ArgumentList.CloseParenToken.IsMissing);
        Assert.Single(invocation.ArgumentList.Arguments);
        Assert.Contains(parser.Diagnostics, d => d.Descriptor == CompilerDiagnostics.InvalidExpressionTerm);
    }

    [Fact]
    public void BracketedArgumentList_TrailingCommaBeforeCloseBracket_Recovers()
    {
        var lexer = new Lexer(new StringReader("foo[1, ]"));
        var context = new BaseParseContext(lexer);
        var parser = new ExpressionSyntaxParser(context);

        var expression = (ExpressionSyntax)parser.ParseExpression().CreateRed();
        var elementAccess = Assert.IsType<ElementAccessExpressionSyntax>(expression);

        Assert.False(elementAccess.ArgumentList.CloseBracketToken.IsMissing);
        Assert.Single(elementAccess.ArgumentList.Arguments);
        Assert.Contains(parser.Diagnostics, d => d.Descriptor == CompilerDiagnostics.InvalidExpressionTerm);
    }

    [Fact]
    public void CollectionExpression_MissingCloseBracket_ProducesMissingToken()
    {
        var lexer = new Lexer(new StringReader("[1,"));
        var context = new BaseParseContext(lexer);
        var parser = new ExpressionSyntaxParser(context);

        var expression = (ExpressionSyntax)parser.ParseExpression().CreateRed();
        var collection = Assert.IsType<CollectionExpressionSyntax>(expression);

        Assert.True(collection.CloseBracketToken.IsMissing);
        Assert.NotEmpty(collection.Elements);
    }

    [Fact]
    public void CompilationUnit_StrayTokenAfterDeclaration_ParsesAsIncompleteMember()
    {
        var source = """
            class Foo {}
            s
            class Bar {}
            """;

        var tree = SyntaxTree.ParseText(source);
        var root = tree.GetRoot();

        Assert.Equal(3, root.Members.Count);
        Assert.IsType<ClassDeclarationSyntax>(root.Members[0]);
        Assert.IsType<IncompleteMemberDeclarationSyntax>(root.Members[1]);
        Assert.IsType<ClassDeclarationSyntax>(root.Members[2]);
        Assert.DoesNotContain(root.Members, member => member is GlobalStatementSyntax);
        Assert.Contains(tree.GetDiagnostics(), d => d.Descriptor == CompilerDiagnostics.UnexpectedTokenInIncompleteSyntax);
    }

    [Fact]
    public void FileScopedNamespace_StrayTokenAfterDeclaration_ParsesAsIncompleteMember()
    {
        var source = """
            namespace N;
            class Foo {}
            s
            class Bar {}
            """;

        var tree = SyntaxTree.ParseText(source);
        var root = tree.GetRoot();
        var fileScopedNamespace = Assert.IsType<FileScopedNamespaceDeclarationSyntax>(Assert.Single(root.Members));

        Assert.IsType<ClassDeclarationSyntax>(fileScopedNamespace.Members[0]);
        Assert.Contains(fileScopedNamespace.Members, member => member is IncompleteMemberDeclarationSyntax);
        Assert.DoesNotContain(fileScopedNamespace.Members, member => member is GlobalStatementSyntax);
        Assert.Contains(tree.GetDiagnostics(), d => d.Descriptor == CompilerDiagnostics.UnexpectedTokenInIncompleteSyntax);
    }

    [Fact]
    public void CompilationUnit_GlobalStatementBeforeDeclaration_RemainsGlobalStatement()
    {
        var source = """
            let x = 1
            class Foo {}
            """;

        var tree = SyntaxTree.ParseText(source);
        var root = tree.GetRoot();

        Assert.Equal(2, root.Members.Count);
        Assert.IsType<GlobalStatementSyntax>(root.Members[0]);
        Assert.IsType<ClassDeclarationSyntax>(root.Members[1]);
    }

    [Fact]
    public void FileScopedNamespace_GlobalStatementBeforeDeclaration_RemainsGlobalStatement()
    {
        var source = """
            namespace N;
            let x = 1
            class Foo {}
            """;

        var tree = SyntaxTree.ParseText(source);
        var root = tree.GetRoot();
        var fileScopedNamespace = Assert.IsType<FileScopedNamespaceDeclarationSyntax>(Assert.Single(root.Members));

        Assert.Equal(2, fileScopedNamespace.Members.Count);
        Assert.IsType<GlobalStatementSyntax>(fileScopedNamespace.Members[0]);
        Assert.IsType<ClassDeclarationSyntax>(fileScopedNamespace.Members[1]);
    }

    [Fact]
    public void CompilationUnit_FunctionAfterUnion_ParsesAsGlobalStatement()
    {
        var source = """
            WriteLine(1)
            union Option<T> {
                Some(value: T)
                None
            }
            func tapIfPositive(opt: Option<int>) -> Option<int> {
                return opt
            }
            """;

        var tree = SyntaxTree.ParseText(source);
        var root = tree.GetRoot();

        Assert.Equal(3, root.Members.Count);
        Assert.IsType<GlobalStatementSyntax>(root.Members[0]);
        Assert.IsType<UnionDeclarationSyntax>(root.Members[1]);
        var trailingGlobal = Assert.IsType<GlobalStatementSyntax>(root.Members[2]);
        Assert.IsType<FunctionStatementSyntax>(trailingGlobal.Statement);
        Assert.DoesNotContain(root.Members, member => member is IncompleteMemberDeclarationSyntax);
    }

    [Fact]
    public void FileScopedNamespace_FunctionAfterUnion_ParsesAsGlobalStatement()
    {
        var source = """
            namespace N;
            WriteLine(1)
            union Option<T> {
                Some(value: T)
                None
            }
            func tapIfPositive(opt: Option<int>) -> Option<int> {
                return opt
            }
            """;

        var tree = SyntaxTree.ParseText(source);
        var root = tree.GetRoot();
        var fileScopedNamespace = Assert.IsType<FileScopedNamespaceDeclarationSyntax>(Assert.Single(root.Members));

        Assert.Equal(3, fileScopedNamespace.Members.Count);
        Assert.IsType<GlobalStatementSyntax>(fileScopedNamespace.Members[0]);
        Assert.IsType<UnionDeclarationSyntax>(fileScopedNamespace.Members[1]);
        var trailingGlobal = Assert.IsType<GlobalStatementSyntax>(fileScopedNamespace.Members[2]);
        Assert.IsType<FunctionStatementSyntax>(trailingGlobal.Statement);
        Assert.DoesNotContain(fileScopedNamespace.Members, member => member is IncompleteMemberDeclarationSyntax);
    }

    [Fact]
    public void TypeMembers_OnSameLine_AreReportedAsMissingTerminator()
    {
        var source = """
            class C {
                public A() -> int { return 1 } public B() -> int { return 2 }
            }
            """;

        var tree = SyntaxTree.ParseText(source);
        var root = tree.GetRoot();
        var declaration = Assert.Single(root.Members.OfType<ClassDeclarationSyntax>());

        Assert.True(declaration.Members.Count >= 2);
        Assert.Contains(
            tree.GetDiagnostics(),
            d => d.Descriptor == CompilerDiagnostics.ExpectedNewLineBetweenDeclarations);
    }

    [Fact]
    public void TypeDeclarations_OnSameLineWithoutTerminator_ReportMissingTerminatorDiagnostic()
    {
        var source = "class A {} trait T for A {}";

        var tree = SyntaxTree.ParseText(source);
        var root = tree.GetRoot();
        var diagnostic = Assert.Single(
            tree.GetDiagnostics().Where(d => d.Descriptor == CompilerDiagnostics.ExpectedNewLineBetweenDeclarations));

        Assert.Equal(2, root.Members.Count);
        Assert.Equal(source.IndexOf(" trait", StringComparison.Ordinal), diagnostic.Location.SourceSpan.Start);
        Assert.Equal(1, diagnostic.Location.SourceSpan.Length);
    }

    [Fact]
    public void Statements_OnSameLineWithoutTerminator_ReportDiagnosticAtInsertionPoint()
    {
        var source = "let a = 1 let b = 2";
        var tree = SyntaxTree.ParseText(source);

        var diagnostic = Assert.Single(
            tree.GetDiagnostics().Where(d => d.Descriptor == CompilerDiagnostics.ConsecutiveStatementsMustBeSeparatedBySemicolon));

        Assert.Equal(source.IndexOf(" let b", StringComparison.Ordinal), diagnostic.Location.SourceSpan.Start);
        Assert.Equal(1, diagnostic.Location.SourceSpan.Length);
    }

    [Fact]
    public void TypeDeclarations_OnNextLine_AreAcceptedWithoutMissingTerminatorDiagnostic()
    {
        var source = """
            class A {}
            trait T for A {}
            """;

        var tree = SyntaxTree.ParseText(source);
        var root = tree.GetRoot();

        Assert.Equal(2, root.Members.Count);
        Assert.DoesNotContain(
            tree.GetDiagnostics(),
            d => d.Descriptor == CompilerDiagnostics.ExpectedNewLineBetweenDeclarations);
    }

    [Fact]
    public void GlobalStatement_PreservesTerminatorToken()
    {
        var source = """
            let x = 1
            class C {}
            """;

        var tree = SyntaxTree.ParseText(source);
        var root = tree.GetRoot();
        var global = Assert.IsType<GlobalStatementSyntax>(root.Members[0]);

        Assert.Equal(SyntaxKind.None, global.TerminatorToken.Kind);
    }

    [Fact]
    public void IncompleteMember_PreservesTerminatorToken()
    {
        var source = """
            class C {
                ?
                public M() -> int { return 1 }
            }
            """;

        var tree = SyntaxTree.ParseText(source);
        var declaration = Assert.Single(tree.GetRoot().Members.OfType<ClassDeclarationSyntax>());
        var incomplete = Assert.IsType<IncompleteMemberDeclarationSyntax>(declaration.Members[0]);

        Assert.Equal(SyntaxKind.NewLineToken, incomplete.TerminatorToken.Kind);
    }

    [Fact]
    public void ExtensionMemberRecovery_MissingBlockClose_DoesNotHangOnFollowingForKeyword()
    {
        var source = """
            extension OptionExtensions<T> for Option<T> {
                public UnwrapOr(defaultValue: T) -> T {
                    if self is .Some(val value) {
                        return value
                    return defaultValue
                }
            }

            extension OptionExtensionsNested<T> for Option<Option<T>> {
            }
            """;

        var parseTask = Task.Run(() => SyntaxTree.ParseText(source));
        Assert.True(parseTask.Wait(TimeSpan.FromSeconds(5)));

        var tree = parseTask.Result;
        var root = tree.GetRoot();
        Assert.NotEmpty(root.Members.OfType<ExtensionDeclarationSyntax>());
        Assert.Contains(
            tree.GetDiagnostics(),
            d => d.Descriptor == CompilerDiagnostics.UnexpectedTokenInIncompleteSyntax &&
                 string.Equals(d.GetMessageArgs().FirstOrDefault()?.ToString(), "for", StringComparison.Ordinal));
    }
}
