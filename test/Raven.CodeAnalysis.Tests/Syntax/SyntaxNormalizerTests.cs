using System;
using System.Linq;

using Raven.CodeAnalysis.Syntax;

using Xunit;

namespace Raven.CodeAnalysis.Syntax.Tests;

public class SyntaxNormalizerTests
{
    [Fact]
    public void NormalizeWhitespace_FormatsControlFlowAndSpacing()
    {
        const string source = "func  main( x:int,y :int)->int{if(x>y){return x}else{return y}}";
        var tree = SyntaxTree.ParseText(source);

        var normalized = tree.GetRoot().NormalizeWhitespace().ToFullString();

        var expected = """
func main(x: int, y: int) -> int {
    if (x > y) {
        return x
    } else {
        return y
    }
}
""";

        Assert.Equal(expected, normalized);
    }

    [Fact]
    public void NormalizeWhitespace_FormatsTypeSyntaxes()
    {
        const string source = "val x:Result<int,string>?=Foo<int ,string >().Bar( a,b )";
        var tree = SyntaxTree.ParseText(source);

        var normalized = tree.GetRoot().NormalizeWhitespace().ToFullString();

        Assert.Equal("val x: Result<int, string>? = Foo<int, string>().Bar(a, b)", normalized);
    }

    [Fact]
    public void NormalizeWhitespace_HandlesRepresentativeSyntaxes()
    {
        var snippets = new[]
        {
            "import  System.Console.*\n\nfunc  Main( )->() {WriteLine(\"hi\")}",
            "val res= value match{.Ok(val x)=>x,.Error(val e)=>0}",
            "class Foo<T>{public init(x:T){self.x=x} val x:T}",
            "union Result<T, E> { case Ok(value: T)\ncase Error(error: E) }",
            "func f(x:int)->int{if(x>0){return x}else{return -x}}"
        };

        foreach (var snippet in snippets)
        {
            var tree = SyntaxTree.ParseText(snippet);
            if (tree.GetDiagnostics().Any())
            {
                continue;
            }

            var root = tree.GetRoot();
            var once = root.NormalizeWhitespace().ToFullString();

            Assert.False(string.IsNullOrWhiteSpace(once));

            var normalizedTree = SyntaxTree.ParseText(once);
            Assert.NotNull(normalizedTree.GetRoot());
        }
    }

    [Fact]
    public void NormalizeWhitespace_PreservesIndentationAcrossNewLineTerminators()
    {
        const string source = """
import System.Console.*

val foo = Foo()
foo.Bar()

class Foo {
  public func Bar() -> () {
    val x = 2
    WriteLine(x)
  }
}
""";

        var tree = SyntaxTree.ParseText(source);
        var normalized = tree.GetRoot().NormalizeWhitespace().ToFullString();

        var expected = """
import System.Console.*
val foo = Foo()
foo.Bar()
class Foo {
    public func Bar() -> () {
        val x = 2
        WriteLine(x)
    }
}
""";

        Assert.Equal(expected, normalized);
        Assert.DoesNotContain(" \n", normalized);
    }

    [Fact]
    public void NormalizeWhitespace_FormatsCollectionComprehension()
    {
        const string source = "val odds=[for n in nums if n%2==1=>n*10]";
        var tree = SyntaxTree.ParseText(source);

        var normalized = tree.GetRoot().NormalizeWhitespace().ToFullString();

        Assert.Equal("val odds = [for n in nums if n % 2 == 1 => n * 10]", normalized);
    }

    [Fact]
    public void NormalizeWhitespace_FormatsLeadingAttributeListsOnSeparateLines()
    {
        const string source = """
class CounterViewModel{
#[Observable][Required]var Count:int=0
}
""";

        var tree = SyntaxTree.ParseText(source);

        var normalized = tree.GetRoot().NormalizeWhitespace().ToFullString();

        var expected = """
class CounterViewModel {
    [#Observable]
    [Required]
    var Count: int = 0
}
""";

        Assert.Equal(expected, normalized);
    }

    [Fact]
    public void NormalizeWhitespace_FormatsPatternBindingForms()
    {
        const string source = """
func  main()->(){
if val Person(1,name,_)=person{
WriteLine(name)
}
for var (x,0) in points{
WriteLine(x)
}
match value{
val [first,second,...rest]=>first+second+rest.Length
val Some((x,y))=>x+y
}
}
""";

        var tree = SyntaxTree.ParseText(source);

        var normalized = tree.GetRoot().NormalizeWhitespace().ToFullString();

        var expected = """
func main() -> () {
    if val Person(1, name, _) = person {
        WriteLine(name)
    }
    for var (x, 0) in points {
        WriteLine(x)
    }
    match value {
        val [first, second, ...rest] => first + second + rest.Length
        val Some((x, y)) => x + y
    }
}
""";

        Assert.Equal(expected, normalized);
    }

    [Fact]
    public void Formatter_Format_FormatsOnlyAnnotatedNode()
    {
        const string source = """
func  First( )->() {val x=1}
func  Second( )->() {val y=2}
""";

        var tree = SyntaxTree.ParseText(source);
        var root = tree.GetRoot();
        var functions = root.DescendantNodes().Where(static node => node is FunctionStatementSyntax).Cast<FunctionStatementSyntax>().ToArray();
        var annotatedSecond = (FunctionStatementSyntax)functions[1].WithAdditionalAnnotations(Formatter.Annotation);
        var updatedRoot = root.ReplaceNode(functions[1], annotatedSecond);

        var formatted = Formatter.Format(updatedRoot).ToFullString();

        var expected = """
func  First( )->() {val x=1}
func Second() -> () {
    val y = 2
}
""";

        Assert.Equal(expected, formatted);
    }

    [Fact]
    public void Formatter_Format_RewritesElasticTriviaOutsideAnnotatedNode()
    {
        const string source = """
val  first=1
val  second=2
""";

        var tree = SyntaxTree.ParseText(source);
        var root = tree.GetRoot();
        var globals = root.DescendantNodes().Where(static node => node is GlobalStatementSyntax).Cast<GlobalStatementSyntax>().ToArray();
        var firstWithElasticTrivia = globals[0].ReplaceTokens(globals[0].DescendantTokens(), static (original, _) =>
            original
                .WithLeadingTrivia(ToElasticTrivia(original.LeadingTrivia))
                .WithTrailingTrivia(ToElasticTrivia(original.TrailingTrivia)));
        var annotatedSecond = globals[1].WithAdditionalAnnotations(Formatter.Annotation);
        var updatedRoot = root
            .ReplaceNode(globals[0], firstWithElasticTrivia)
            .ReplaceNode(globals[1], annotatedSecond);

        var formatted = Formatter.Format(updatedRoot).ToFullString();

        Assert.Equal(
            """
val first = 1
val second = 2
""",
            formatted);
    }

    [Fact]
    public void Formatter_Format_FormatsAnnotatedIfPatternStatement()
    {
        const string source = """
func First() -> () { val keep = 1 }
func  Second( )->() {if val Person(1,name,_)=person{WriteLine(name)}}
""";

        var tree = SyntaxTree.ParseText(source);
        var root = tree.GetRoot();
        var functions = root.DescendantNodes().Where(static node => node is FunctionStatementSyntax).Cast<FunctionStatementSyntax>().ToArray();
        var annotatedSecond = (FunctionStatementSyntax)functions[1].WithAdditionalAnnotations(Formatter.Annotation);
        var updatedRoot = root.ReplaceNode(functions[1], annotatedSecond);

        var formatted = Formatter.Format(updatedRoot).ToFullString();

        var expected = """
func First() -> () { val keep = 1 }
func Second() -> () {
    if val Person(1, name, _) = person {
        WriteLine(name)
    }
}
""";

        Assert.Equal(expected, formatted);
    }

    [Fact]
    public void Formatter_Format_FormatsAdjacentTypeMembers()
    {
        var tree = SyntaxTree.ParseText("""
            class MyViewModel {}
            """);
        var classDeclaration = tree.GetRoot().DescendantNodes().OfType<ClassDeclarationSyntax>().Single();

        var membersTree = SyntaxTree.ParseText("""
            class __GeneratedContainer {
                private var _Title: string = ""
                var Title: string {
                    get => _Title
                    set {
                        _Title = value
                    }
                }
            }
            """);
        var members = membersTree.GetRoot()
            .DescendantNodes()
            .OfType<ClassDeclarationSyntax>()
            .Single()
            .Members;

        var updatedClass = classDeclaration.WithMembers(members);
        var formatted = Formatter.Format(updatedClass).ToFullString();

        var expected = """
class MyViewModel {
    private var _Title: string = ""
    var Title: string {
        get => _Title
        set {
            _Title = value
        }
    }
}
""";

        Assert.Equal(expected, formatted);
    }

    [Fact]
    public void Formatter_Format_KeepsClosingParenAttachedAfterBlockBodiedLambdaArgument()
    {
        var tree = SyntaxTree.ParseText("""
            func Main() {
                use subscription = Observe((value) => {
                    WriteLine(value)
                })
            }
            """);
        var root = tree.GetRoot();
        var useStatement = root.DescendantNodes().OfType<UseDeclarationStatementSyntax>().Single();
        var rewrittenStatement = useStatement
            .WithAdditionalAnnotations(Formatter.Annotation)
            .ReplaceTokens(useStatement.DescendantTokens(), static (original, _) =>
                original
                    .WithLeadingTrivia(ToElasticTrivia(original.LeadingTrivia))
                    .WithTrailingTrivia(ToElasticTrivia(original.TrailingTrivia)));
        var updatedRoot = root.ReplaceNode(useStatement, rewrittenStatement);

        var formatted = Formatter.Format(updatedRoot).ToFullString();

        Assert.Contains(
            "Observe((value) => {\n        WriteLine(value)\n    })",
            formatted);
    }

    [Fact]
    public void NormalizeWhitespace_FormatsAccessorListsAndRawBlockStatements()
    {
        const string source = """
class MyViewModel {
    var Title: string {
        get => _Title;
        set {
            val oldValue = _Title
            _Title = value
            RaisePropertyChanged(nameof(Title), oldValue, value)
        }
    }
}
""";

        var tree = SyntaxTree.ParseText(source);
        var property = tree.GetRoot().DescendantNodes().OfType<PropertyDeclarationSyntax>().Single();
        var accessors = property.AccessorList!.Accessors;
        var setAccessor = accessors[1];
        var statements = setAccessor.Body!.Statements;

        var rawProperty = property
            .WithAccessorList(SyntaxFactory.AccessorList(
                SyntaxFactory.List([
                    accessors[0].WithTerminatorToken(SyntaxFactory.Token(SyntaxKind.None)),
                    setAccessor.WithTerminatorToken(SyntaxFactory.Token(SyntaxKind.None)).WithBody(
                        setAccessor.Body.WithStatements(SyntaxFactory.List<StatementSyntax>([
                            ((LocalDeclarationStatementSyntax)statements[0]).WithTerminatorToken(SyntaxFactory.Token(SyntaxKind.None)),
                            ((AssignmentStatementSyntax)statements[1]).WithTerminatorToken(SyntaxFactory.Token(SyntaxKind.None)),
                            ((ExpressionStatementSyntax)statements[2]).WithTerminatorToken(SyntaxFactory.Token(SyntaxKind.None))
                        ])))
                ])));

        var normalized = rawProperty.NormalizeWhitespace().ToFullString();

        var expected = """
var Title: string {
    get => _Title
    set {
        val oldValue = _Title
        _Title = value
        RaisePropertyChanged(nameof(Title), oldValue, value)
    }
}
""";

        Assert.Equal(expected, normalized);
    }

    private static SyntaxTriviaList ToElasticTrivia(SyntaxTriviaList triviaList)
    {
        return SyntaxFactory.TriviaList(triviaList.Select(static trivia => trivia.Kind switch
        {
            SyntaxKind.WhitespaceTrivia => SyntaxFactory.ElasticWhitespace(trivia.Text),
            SyntaxKind.TabTrivia => SyntaxFactory.ElasticTab,
            SyntaxKind.LineFeedTrivia => SyntaxFactory.ElasticLineFeed,
            SyntaxKind.CarriageReturnTrivia => SyntaxFactory.ElasticCarriageReturn,
            SyntaxKind.CarriageReturnLineFeedTrivia => SyntaxFactory.ElasticCarriageReturnLineFeed,
            _ => trivia
        }));
    }
}
