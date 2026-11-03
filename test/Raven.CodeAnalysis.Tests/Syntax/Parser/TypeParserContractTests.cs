using Raven.CodeAnalysis.Syntax.InternalSyntax.Parser;

namespace Raven.CodeAnalysis.Syntax.Parser.Tests;

[Trait("Target", "Shared")]
public sealed class TypeParserContractTests
{
    [Theory]
    [InlineData("class C {}", SyntaxKind.ClassDeclaration)]
    [InlineData("open class C {}", SyntaxKind.ClassDeclaration)]
    [InlineData("class C<T> where T: class {}", SyntaxKind.ClassDeclaration)]
    [InlineData("class C(private val Value: int) : Base(Value), IFoo {}", SyntaxKind.ClassDeclaration)]
    [InlineData("struct S {}", SyntaxKind.StructDeclaration)]
    [InlineData("ref struct S {}", SyntaxKind.StructDeclaration)]
    [InlineData("interface I<out T, in U> {}", SyntaxKind.InterfaceDeclaration)]
    [InlineData("record R(Value: int)", SyntaxKind.RecordDeclaration)]
    [InlineData("record class R(Value: int) {}", SyntaxKind.RecordDeclaration)]
    [InlineData("record struct R(Value: int) {}", SyntaxKind.RecordDeclaration)]
    [InlineData("sealed class C permits A, B {}", SyntaxKind.ClassDeclaration)]
    public void DeclarationPreservesSourceAndShape(string source, SyntaxKind kind)
    {
        var parser = Parser(source);
        var result = parser.Parse().CreateRed();
        Assert.Equal(kind, result.Kind);
        Assert.Equal(source, result.ToFullString());
        Assert.Empty(parser.Diagnostics);
        Assert.Equal(SyntaxKind.EndOfFileToken, parser.PeekToken().Kind);
    }

    [Theory]
    [InlineData("init() {}", SyntaxKind.ConstructorDeclaration)]
    [InlineData("init(value: int) : self() {}", SyntaxKind.ConstructorDeclaration)]
    [InlineData("init(value: int) : base(value) {}", SyntaxKind.ConstructorDeclaration)]
    [InlineData("init => Setup()", SyntaxKind.ParameterlessConstructorDeclaration)]
    [InlineData("{ Setup() }", SyntaxKind.InitializerBlockDeclaration)]
    [InlineData("finally {}", SyntaxKind.FinallyDeclaration)]
    [InlineData("func F() -> int => 1", SyntaxKind.MethodDeclaration)]
    [InlineData("func F<T>(value: T) -> T => value", SyntaxKind.MethodDeclaration)]
    [InlineData("func IFoo.F() {}", SyntaxKind.MethodDeclaration)]
    [InlineData("val P: int => 1", SyntaxKind.PropertyDeclaration)]
    [InlineData("var P: int { get; set; }", SyntaxKind.PropertyDeclaration)]
    [InlineData("val P: int = 1", SyntaxKind.PropertyDeclaration)]
    [InlineData("val IFoo.P: int { get; }", SyntaxKind.PropertyDeclaration)]
    [InlineData("val self[index: int]: int => index", SyntaxKind.IndexerDeclaration)]
    [InlineData("var IFoo.self[index: int]: int { get; set; }", SyntaxKind.IndexerDeclaration)]
    [InlineData("event Changed: System.Action { add; remove; }", SyntaxKind.EventDeclaration)]
    [InlineData("field Value: int = 1", SyntaxKind.FieldDeclaration)]
    [InlineData("const Value: int = 1", SyntaxKind.ConstDeclaration)]
    [InlineData("delegate D(value: int) -> int", SyntaxKind.DelegateDeclaration)]
    [InlineData("static func +(left: C, right: C) -> C => left", SyntaxKind.OperatorDeclaration)]
    [InlineData("static func checked +(left: C, right: C) -> C => left", SyntaxKind.OperatorDeclaration)]
    [InlineData("static func !(value: C) -> bool => false", SyntaxKind.OperatorDeclaration)]
    [InlineData("static func explicit(value: C) -> int => 1", SyntaxKind.ConversionOperatorDeclaration)]
    [InlineData("class Nested {}", SyntaxKind.ClassDeclaration)]
    [InlineData("enum E { A }", SyntaxKind.EnumDeclaration)]
    [InlineData("case Value(int)", SyntaxKind.CaseDeclaration)]
    public void MembersPreserveSourceAndShape(string member, SyntaxKind kind)
    {
        var source = $"class C {{ {member} }}";
        var parser = Parser(source);
        var result = (ClassDeclarationSyntax)parser.Parse().CreateRed();
        Assert.Equal(kind, Assert.Single(result.Members).Kind);
        Assert.Equal(source, result.ToFullString());
        Assert.Empty(parser.Diagnostics);
        Assert.Equal(SyntaxKind.EndOfFileToken, parser.PeekToken().Kind);
    }

    [Theory]
    [InlineData("class C { + }")]
    [InlineData("class C { var P: int { + } }")]
    [InlineData("class C { init() }")]
    public void RecoveryPreservesSourceAndReportsDiagnostics(string source)
    {
        var parser = Parser(source);
        Assert.Equal(source, parser.Parse().CreateRed().ToFullString());
        Assert.NotEmpty(parser.Diagnostics);
        Assert.Equal(SyntaxKind.EndOfFileToken, parser.PeekToken().Kind);
    }

    [Theory]
    [InlineData("class C {}", SyntaxKind.ClassKeyword)]
    [InlineData("record R(Value: int)", SyntaxKind.RecordKeyword)]
    [InlineData("interface I {}", SyntaxKind.InterfaceKeyword)]
    public void TypeKeywordLookaheadRestoresContext(string source, SyntaxKind kind)
    {
        var parser = Parser(source);
        var position = parser.Position;
        Assert.Equal(kind, TypeDeclarationParser.PeekTypeKeyword(parser));
        Assert.Equal(position, parser.Position);
        Assert.Empty(parser.Diagnostics);
        Assert.Equal(source, parser.Parse().CreateRed().ToFullString());
    }

    private static TypeDeclarationParser Parser(string source) =>
        new(new BaseParseContext(new Lexer(new StringReader(source))));
}
