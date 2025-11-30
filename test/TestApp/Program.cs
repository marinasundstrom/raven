using System;
using System.IO;
using System.Linq;

using Raven.CodeAnalysis;
using Raven.CodeAnalysis.Syntax;
using Raven.CodeAnalysis.Text;

using static Raven.CodeAnalysis.Syntax.SyntaxFactory;

class Program
{
    private const string TargetFramework = "net9.0";
    static void Main()
    {
        PrintMembers();
    }

    static void QuoterTest()
    {
        var code = """
        import System.*

        if true {
            Console.WriteLine("Hello, World!")
        } else {
            42
        }
        """;

        Console.WriteLine("Input:");
        Console.WriteLine(code);

        var quoted = RavenQuoter.QuoteText(code, new RavenQuoterOptions
        {
            IncludeTrivia = true,
            GenerateUsingDirectives = true,
            UseStaticSyntaxFactoryImport = true,
            UseNamedArguments = true,
            IgnoreNullValue = true,
            UseFactoryPropsForSimpleTokens = true
        });

        Console.WriteLine("\n\nAST in C#:");

        Console.WriteLine(quoted);

        var cu = CompilationUnit(
    attributeLists: List<AttributeListSyntax>(),
    imports: SingletonList(
        ImportDirective(
            importKeyword: ImportKeyword.WithTrailingTrivia(TriviaList(Trivia(SyntaxKind.WhitespaceTrivia, " "))),
            name: QualifiedName(
                left: IdentifierName(
                    identifier: Identifier("System")
                ),
                dotToken: DotToken,
                right: WildcardName(
                    startToken: StarToken
                )
            ),
            terminatorToken: NewLineToken
        )),
    aliases: List<AliasDirectiveSyntax>(),
    members: SingletonList<MemberDeclarationSyntax>(
        GlobalStatement(
            attributeLists: List<AttributeListSyntax>(),
            modifiers: TokenList(),
            statement: IfStatement(
                ifKeyword: IfKeyword.WithLeadingTrivia(TriviaList(Trivia(SyntaxKind.EndOfLineTrivia, "\n"))).WithTrailingTrivia(TriviaList(Trivia(SyntaxKind.WhitespaceTrivia, " "))),
                condition: LiteralExpression(
                    kind: SyntaxKind.TrueLiteralExpression,
                    token: TrueKeyword.WithTrailingTrivia(TriviaList(Trivia(SyntaxKind.WhitespaceTrivia, " ")))
                ),
                thenStatement: BlockStatement(
                    openBraceToken: OpenBraceToken,
                    statements: SingletonList<StatementSyntax>(
                        ExpressionStatement(
                            expression: InvocationExpression(
                                expression: MemberAccessExpression(
                                    kind: SyntaxKind.SimpleMemberAccessExpression,
                                    expression: IdentifierName(
                                        identifier: Identifier("Console").WithLeadingTrivia(TriviaList(
                                            new[] {
                                                Trivia(SyntaxKind.EndOfLineTrivia, "\n"),
                                                Trivia(SyntaxKind.WhitespaceTrivia, "    ")
                                            }
                                        ))
                                    ),
                                    operatorToken: DotToken,
                                    name: IdentifierName(
                                        identifier: Identifier("WriteLine")
                                    )
                                ),
                                argumentList: ArgumentList(
                                    openParenToken: OpenParenToken,
                                    arguments: SingletonSeparatedList<ArgumentSyntax>(
                                        Argument(
                                            expression: LiteralExpression(
                                                kind: SyntaxKind.StringLiteralExpression,
                                                token: Literal("\"Hello, World!\"", "Hello, World!")
                                            )
                                        )),
                                    closeParenToken: CloseParenToken
                                )
                            ),
                            terminatorToken: NewLineToken
                        )),
                    closeBraceToken: CloseBraceToken.WithTrailingTrivia(TriviaList(Trivia(SyntaxKind.WhitespaceTrivia, " ")))
                ),
                elseClause: ElseClause2(
                    elseKeyword: ElseKeyword.WithTrailingTrivia(TriviaList(Trivia(SyntaxKind.WhitespaceTrivia, " "))),
                    statement: BlockStatement(
                        openBraceToken: OpenBraceToken,
                        statements: SingletonList<StatementSyntax>(
                            ExpressionStatement(
                                expression: LiteralExpression(
                                    kind: SyntaxKind.NumericLiteralExpression,
                                    token: Literal("42", 42).WithLeadingTrivia(TriviaList(
                                        new[] {
                                            Trivia(SyntaxKind.EndOfLineTrivia, "\n"),
                                            Trivia(SyntaxKind.WhitespaceTrivia, "    ")
                                        }
                                    ))
                                ),
                                terminatorToken: NewLineToken
                            )),
                        closeBraceToken: CloseBraceToken
                    )
                ),
                terminatorToken: Token(SyntaxKind.None)
            )
        )),
    endOfFileToken: EndOfFileToken
).NormalizeWhitespace();

        Console.WriteLine("\n\nAST to string:");

        Console.WriteLine(cu);

        var syntaxTree = SyntaxTree.Create(cu);

        var compilation = Compilation.Create("test", [syntaxTree], options: new CompilationOptions(OutputKind.ConsoleApplication));
        var version = TargetFrameworkResolver.ResolveVersion(TargetFramework);
        var refDir = TargetFrameworkResolver.GetDirectoryPath(version);
        var references = new[]
        {
            MetadataReference.CreateFromFile(Path.Combine(refDir!, "System.Runtime.dll")),
            MetadataReference.CreateFromFile(typeof(Console).Assembly.Location),
            MetadataReference.CreateFromFile(Path.Combine(refDir!, "System.Collections.dll")),
            MetadataReference.CreateFromFile(Path.Combine(refDir!, "System.Runtime.Extensions.dll")),
            MetadataReference.CreateFromFile(Path.Combine(refDir!, "System.IO.FileSystem.dll")),
        };
        compilation = compilation.AddReferences(references);

        Console.WriteLine("\n\nAST to highlighted syntax:");

        Console.WriteLine(ConsoleSyntaxHighlighter.WriteNodeToText(syntaxTree.GetRoot(), compilation, true));
    }

    static void PrintMembers()
    {
        var compilation = Compilation.Create("test", options: new CompilationOptions(OutputKind.ConsoleApplication));
        var version = TargetFrameworkResolver.ResolveVersion(TargetFramework);
        var refDir = TargetFrameworkResolver.GetDirectoryPath(version);
        var references = new[]
        {
            MetadataReference.CreateFromFile(Path.Combine(refDir!, "System.Runtime.dll")),
            MetadataReference.CreateFromFile(typeof(Console).Assembly.Location),
            MetadataReference.CreateFromFile(Path.Combine(refDir!, "System.Collections.dll")),
            MetadataReference.CreateFromFile(Path.Combine(refDir!, "System.Runtime.Extensions.dll")),
            MetadataReference.CreateFromFile(Path.Combine(refDir!, "System.IO.FileSystem.dll")),
        };
        compilation = compilation.AddReferences(references);

        // force setup
        compilation.GetDiagnostics();

        //ReadConsoleClass(compilation, refDir, references);

        var type = compilation.GetTypeByMetadataName("System.String");

        if (type.IsUnboundGenericType)
        {
            var arg = compilation.GetSpecialType(SpecialType.System_String);
            type = (INamedTypeSymbol?)type.Construct(arg);
        }

        if (type != null)
        {
            Console.WriteLine($"Base type: {type.BaseType?.ToDisplayString(SymbolDisplayFormat.FullyQualifiedFormat.WithTypeQualificationStyle(SymbolDisplayTypeQualificationStyle.NameOnly)) ?? "<None>"}");

            Console.WriteLine();

            Console.WriteLine($"Implements:");

            foreach (var iface in type.AllInterfaces) Console.WriteLine("• " + iface.ToDisplayString(SymbolDisplayFormat.FullyQualifiedFormat.WithTypeQualificationStyle(SymbolDisplayTypeQualificationStyle.NameOnly)));

            Console.WriteLine();

            var members = type.GetMembers()
                    .Where(x => x is not IMethodSymbol ms || ms.AssociatedSymbol is null)
                    //.Where(x => x is IMethodSymbol mb && mb.MethodKind == MethodKind.Constructor)
                    .ToArray();

            Console.WriteLine($"Members ({members.Length}):");
            foreach (var m in members) Console.WriteLine("• " + m.ToDisplayString(SymbolDisplayFormat.FullyQualifiedFormat.WithTypeQualificationStyle(SymbolDisplayTypeQualificationStyle.NameOnly)));
        }
    }

    private static void ReadConsoleClass(Compilation compilation, string refDir, PortableExecutableReference[] references)
    {
        var consoleType = compilation.GetTypeByMetadataName("System.Console");
        Console.WriteLine($"Console type found: {consoleType != null}");
        if (consoleType != null)
        {
            var writeLineMembers = consoleType.GetMembers("WriteLine");
            Console.WriteLine($"WriteLine members: {writeLineMembers.Length}");
            foreach (var m in writeLineMembers) Console.WriteLine(m.ToDisplayString(SymbolDisplayFormat.FullyQualifiedFormat));
        }

        var resolver = new System.Reflection.PathAssemblyResolver(references.Select(r => r.FilePath));
        var mlc = new System.Reflection.MetadataLoadContext(resolver);
        var asm = mlc.LoadFromAssemblyPath(Path.Combine(refDir!, "System.Console.dll"));
        var consoleType2 = asm.GetType("System.Console", true);
        Console.WriteLine($"Reflection Console type null? {consoleType2 == null}");
        Console.WriteLine($"Declared methods via reflection: {consoleType2?.GetMethods().Length}");

        var stringType = compilation.GetTypeByMetadataName("System.String");
        Console.WriteLine($"String type found: {stringType != null}");
        if (stringType != null)
        {
            var lengthMembers = stringType.GetMembers("Length");
            Console.WriteLine($"String Length members: {lengthMembers.Length}");
        }
    }
}
