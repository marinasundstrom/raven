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
        var code = """
        import System.*

        Console.WriteLine("Hello, World!")
        """;

        Console.WriteLine("Input:");
        Console.WriteLine(code);

        var quoted = RavenQuoter.QuoteText(code, new RavenQuoterOptions
        {
            UseStaticSyntaxFactoryImport = true
        });

        Console.WriteLine("\n\nAST in C#:");

        Console.WriteLine(quoted);

        var cu = CompilationUnit(
    List<AttributeListSyntax>(),
    SingletonList<ImportDirectiveSyntax>(ImportDirective(
        QualifiedName(
            IdentifierName(
                Identifier("System")
            ),
            WildcardName(Token(SyntaxKind.StarToken))
        )
    )
        .WithImportKeyword(Token(SyntaxKind.ImportKeyword).WithTrailingTrivia(TriviaList(Trivia(SyntaxKind.WhitespaceTrivia, " "))))
        .WithTerminatorToken(Token(SyntaxKind.NewLineToken))),
    List<AliasDirectiveSyntax>(),
    SingletonList<MemberDeclarationSyntax>(GlobalStatement(
        List<AttributeListSyntax>(),
        TokenList(),
        ExpressionStatement(
            InvocationExpression(
                MemberAccessExpression(
                    SyntaxKind.SimpleMemberAccessExpression,
                    IdentifierName(
                        Identifier("Console").WithLeadingTrivia(TriviaList(Trivia(SyntaxKind.EndOfLineTrivia, "\n")))
                    ),
                    Token(SyntaxKind.DotToken),
                    IdentifierName(
                        Identifier("WriteLine")
                    )
                ),
                ArgumentList(
                    SingletonSeparatedList<ArgumentSyntax>(Argument(
                        LiteralExpression(
                            SyntaxKind.StringLiteralExpression,
                            Literal("\"Hello, World!\"")
                        )
                    ))
                )
                    .WithOpenParenToken(Token(SyntaxKind.OpenParenToken))
                    .WithCloseParenToken(Token(SyntaxKind.CloseParenToken))
            )
        )
            .WithTerminatorToken(Token(SyntaxKind.None))
    ))
)
    .WithEndOfFileToken(Token(SyntaxKind.EndOfFileToken)).NormalizeWhitespace();

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

        var consoleType = compilation.GetTypeByMetadataName("System.Console");
        Console.WriteLine($"Console type found: {consoleType != null}");
        if (consoleType != null)
        {
            var writeLineMembers = consoleType.GetMembers("WriteLine");
            Console.WriteLine($"WriteLine members: {writeLineMembers.Length}");
            foreach (var m in writeLineMembers) Console.WriteLine(m.ToDisplayString(SymbolDisplayFormat.FullyQualifiedFormat));
        }

        var resolver = new System.Reflection.PathAssemblyResolver(references.Select(r => r.FilePath));
        using var mlc = new System.Reflection.MetadataLoadContext(resolver);
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
