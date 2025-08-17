namespace Generator;

using System;
using System.Diagnostics;
using System.Linq;
using System.Text;

using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using Microsoft.CodeAnalysis.Text;

using static Microsoft.CodeAnalysis.CSharp.SyntaxFactory;

[Generator]
public partial class SymbolPartialGenerator : IIncrementalGenerator
{
    public void Initialize(IncrementalGeneratorInitializationContext context)
    {
        // Identify all partial classes that inherit (directly or indirectly) from Symbol
        var partialClasses = context.SyntaxProvider
            .CreateSyntaxProvider(
                predicate: IsPotentialPartialClass,
                transform: GetClassSymbol)
            .Where(symbol => symbol is not null && symbol.InheritsFromSymbol() && !symbol.Name.Contains("Synthesized"));

        // Generate source for each identified class
        context.RegisterSourceOutput(partialClasses, ProcessSymbolClass);
    }

    private static bool IsPotentialPartialClass(SyntaxNode node, CancellationToken cancellationToken)
    {
        if (node is ClassDeclarationSyntax classDecl)
        {
            return !classDecl.Modifiers.Any(m => m.IsKind(SyntaxKind.AbstractKeyword)) &&
             classDecl.Modifiers.Any(m => m.IsKind(SyntaxKind.PartialKeyword));
        }
        return false;
    }

    private static INamedTypeSymbol? GetClassSymbol(GeneratorSyntaxContext context, CancellationToken cancellationToken)
    {
        if (context.Node is ClassDeclarationSyntax classDecl &&
            context.SemanticModel.GetDeclaredSymbol(classDecl, cancellationToken) is INamedTypeSymbol classSymbol)
        {
            return classSymbol;
        }

        return null;
    }

    private static void ProcessSymbolClass(SourceProductionContext context, INamedTypeSymbol? classSymbol)
    {
        if (classSymbol is null)
            return;

        var namespaceName = classSymbol.ContainingNamespace.ToDisplayString();
        var className = classSymbol.Name;

        var symbolPartialClass = GeneratePartialClass(context, classSymbol);

        // Wrap it in a namespace
        var namespaceDeclaration = FileScopedNamespaceDeclaration(ParseName(namespaceName))
            .AddMembers(
                symbolPartialClass);

        // Convert to source text and add to the compilation
        var syntaxTree = CompilationUnit()
            .AddUsings(
                UsingDirective(ParseName("System")),
                UsingDirective(ParseName("Raven.CodeAnalysis.Symbols")))
            .AddMembers(namespaceDeclaration)
            .NormalizeWhitespace();

        context.AddSource($"{className}_Generated.cs", SourceText.From(syntaxTree.ToFullString(), Encoding.UTF8));
    }
}