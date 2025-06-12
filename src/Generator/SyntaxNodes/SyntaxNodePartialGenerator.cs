namespace Generator;

using System.Linq;
using System.Text;

using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using Microsoft.CodeAnalysis.Text;

using static Microsoft.CodeAnalysis.CSharp.SyntaxFactory;

[Generator]
public partial class SyntaxNodePartialGenerator : IIncrementalGenerator
{
    public void Initialize(IncrementalGeneratorInitializationContext context)
    {
        //Debugger.Launch();

        // Identify all partial classes that inherit (directly or indirectly) from SyntaxNode
        var partialClasses = context.SyntaxProvider
            .CreateSyntaxProvider(
                predicate: IsPotentialPartialClass,
                transform: GetClassSymbol)
            .Where(symbol => symbol is not null && InheritsFromSyntaxNode(symbol));

        // Generate source for each identified class
        context.RegisterSourceOutput(partialClasses, ProcessSyntaxNodeClass);
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

    private static bool InheritsFromSyntaxNode(ITypeSymbol classSymbol)
    {
        // Traverse the inheritance hierarchy to check if the type derives from SyntaxNode
        var baseType = classSymbol.BaseType;
        while (baseType != null)
        {
            if (baseType.Name == "SyntaxNode" && baseType.ContainingNamespace.ToDisplayString() == "Raven.CodeAnalysis.Syntax")
            {
                return true;
            }
            baseType = baseType.BaseType;
        }

        return false;
    }

    private static void ProcessSyntaxNodeClass(SourceProductionContext context, INamedTypeSymbol? classSymbol)
    {
        if (classSymbol is null)
            return;

        var namespaceName = classSymbol.ContainingNamespace.ToDisplayString();
        var className = classSymbol.Name;

        var syntaxNodePartialClass = GeneratePartialClass(context, classSymbol);

        var visitorPartialClass = VisitorPartialGenerator.GeneratePartialClassWithVisitMethodForVisitor(context, namespaceName, className);

        var visitorGenericPartialClass = VisitorPartialGenerator.GeneratePartialClassWithVisitMethodForGenericVisitor(context, namespaceName, className);

        var rewriterGenericPartialClass = VisitorPartialGenerator.GenerateVisitMethodForRewriter(context, classSymbol);

        // Wrap it in a namespace
        var namespaceDeclaration = FileScopedNamespaceDeclaration(ParseName(namespaceName))
            .AddMembers(
                syntaxNodePartialClass, visitorPartialClass,
                visitorGenericPartialClass, rewriterGenericPartialClass);

        // Convert to source text and add to the compilation
        var syntaxTree = CompilationUnit()
            .AddUsings(
                UsingDirective(ParseName("System")),
                UsingDirective(ParseName("Raven.CodeAnalysis.Syntax")))
            .AddMembers(namespaceDeclaration)
            .NormalizeWhitespace();

        context.AddSource($"{className}_Generated.cs", SourceText.From(syntaxTree.ToFullString(), Encoding.UTF8));
    }
}