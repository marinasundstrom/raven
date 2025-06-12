namespace Generator;

using System.Diagnostics;
using System.Linq;
using System.Text;

using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using Microsoft.CodeAnalysis.Text;

using static Microsoft.CodeAnalysis.CSharp.SyntaxFactory;

[Generator]
public partial class SymbolVisitorPartialGenerator : IIncrementalGenerator
{
    public void Initialize(IncrementalGeneratorInitializationContext context)
    {
        // Identify all partial classes that inherit (directly or indirectly) from Symbol
        var partialClasses = context.SyntaxProvider
            .CreateSyntaxProvider(
                predicate: IsPotentialSymbolInterface,
                transform: GetInterfaceSymbol)
            .Where(symbol => symbol is not null && IsNotISymbol(symbol) && IsNotReserved(symbol) && ImplementsISymbol(symbol));

        // Generate source for each identified class
        context.RegisterSourceOutput(partialClasses, ProcessSymbolClass);
    }

    private bool IsNotISymbol(INamedTypeSymbol symbol)
    {
        return symbol.Name != "ISymbol";
    }

    string[] reservedNames = new[]
    {
        "INamespaceOrTypeSymbol",
        "ITypeSymbol",
    };

    private bool IsNotReserved(INamedTypeSymbol symbol)
    {
        return !reservedNames.Contains(symbol.Name);
    }

    private static bool IsPotentialSymbolInterface(SyntaxNode node, CancellationToken cancellationToken)
    {

        if (node is InterfaceDeclarationSyntax interfaceDecl)
        {
            return interfaceDecl.Modifiers.Any(m => m.IsKind(SyntaxKind.PublicKeyword));
        }
        return false;
    }

    private static INamedTypeSymbol? GetInterfaceSymbol(GeneratorSyntaxContext context, CancellationToken cancellationToken)
    {
        if (context.Node is InterfaceDeclarationSyntax interfaceDecl &&
            context.SemanticModel.GetDeclaredSymbol(interfaceDecl, cancellationToken) is INamedTypeSymbol classSymbol)
        {
            return classSymbol;
        }

        return null;
    }

    private static bool ImplementsISymbol(ITypeSymbol interfaceSymbol)
    {
        // Check if the interface itself is ISymbol
        if (interfaceSymbol.TypeKind is TypeKind.Interface &&
            interfaceSymbol.Name == "ISymbol" &&
            interfaceSymbol.ContainingNamespace.ToDisplayString() == "Raven.CodeAnalysis")
        {
            return true;
        }

        // Check all inherited interfaces
        foreach (var iface in interfaceSymbol.AllInterfaces)
        {
            if (iface.Name == "ISymbol" &&
                iface.ContainingNamespace.ToDisplayString() == "Raven.CodeAnalysis")
            {
                return true;
            }
        }

        return false;
    }

    private static void ProcessSymbolClass(SourceProductionContext context, INamedTypeSymbol? interfaceSymbol)
    {
        if (interfaceSymbol is null)
            return;

        var namespaceName = interfaceSymbol.ContainingNamespace.ToDisplayString();
        var interfaceName = interfaceSymbol.Name;

        var visitorPartialClass = VisitorPartialGenerator.GeneratePartialClassWithVisitMethodForVisitor(new VisitorPartialGeneratorOptions(interfaceSymbol, nodeTypeNamePrefix: "Symbol", resultType: "ISymbol"));

        var visitorGenericPartialClass = VisitorPartialGenerator.GeneratePartialClassWithVisitMethodForGenericVisitor(new VisitorPartialGeneratorOptions(interfaceSymbol, nodeTypeNamePrefix: "Symbol", resultType: "ISymbol"));

        //var rewriterGenericPartialClass = VisitorPartialGenerator.GenerateVisitMethodForRewriter(suffix: "Symbol", resultType: "ISymbol");

        // Wrap it in a namespace
        var namespaceDeclaration = FileScopedNamespaceDeclaration(ParseName(namespaceName))
                                                            .AddMembers(
                                                                visitorPartialClass,
                                                                visitorGenericPartialClass);

        // Convert to source text and add to the compilation
        var syntaxTree = CompilationUnit()
            .AddUsings(
                UsingDirective(ParseName("System")),
                UsingDirective(ParseName("Raven.CodeAnalysis.Symbols")))
            .AddMembers(namespaceDeclaration)
            .NormalizeWhitespace();

        context.AddSource($"{interfaceName}_Generated.cs", SourceText.From(syntaxTree.ToFullString(), Encoding.UTF8));
    }
}