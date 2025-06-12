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
public partial class BoundNodeVisitorPartialGenerator : IIncrementalGenerator
{
    public void Initialize(IncrementalGeneratorInitializationContext context)
    {
        // Identify all partial classes that inherit (directly or indirectly) from Symbol
        var partialClasses = context.SyntaxProvider
            .CreateSyntaxProvider(
                predicate: IsPotentialPartialClass,
                transform: GetClassSymbol)
            .Where(symbol => symbol is not null && symbol.InheritsFromBoundNode() && IsNotReserved(symbol));

        // Generate source for each identified class
        context.RegisterSourceOutput(partialClasses, ProcessBoundNodeClass);
    }

    readonly string[] reservedNames = [];

    private bool IsNotReserved(INamedTypeSymbol symbol)
    {
        return !reservedNames.Contains(symbol.Name);
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

    private static void ProcessBoundNodeClass(SourceProductionContext context, INamedTypeSymbol? classSymbol)
    {
        if (classSymbol is null)
            return;

        var namespaceName = classSymbol.ContainingNamespace.ToDisplayString();
        var className = classSymbol.Name;

        var visitorPartialClass = VisitorPartialGenerator.GeneratePartialClassWithVisitMethodForVisitor(new VisitorPartialGeneratorOptions(classSymbol, nodeTypeNamePrefix: "Bound", visitorClassNamePrefix: "BoundTree", resultType: "BoundNode", isInternal: true));

        var visitorGenericPartialClass = VisitorPartialGenerator.GeneratePartialClassWithVisitMethodForGenericVisitor(new VisitorPartialGeneratorOptions(classSymbol, nodeTypeNamePrefix: "Bound", visitorClassNamePrefix: "BoundTree", resultType: "BoundNode", isInternal: true));

        var rewriterGenericPartialClass = VisitorPartialGenerator.GenerateVisitMethodForRewriter(new RewriterPartialGeneratorOptions(classSymbol, GenerateBoundTreeUpdateMethodImpl, nodeTypeNamePrefix: "Bound", rewriterClassNamePrefix: "BoundTree", resultType: "BoundNode", isInternal: true));

        // Wrap it in a namespace
        var namespaceDeclaration = FileScopedNamespaceDeclaration(ParseName(namespaceName))
            .AddMembers(
                visitorPartialClass,
                visitorGenericPartialClass,
                rewriterGenericPartialClass);

        // Convert to source text and add to the compilation
        var syntaxTree = CompilationUnit()
            .AddUsings(
                UsingDirective(ParseName("System")),
                UsingDirective(ParseName("Raven.CodeAnalysis")))
            .AddMembers(namespaceDeclaration)
            .NormalizeWhitespace();

        context.AddSource($"{className}_Generated.cs", SourceText.From(syntaxTree.ToFullString(), Encoding.UTF8));
    }

    private static ExpressionSyntax GenerateBoundTreeUpdateMethodImpl(RewriterPartialGeneratorOptions options)
    {
        ExpressionSyntax expr;

        var parameters = options.TypeSymbol.Constructors.First().Parameters
            //.Where(x => x.Type.InheritsFromBoundNode() || x.Type.IsImplementingISymbol())
            .Select(x => new PropOrParamType(x.Name, x.Type.ToDisplayString(), FormatName(x), x.Type));

        var args = parameters.Select(property =>
        {
            var accessExpr = MemberAccessExpression(SyntaxKind.SimpleMemberAccessExpression,
                    IdentifierName(options.NodeParamName),
                    Token(SyntaxKind.DotToken),
                    IdentifierName(property.TargetName));

            if (!property.TypeSymbol.InheritsFromBoundNode() && !property.TypeSymbol.IsImplementingISymbol())
            {
                return Argument(accessExpr);
            }

            var isList = property.Type.Contains("[]") ||
                         property.Type.Contains("List") ||
                         property.Type.Contains("IEnumerable") ||
                         property.Type.Contains("ImmutableArray");

            var propertyType = ParseTypeName(property.Type);

            string? t = null;

            var childPropertyTypeNae = propertyType.DescendantNodes()
                .OfType<IdentifierNameSyntax>()
                .LastOrDefault();

            t = childPropertyTypeNae?.Identifier.Text;

            //return Argument(IdentifierName(t?.ToString() ?? "Foo"));

            if (t is null) return Argument(IdentifierName(property.TargetName));

            string updateMethodNameStr = null!;

            if (isList)
            {
                updateMethodNameStr = $"VisitList";
            }
            else if (t.Contains("Bound"))
            {
                updateMethodNameStr = $"Visit{t.Replace("Bound", string.Empty)}";
            }
            else
            {
                updateMethodNameStr = t switch
                {
                    "INamespaceSymbol" => "VisitNamespace",
                    "ITypeSymbol" => "VisitType",
                    "IMethodSymbol" => "VisitMethod",
                    "IPropertySymbol" => "VisitProperty",
                    "IFieldSymbol" => "VisitField",
                    "ILocalSymbol" => "VisitLocal",
                    "ISymbol" => "VisitSymbol",
                    _ => t
                };
            }

            NameSyntax updateMethodName = IdentifierName(updateMethodNameStr);

            var updateInvocation = InvocationExpression(updateMethodName, ArgumentList([
                Argument(accessExpr)
            ]));

            var castingResult = CastExpression(ParseTypeName(property.Type), updateInvocation);

            return Argument(castingResult);
        }).ToList();

        expr = InvocationExpression(
            ConditionalAccessExpression(IdentifierName(options.NodeParamName), MemberBindingExpression(IdentifierName("Update"))))
                    .WithArgumentList(
                        ArgumentList(
                            SeparatedList(
                                args)));

        return expr;

        //return IdentifierName(options.NodeParamName);
    }

    private static string FormatName(IParameterSymbol x)
    {
        return char.ToUpper(x.Name[0]) + x.Name.Substring(1);
    }
}