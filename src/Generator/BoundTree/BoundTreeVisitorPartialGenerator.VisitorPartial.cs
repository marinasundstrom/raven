namespace Generator;

using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;

using static Microsoft.CodeAnalysis.CSharp.SyntaxFactory;

public partial class BoundNodeVisitorPartialGenerator : IIncrementalGenerator
{
    private static ClassDeclarationSyntax GenerateVisitorPartialClass(SourceProductionContext context, INamedTypeSymbol? classSymbol)
    {
        var namespaceName = classSymbol.ContainingNamespace.ToDisplayString();
        var className = classSymbol.Name;

        return VisitorPartialGenerator.GeneratePartialClassWithVisitMethodForVisitor(context, namespaceName, className, suffix: "Bound", visitorClassName: "BoundTree", resultType: "BoundNode", isInternal: true);
    }
}