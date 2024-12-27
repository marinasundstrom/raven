using Raven.CodeAnalysis;
using Raven.CodeAnalysis.Syntax;

static class Test
{
    public static void ListNamespaces(Compilation compilation)
    {
        var globalNamespace = compilation.GlobalNamespace.GetMembers("System").First() as INamespaceSymbol;

        foreach (var member in globalNamespace!.GetMembers().OfType<INamespaceSymbol>())
        {
            Console.WriteLine(member.ToDisplayString(SymbolDisplayFormat.FullyQualifiedFormat));
        }
    }

    public static void GetSymbol(Compilation compilation)
    {
        var syntaxTree = compilation.SyntaxTrees.First();

        var semanticModel = compilation.GetSemanticModel(syntaxTree);

        var variableDeclarator = syntaxTree.GetRoot()
            .DescendantNodes()
            .OfType<VariableDeclaratorSyntax>()
            .First();

        var loc = variableDeclarator.GetLocation();

        var symbol = semanticModel.GetSymbolInfo(variableDeclarator).Symbol as ILocalSymbol;

        Console.WriteLine(symbol!.ContainingSymbol!.ToDisplayString(SymbolDisplayFormat.FullyQualifiedFormat));
    }
}
