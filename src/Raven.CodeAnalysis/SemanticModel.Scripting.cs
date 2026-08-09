using System.Collections.Immutable;

using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis;

public partial class SemanticModel
{
    /// <summary>
    /// Binds and returns the declarations contributed to script continuation state by this tree.
    /// </summary>
    internal ImmutableArray<ISymbol> GetSubmissionDeclarations()
    {
        if (!Compilation.IsSubmission)
            return ImmutableArray<ISymbol>.Empty;

        using var semanticAccess = EnterSemanticAccess(CancellationToken.None);
        using var semanticQueryBinding = EnterSemanticQueryBinding();

        EnsureBindingReady();
        Compilation.EnsureSourceDeclarationsComplete();

        var root = SyntaxTree.GetRoot();
        EnsureTopLevelFunctionDeclarations(root);
        EnsureTopLevelCompilationUnitBound(root);

        var declarations = ImmutableArray.CreateBuilder<ISymbol>();
        foreach (var global in Compilation.GetBindableGlobalStatements(root))
        {
            if (global.Statement is not LocalDeclarationStatementSyntax localDeclaration)
                continue;

            foreach (var declarator in localDeclaration.Declaration.Declarators)
            {
                if (GetDeclaredSymbol(declarator) is ILocalSymbol local)
                    declarations.Add(local);
            }
        }

        foreach (var global in root.DescendantNodes().OfType<GlobalStatementSyntax>())
        {
            if (!Compilation.IsTopLevelFunctionMember(global) ||
                global.Statement is not FunctionStatementSyntax function)
            {
                continue;
            }

            if (GetDeclaredSymbol(function) is IMethodSymbol method)
                declarations.Add(method);
        }

        return declarations.ToImmutable();
    }
}
