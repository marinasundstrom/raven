using System.Runtime.CompilerServices;

using Raven.CodeAnalysis.Syntax;

[assembly: InternalsVisibleTo("Test")]
[assembly: InternalsVisibleTo("Raven.Tests")]

namespace Raven.CodeAnalysis.Syntax;

public class SyntaxTree
{
    private CompilationUnitSyntax _compilationUnit;

    public SyntaxTree()
    {

    }

    public CompilationUnitSyntax GetSyntaxRoot() { return _compilationUnit; }

    public static SyntaxTree ParseText(string text)
    {
        return null!;
    }

    public CompilationUnitSyntax GetCompilationUnitRoot()
    {
        return null!;
    }

    public IEnumerable<Diagnostic> GetDiagnostics(CancellationToken cancellationToken = default)
    {
        return null;
    }

    public IEnumerable<Diagnostic> GetDiagnostics(SyntaxNodeOrToken syntaxNodeOrTokent)
    {
        return null;
    }

    public object GetChanges(SyntaxTree syntaxTree)
    {
        return null;
    }

    public static SyntaxTree Create(CompilationUnitSyntax compilationUnit)
    {
        var syntaxTree = new SyntaxTree();

        compilationUnit = compilationUnit
            .WithRoot(syntaxTree);

        syntaxTree.AddSyntaxTree(compilationUnit);

        return syntaxTree;
    }

    private void AddSyntaxTree(CompilationUnitSyntax compilationUnit)
    {
        _compilationUnit = compilationUnit;
    }
}
