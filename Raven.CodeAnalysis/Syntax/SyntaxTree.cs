using System.Diagnostics.CodeAnalysis;
using System.Runtime.CompilerServices;

using Raven.CodeAnalysis.Syntax;

[assembly: InternalsVisibleTo("Test")]
[assembly: InternalsVisibleTo("Raven.Tests")]

namespace Raven.CodeAnalysis.Syntax;

public class SyntaxTree
{
    private CompilationUnitSyntax _compilationUnit;
    private SourceText? _sourceText;

    private SyntaxTree()
    {

    }

    private SyntaxTree(SourceText sourceText)
    {
        _sourceText = sourceText;
    }


    public CompilationUnitSyntax GetSyntaxRoot() { return _compilationUnit; }

    public static SyntaxTree ParseText(string text)
    {
        var source = new SourceText(text);

        // TODO: Invoke parser

        return new SyntaxTree(source);
    }

    public IEnumerable<Diagnostic> GetDiagnostics(CancellationToken cancellationToken = default)
    {
        return null;
    }

    public IEnumerable<Diagnostic> GetDiagnostics(SyntaxNodeOrToken syntaxNodeOrToken)
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

    public Location GetLocation(TextSpan span)
    {
        var sourceText = GetText();

        if (sourceText is null)
            throw new Exception();

        var (line, column) = sourceText.GetLineAndColumn(span);
        return new Location(line, column);
    }

    public SourceText? GetText()
    {
        return _sourceText;
    }

    public bool TryGetText([NotNullWhen(true)] out SourceText? text)
    {
        if (_sourceText is not null)
        {
            text = new SourceText("");
            return true;
        }
        text = null;
        return false;
    }
    }
}