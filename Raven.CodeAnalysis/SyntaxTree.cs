using System.Runtime.CompilerServices;

[assembly: InternalsVisibleTo("Test")]
[assembly: InternalsVisibleTo("Raven.Tests")]

namespace Raven.CodeAnalysis;

public class SyntaxTree
{
    public CompilationUnit GetSyntaxRoot() { return null!; }
}
