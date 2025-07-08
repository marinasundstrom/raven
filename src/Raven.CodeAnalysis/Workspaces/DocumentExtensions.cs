using System.Threading.Tasks;

using Raven.CodeAnalysis.Syntax;
using Raven.CodeAnalysis.Text;

namespace Raven.CodeAnalysis;

public static class DocumentExtensions
{
    public static Task<SourceText> GetTextAsync(this Document document)
        => Task.FromResult(document.Text);

    public static Task<SyntaxTree> GetSyntaxTreeAsync(this Document document)
        => Task.FromResult(document.GetSyntaxTree());
}
