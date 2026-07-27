using Raven.CodeAnalysis.Macros;
using Raven.CodeAnalysis.Syntax;

[assembly: RavenCompilerPlugin(typeof(SampleMacros.AddEquatableMacro))]

namespace SampleMacros;

public sealed class AddEquatableMacro : IAttachedDeclarationMacro
{
    public string Name => "AddEquatable";

    public MacroTarget Targets => MacroTarget.Type;

    public MacroExpansionResult Expand(AttachedMacroContext context)
    {
        if (context.CurrentDeclaration is not ClassDeclarationSyntax current)
        {
            return MacroExpansionResult.FromDiagnostic(
                context.CreateDiagnostic("AddEquatable requires a class declaration."));
        }

        var typeName = current.Identifier.ValueText;
        var tree = SyntaxFactory.ParseSyntaxTree($$"""
            class __GeneratedContainer : System.IEquatable<{{typeName}}> {
                func Equals(other: {{typeName}}) -> bool {
                    return Name == other.Name && Age == other.Age
                }
            }
            """);

        var container = (ClassDeclarationSyntax)tree.GetRoot().Members[0];
        var method = (MethodDeclarationSyntax)container.Members[0];
        var equatableType = container.BaseList!.Types[0];
        var baseList = AddBaseType(current.BaseList, equatableType);

        return MacroExpansionResult.FromReplacement(
            current.WithBaseList(baseList),
            [method]);
    }

    private static BaseListSyntax AddBaseType(BaseListSyntax? existing, BaseTypeSyntax baseType)
    {
        if (existing is null)
            return SyntaxFactory.BaseList(SyntaxFactory.SingletonSeparatedList<BaseTypeSyntax>(baseType));

        var items = existing.Types.GetWithSeparators().ToList();
        items.Add(SyntaxFactory.Token(SyntaxKind.CommaToken));
        items.Add(baseType);
        return existing.WithTypes(SyntaxFactory.SeparatedList<BaseTypeSyntax>([.. items]));
    }
}
