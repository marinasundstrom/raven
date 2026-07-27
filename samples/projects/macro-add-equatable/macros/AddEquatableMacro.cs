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
        var tree = SyntaxFactory.ParseSyntaxTree("""
            class __GeneratedContainer {
                func GeneratedEqualsMarker() -> bool { return true }
            }
            """);

        var container = (ClassDeclarationSyntax)tree.GetRoot().Members[0];
        var method = (MethodDeclarationSyntax)container.Members[0];

        return MacroExpansionResult.FromIntroducedMembers([method]);
    }
}
