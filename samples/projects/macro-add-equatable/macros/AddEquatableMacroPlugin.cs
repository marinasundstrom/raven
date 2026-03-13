using System.Collections.Immutable;

using Raven.CodeAnalysis.Macros;
using Raven.CodeAnalysis.Syntax;

namespace SampleMacros;

public sealed class AddEquatableMacroPlugin : IRavenMacroPlugin
{
    public string Name => "SampleMacros.AddEquatable";

    public ImmutableArray<IMacroDefinition> GetMacros()
        => [new AddEquatableMacro()];
}

public sealed class AddEquatableMacro : IAttachedDeclarationMacro
{
    public string Name => "AddEquatable";

    public MacroKind Kind => MacroKind.AttachedDeclaration;

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

        return new MacroExpansionResult
        {
            IntroducedMembers = [method]
        };
    }
}
