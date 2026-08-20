using System.Collections.Immutable;

using Raven.CodeAnalysis;
using Raven.CodeAnalysis.Macros;
using Raven.CodeAnalysis.Syntax;
using Raven.CodeAnalysis.Text;

namespace Raven.LanguageServer.Tests;

internal sealed class DeclarationFragmentMacro : IMacroDefinition, IMacroFragmentProvider
{
    public string Namespace => string.Empty;

    public string Name => "FunctionComponent";

    public string? Alias => "component";

    public MacroInvocationTargets InvocationTargets =>
        MacroInvocationTargets.NamespaceMember | MacroInvocationTargets.TypeMember;

    public ImmutableArray<MacroFragmentRegion> GetFragmentRegions(TokenTreeMacroContext context)
    {
        var declaration = (FreestandingMacroDeclarationSyntax)context.Syntax;
        var parameter = declaration.ParameterList!.Parameters[0];
        var local = context.CreateFragmentParameter(
            parameter.Identifier.ValueText,
            context.Compilation.GetSpecialType(SpecialType.System_String),
            parameter.Identifier.Span);
        return
        [
            context.CreateFragmentRegion(
                MacroFragmentKind.Block,
                new TextSpan(0, context.BodySpan.Length),
                [local])
        ];
    }

    public MemberDeclarationSyntax Expand(
        FreestandingMacroDeclarationSyntax declaration,
        TokenTreeMacroContext context)
        => SyntaxFactory.ParseMemberDeclaration(
            $"class {declaration.Identifier.ValueText} {{ }}")!;
}
