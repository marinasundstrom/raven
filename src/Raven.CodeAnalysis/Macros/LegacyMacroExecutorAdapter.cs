using System.Collections.Immutable;

namespace Raven.CodeAnalysis.Macros;

internal sealed class LegacyMacroExecutorAdapter : IMacroExecutor
{
    private readonly IMacroDefinition _definition;
    private readonly MacroDefinitionDescriptor _descriptor;
    private readonly ImmutableArray<MacroExecutorParameter> _parameters;

    public LegacyMacroExecutorAdapter(
        IMacroDefinition definition,
        MacroDefinitionDescriptor descriptor)
    {
        _definition = definition;
        _descriptor = descriptor;
        _parameters = descriptor.Parameters
            .Select(static parameter => new MacroExecutorParameter(
                parameter.Name,
                parameter.ParameterType,
                parameter.TypeDisplayName,
                GetParameterSource(parameter.Role),
                parameter.Ordinal,
                parameter.Ordinal,
                parameter.IsRequired,
                parameter.DefaultValueDisplay ?? string.Empty))
            .ToImmutableArray();
    }

    public string Namespace => _definition.Namespace;

    public string Name => _definition.Name;

    public string? Alias => _definition.Alias;

    public string? Documentation => _definition.Documentation;

    public DocumentationFormat DocumentationFormat => _definition.DocumentationFormat;

    public MacroInvocationTargets InvocationTargets => _descriptor.InvocationTargets;

    public bool AcceptsArguments => _descriptor.AcceptsArguments;

    public ImmutableArray<string> TypeParameters => [];

    public ImmutableArray<MacroExecutorParameter> Parameters => _parameters;

    public MacroApplicationKind ApplicationKind => _descriptor.ApplicationKind;

    public bool HasTokenBody => _descriptor.HasTokenBody;

    public MacroTarget Targets => _descriptor.AttachmentTargets;

    public MacroExecutionResult Expand(MacroExecutionContext context)
    {
        return (_definition, context.Context) switch
        {
            (IAttachedDeclarationMacro macro, AttachedMacroContext attachedContext) =>
                MacroExecutionResult.Attached(
                    MacroExpansionService.ExpandWithTypedParametersIfAvailable(
                        macro,
                        attachedContext,
                        context.Diagnostics)
                    ?? macro.Expand(attachedContext)
                    ?? MacroExpansionResult.Empty),
            (ITokenTreeMacro macro, TokenTreeMacroContext tokenTreeContext) =>
                MacroExecutionResult.Invocable(
                    MacroExpansionService.ExpandWithTypedParametersIfAvailable(
                        macro,
                        tokenTreeContext,
                        context.Diagnostics)
                    ?? macro.Expand(tokenTreeContext)
                    ?? InvocableMacroExpansionResult.Empty),
            (IInvocableMacro macro, InvocableMacroContext invocableContext) =>
                MacroExecutionResult.Invocable(
                    MacroExpansionService.ExpandWithTypedParametersIfAvailable(
                        macro,
                        invocableContext,
                        context.Diagnostics)
                    ?? macro.Expand(invocableContext)
                    ?? InvocableMacroExpansionResult.Empty),
            _ => throw new InvalidOperationException(
                $"Macro '{Name}' cannot execute with a {context.Context.GetType().Name} context."),
        };
    }

    private static MacroParameterSource GetParameterSource(MacroParameterRole role)
        => role switch
        {
            MacroParameterRole.SyntaxInput => MacroParameterSource.SyntaxInput,
            MacroParameterRole.Context => MacroParameterSource.Context,
            MacroParameterRole.TokenBody => MacroParameterSource.TokenBody,
            MacroParameterRole.AttachedTarget => MacroParameterSource.AttachedTarget,
            _ => MacroParameterSource.Value,
        };
}
