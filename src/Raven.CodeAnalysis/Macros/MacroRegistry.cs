using System;
using System.Collections.Generic;
using System.Collections.Immutable;

namespace Raven.CodeAnalysis.Macros;

internal sealed class MacroRegistry
{
    private static readonly DiagnosticDescriptor s_macroLoadFailed = DiagnosticDescriptor.Create(
        "RAVM001",
        "Macro plugin load failed",
        "",
        "",
        "Failed to load macro reference '{0}': {1}",
        "compiler",
        DiagnosticSeverity.Error,
        true);

    private static readonly DiagnosticDescriptor s_duplicateMacroName = DiagnosticDescriptor.Create(
        "RAVM002",
        "Duplicate macro name",
        "",
        "",
        "Macro '{0}' is exported by multiple plugins: '{1}' and '{2}'",
        "compiler",
        DiagnosticSeverity.Error,
        true);

    private readonly ImmutableDictionary<string, LoadedAttachedMacro> _attachedMacros;

    private MacroRegistry(
        ImmutableDictionary<string, LoadedAttachedMacro> attachedMacros,
        ImmutableArray<Diagnostic> diagnostics)
    {
        _attachedMacros = attachedMacros;
        Diagnostics = diagnostics;
    }

    public ImmutableArray<Diagnostic> Diagnostics { get; }

    public static MacroRegistry Create(IEnumerable<MacroReference> references)
    {
        var diagnostics = ImmutableArray.CreateBuilder<Diagnostic>();
        var attachedMacros = ImmutableDictionary.CreateBuilder<string, LoadedAttachedMacro>(StringComparer.Ordinal);

        foreach (var reference in references)
        {
            try
            {
                foreach (var plugin in reference.GetPlugins())
                {
                    foreach (var macro in plugin.GetMacros())
                    {
                        if (macro is not IAttachedDeclarationMacro attached)
                            continue;

                        if (attachedMacros.TryGetValue(attached.Name, out var existing))
                        {
                            diagnostics.Add(Diagnostic.Create(
                                s_duplicateMacroName,
                                Location.None,
                                attached.Name,
                                existing.Plugin.Name,
                                plugin.Name));
                            continue;
                        }

                        attachedMacros.Add(attached.Name, new LoadedAttachedMacro(plugin, attached));
                    }
                }
            }
            catch (Exception ex)
            {
                diagnostics.Add(Diagnostic.Create(s_macroLoadFailed, Location.None, reference.Display, ex.Message));
            }
        }

        return new MacroRegistry(attachedMacros.ToImmutable(), diagnostics.ToImmutable());
    }

    public bool TryResolveAttachedMacro(string macroName, out LoadedAttachedMacro macro)
        => _attachedMacros.TryGetValue(macroName, out macro);
}

internal readonly record struct LoadedAttachedMacro(IRavenMacroPlugin Plugin, IAttachedDeclarationMacro Macro);
