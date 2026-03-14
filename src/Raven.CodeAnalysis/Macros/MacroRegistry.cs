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
    private readonly ImmutableDictionary<string, LoadedFreestandingMacro> _freestandingMacros;

    private MacroRegistry(
        ImmutableDictionary<string, LoadedAttachedMacro> attachedMacros,
        ImmutableDictionary<string, LoadedFreestandingMacro> freestandingMacros,
        ImmutableArray<Diagnostic> diagnostics)
    {
        _attachedMacros = attachedMacros;
        _freestandingMacros = freestandingMacros;
        Diagnostics = diagnostics;
    }

    public ImmutableArray<Diagnostic> Diagnostics { get; }

    public static MacroRegistry Create(IEnumerable<MacroReference> references)
    {
        var diagnostics = ImmutableArray.CreateBuilder<Diagnostic>();
        var attachedMacros = ImmutableDictionary.CreateBuilder<string, LoadedAttachedMacro>(StringComparer.Ordinal);
        var freestandingMacros = ImmutableDictionary.CreateBuilder<string, LoadedFreestandingMacro>(StringComparer.Ordinal);

        foreach (var reference in references)
        {
            try
            {
                foreach (var plugin in reference.GetPlugins())
                {
                    foreach (var macro in plugin.GetMacros())
                    {
                        switch (macro)
                        {
                            case IAttachedDeclarationMacro attached:
                                if (attachedMacros.TryGetValue(attached.Name, out var existingAttached))
                                {
                                    diagnostics.Add(Diagnostic.Create(
                                        s_duplicateMacroName,
                                        Location.None,
                                        attached.Name,
                                        existingAttached.Plugin.Name,
                                        plugin.Name));
                                    continue;
                                }

                                attachedMacros.Add(attached.Name, new LoadedAttachedMacro(plugin, attached));
                                break;

                            case IFreestandingExpressionMacro freestanding:
                                if (freestandingMacros.TryGetValue(freestanding.Name, out var existingFreestanding))
                                {
                                    diagnostics.Add(Diagnostic.Create(
                                        s_duplicateMacroName,
                                        Location.None,
                                        freestanding.Name,
                                        existingFreestanding.Plugin.Name,
                                        plugin.Name));
                                    continue;
                                }

                                freestandingMacros.Add(freestanding.Name, new LoadedFreestandingMacro(plugin, freestanding));
                                break;
                        }
                    }
                }
            }
            catch (Exception ex)
            {
                diagnostics.Add(Diagnostic.Create(s_macroLoadFailed, Location.None, reference.Display, ex.Message));
            }
        }

        return new MacroRegistry(attachedMacros.ToImmutable(), freestandingMacros.ToImmutable(), diagnostics.ToImmutable());
    }

    public bool TryResolveAttachedMacro(string macroName, out LoadedAttachedMacro macro)
        => _attachedMacros.TryGetValue(macroName, out macro);

    public bool TryResolveFreestandingMacro(string macroName, out LoadedFreestandingMacro macro)
        => _freestandingMacros.TryGetValue(macroName, out macro);
}

internal readonly record struct LoadedAttachedMacro(IRavenMacroPlugin Plugin, IAttachedDeclarationMacro Macro);
internal readonly record struct LoadedFreestandingMacro(IRavenMacroPlugin Plugin, IFreestandingExpressionMacro Macro);
