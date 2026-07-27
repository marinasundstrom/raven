using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;

namespace Raven.CodeAnalysis.Macros;

internal sealed class MacroRegistry
{
    private static readonly DiagnosticDescriptor s_macroLoadFailed = DiagnosticDescriptor.Create(
        "RAVM001",
        "Macro reference load failed",
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
        "Macro '{0}' is exported by multiple references: '{1}' and '{2}'",
        "compiler",
        DiagnosticSeverity.Error,
        true);

    private static readonly DiagnosticDescriptor s_invalidMacroContract = DiagnosticDescriptor.Create(
        "RAVM004",
        "Invalid macro contract",
        "",
        "",
        "Macro definition '{0}' from '{1}' must implement exactly one supported macro category interface",
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

        foreach (var macro in DefaultMacroEnvironment.Macros)
            RegisterMacro(macro, "Raven.Compiler");

        foreach (var reference in references)
        {
            try
            {
                foreach (var macro in reference.Macros)
                    RegisterMacro(macro, reference.Display);
            }
            catch (Exception ex)
            {
                diagnostics.Add(Diagnostic.Create(s_macroLoadFailed, Location.None, reference.Display, ex.Message));
            }
        }

        return new MacroRegistry(attachedMacros.ToImmutable(), freestandingMacros.ToImmutable(), diagnostics.ToImmutable());

        void RegisterMacro(IMacroDefinition macro, string origin)
        {
            if (!MacroFacts.TryGetKind(macro, out _))
            {
                diagnostics.Add(Diagnostic.Create(
                    s_invalidMacroContract,
                    Location.None,
                    macro.GetType().FullName ?? macro.GetType().Name,
                    origin));
                return;
            }

            switch (macro)
            {
                case IAttachedDeclarationMacro attached:
                    if (attachedMacros.TryGetValue(attached.Name, out var existingAttached))
                    {
                        diagnostics.Add(Diagnostic.Create(
                            s_duplicateMacroName,
                            Location.None,
                            attached.Name,
                            existingAttached.Origin,
                            origin));
                        return;
                    }

                    attachedMacros.Add(attached.Name, new LoadedAttachedMacro(origin, attached));
                    break;

                case IFreestandingExpressionMacro freestanding:
                    if (freestandingMacros.TryGetValue(freestanding.Name, out var existingFreestanding))
                    {
                        diagnostics.Add(Diagnostic.Create(
                            s_duplicateMacroName,
                            Location.None,
                            freestanding.Name,
                            existingFreestanding.Origin,
                            origin));
                        return;
                    }

                    freestandingMacros.Add(freestanding.Name, new LoadedFreestandingMacro(origin, freestanding));
                    break;

                case ITokenTreeExpressionMacro tokenTree:
                    if (freestandingMacros.TryGetValue(tokenTree.Name, out var existingTokenTree))
                    {
                        diagnostics.Add(Diagnostic.Create(
                            s_duplicateMacroName,
                            Location.None,
                            tokenTree.Name,
                            existingTokenTree.Origin,
                            origin));
                        return;
                    }

                    freestandingMacros.Add(tokenTree.Name, new LoadedFreestandingMacro(origin, tokenTree));
                    break;
            }
        }
    }

    public bool TryResolveAttachedMacro(string macroName, out LoadedAttachedMacro macro)
        => _attachedMacros.TryGetValue(macroName, out macro);

    public bool TryResolveFreestandingMacro(string macroName, out LoadedFreestandingMacro macro)
        => _freestandingMacros.TryGetValue(macroName, out macro);

    public IEnumerable<IMacroDefinition> GetMacros(MacroKind kind)
        => kind switch
        {
            MacroKind.AttachedDeclaration => _attachedMacros.Values.Select(static loaded => (IMacroDefinition)loaded.Macro),
            MacroKind.FreestandingExpression => _freestandingMacros.Values.Select(static loaded => loaded.Macro),
            _ => []
        };
}

internal readonly record struct LoadedAttachedMacro(string Origin, IAttachedDeclarationMacro Macro);
internal readonly record struct LoadedFreestandingMacro(string Origin, IMacroDefinition Macro);
