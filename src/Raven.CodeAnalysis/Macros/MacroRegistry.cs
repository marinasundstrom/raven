using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;

using Raven.CodeAnalysis.Symbols;
using Raven.CodeAnalysis.Syntax;

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

    public IEnumerable<string> Namespaces =>
        _attachedMacros.Values
            .Select(static macro => GetNamespace(macro.CanonicalName))
            .Concat(_freestandingMacros.Values.Select(static macro => GetNamespace(macro.CanonicalName)))
            .Where(static macroNamespace => !string.IsNullOrEmpty(macroNamespace))
            .Distinct(StringComparer.Ordinal);

    public static MacroRegistry Create(IEnumerable<MacroReference> references)
    {
        var diagnostics = ImmutableArray.CreateBuilder<Diagnostic>();
        var attachedMacros = ImmutableDictionary.CreateBuilder<string, LoadedAttachedMacro>(StringComparer.Ordinal);
        var freestandingMacros = ImmutableDictionary.CreateBuilder<string, LoadedFreestandingMacro>(StringComparer.Ordinal);

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
                    var attachedName = GetCanonicalName(attached);
                    if (attachedMacros.TryGetValue(attachedName, out var existingAttached))
                    {
                        diagnostics.Add(Diagnostic.Create(
                            s_duplicateMacroName,
                            Location.None,
                            attachedName,
                            existingAttached.Origin,
                            origin));
                        return;
                    }

                    attachedMacros.Add(
                        attachedName,
                        new LoadedAttachedMacro(
                            origin,
                            attached,
                            attachedName,
                            GetAliases(attached)));
                    break;

                case IFreestandingExpressionMacro freestanding:
                    var freestandingName = GetCanonicalName(freestanding);
                    if (freestandingMacros.TryGetValue(freestandingName, out var existingFreestanding))
                    {
                        diagnostics.Add(Diagnostic.Create(
                            s_duplicateMacroName,
                            Location.None,
                            freestandingName,
                            existingFreestanding.Origin,
                            origin));
                        return;
                    }

                    freestandingMacros.Add(
                        freestandingName,
                        new LoadedFreestandingMacro(
                            origin,
                            freestanding,
                            freestandingName,
                            GetAliases(freestanding)));
                    break;

                case ITokenTreeExpressionMacro tokenTree:
                    var tokenTreeName = GetCanonicalName(tokenTree);
                    if (freestandingMacros.TryGetValue(tokenTreeName, out var existingTokenTree))
                    {
                        diagnostics.Add(Diagnostic.Create(
                            s_duplicateMacroName,
                            Location.None,
                            tokenTreeName,
                            existingTokenTree.Origin,
                            origin));
                        return;
                    }

                    freestandingMacros.Add(
                        tokenTreeName,
                        new LoadedFreestandingMacro(
                            origin,
                            tokenTree,
                            tokenTreeName,
                            GetAliases(tokenTree)));
                    break;
            }
        }
    }

    public bool TryResolveAttachedMacro(
        Compilation compilation,
        SyntaxNode context,
        string macroName,
        out LoadedAttachedMacro macro,
        out bool isAmbiguous)
        => TryResolveMacro(
            compilation,
            context,
            macroName,
            _attachedMacros,
            static loaded => loaded.CanonicalName,
            static loaded => loaded.Aliases,
            out macro,
            out isAmbiguous);

    public bool TryResolveFreestandingMacro(
        Compilation compilation,
        SyntaxNode context,
        string macroName,
        out LoadedFreestandingMacro macro,
        out bool isAmbiguous)
        => TryResolveMacro(
            compilation,
            context,
            macroName,
            _freestandingMacros,
            static loaded => loaded.CanonicalName,
            static loaded => loaded.Aliases,
            out macro,
            out isAmbiguous);

    public IEnumerable<IMacroDefinition> GetMacros(MacroKind kind)
        => kind switch
        {
            MacroKind.AttachedDeclaration => _attachedMacros.Values.Select(static loaded => (IMacroDefinition)loaded.Macro),
            MacroKind.FreestandingExpression => _freestandingMacros.Values.Select(static loaded => loaded.Macro),
            _ => []
        };

    public bool TryResolveMacroSymbol(
        Compilation compilation,
        SyntaxNode context,
        string macroName,
        out IMacroSymbol symbol,
        out bool isAmbiguous)
    {
        var hasAttached = TryResolveAttachedMacro(
            compilation,
            context,
            macroName,
            out var attached,
            out var attachedAmbiguous);
        var hasFreestanding = TryResolveFreestandingMacro(
            compilation,
            context,
            macroName,
            out var freestanding,
            out var freestandingAmbiguous);

        isAmbiguous = attachedAmbiguous || freestandingAmbiguous || (hasAttached && hasFreestanding);
        if (isAmbiguous || (!hasAttached && !hasFreestanding))
        {
            symbol = null!;
            return false;
        }

        var canonicalName = hasAttached
            ? attached.CanonicalName
            : freestanding.CanonicalName;
        var aliases = hasAttached
            ? attached.Aliases
            : freestanding.Aliases;
        var definition = hasAttached
            ? attached.Macro
            : freestanding.Macro;
        var resolvedName = IsQualifiedName(macroName)
            ? GetSimpleName(canonicalName)
            : macroName;
        var macroNamespace = GetNamespace(canonicalName);
        var containingNamespace = string.IsNullOrEmpty(macroNamespace)
            ? compilation.GetSourceGlobalNamespace()
            : compilation.GetOrCreateNamespaceSymbol(macroNamespace)
                ?? compilation.GetSourceGlobalNamespace();

        symbol = new SynthesizedMacroSymbol(
            resolvedName,
            canonicalName,
            aliases,
            definition,
            containingNamespace);
        return true;
    }

    public IEnumerable<(string Name, IMacroDefinition Macro)> GetVisibleMacros(
        Compilation compilation,
        SyntaxNode context,
        MacroKind kind)
    {
        return kind switch
        {
            MacroKind.AttachedDeclaration => GetVisibleMacros(
                compilation,
                context,
                _attachedMacros.Values,
                static loaded => loaded.Macro,
                static loaded => loaded.CanonicalName,
                static loaded => loaded.Aliases),
            MacroKind.FreestandingExpression => GetVisibleMacros(
                compilation,
                context,
                _freestandingMacros.Values,
                static loaded => loaded.Macro,
                static loaded => loaded.CanonicalName,
                static loaded => loaded.Aliases),
            _ => []
        };
    }

    private static string GetCanonicalName(IMacroDefinition macro)
        => string.IsNullOrEmpty(macro.Namespace)
            ? macro.Name
            : $"{macro.Namespace}.{macro.Name}";

    private static ImmutableArray<string> GetAliases(IMacroDefinition macro)
        => new[] { macro.Alias }
            .Concat(macro.GetType()
                .GetCustomAttributes(typeof(MacroAliasAttribute), inherit: false)
                .OfType<MacroAliasAttribute>()
                .Select(static attribute => attribute.Alias))
            .Where(static alias => !string.IsNullOrWhiteSpace(alias))
            .Select(static alias => alias!)
            .Distinct(StringComparer.Ordinal)
            .ToImmutableArray();

    private static bool TryResolveMacro<TMacro>(
        Compilation compilation,
        SyntaxNode context,
        string macroName,
        ImmutableDictionary<string, TMacro> macros,
        Func<TMacro, string> getCanonicalName,
        Func<TMacro, ImmutableArray<string>> getAliases,
        out TMacro macro,
        out bool isAmbiguous)
    {
        if (IsQualifiedName(macroName))
        {
            isAmbiguous = false;
            return macros.TryGetValue(macroName, out macro!);
        }

        var matches = macros.Values
            .Where(candidate =>
                IsNamespaceInScope(
                    compilation,
                    context,
                    GetNamespace(getCanonicalName(candidate))) &&
                (string.Equals(GetSimpleName(getCanonicalName(candidate)), macroName, StringComparison.Ordinal) ||
                 getAliases(candidate).Contains(macroName, StringComparer.Ordinal)))
            .Take(2)
            .ToArray();
        isAmbiguous = matches.Length > 1;
        macro = matches.Length == 1 ? matches[0] : default!;
        return matches.Length == 1;
    }

    private static IEnumerable<(string Name, IMacroDefinition Macro)> GetVisibleMacros<TMacro>(
        Compilation compilation,
        SyntaxNode context,
        IEnumerable<TMacro> macros,
        Func<TMacro, IMacroDefinition> getMacro,
        Func<TMacro, string> getCanonicalName,
        Func<TMacro, ImmutableArray<string>> getAliases)
    {
        foreach (var loaded in macros)
        {
            var canonicalName = getCanonicalName(loaded);
            if (!IsNamespaceInScope(compilation, context, GetNamespace(canonicalName)))
                continue;

            yield return (GetSimpleName(canonicalName), getMacro(loaded));
            foreach (var alias in getAliases(loaded))
                yield return (alias, getMacro(loaded));
        }
    }

    private static bool IsNamespaceInScope(
        Compilation compilation,
        SyntaxNode context,
        string macroNamespace)
    {
        if (string.IsNullOrEmpty(macroNamespace))
            return true;

        var currentNamespace = GetCurrentNamespace(context);
        if (string.Equals(currentNamespace, macroNamespace, StringComparison.Ordinal))
            return true;

        foreach (var import in EnumerateEffectiveImports(compilation, context))
        {
            var importedName = import.Name.ToString();
            if (importedName.EndsWith(".*", StringComparison.Ordinal))
                importedName = importedName[..^2];

            if (string.Equals(importedName, macroNamespace, StringComparison.Ordinal) ||
                (!string.IsNullOrEmpty(currentNamespace) &&
                 string.Equals(
                     $"{currentNamespace}.{importedName}",
                     macroNamespace,
                     StringComparison.Ordinal)))
            {
                return true;
            }
        }

        return false;
    }

    private static IEnumerable<ImportDirectiveSyntax> EnumerateEffectiveImports(
        Compilation compilation,
        SyntaxNode context)
    {
        if (context.SyntaxTree?.GetRoot() is CompilationUnitSyntax root)
        {
            foreach (var import in root.Imports)
                yield return import;
        }

        foreach (var namespaceDeclaration in context.AncestorsAndSelf()
                     .OfType<BaseNamespaceDeclarationSyntax>())
        {
            foreach (var import in namespaceDeclaration.Imports)
                yield return import;
        }

        foreach (var syntaxTree in compilation.SyntaxTrees)
        {
            foreach (var globalImport in syntaxTree.GetRoot()
                         .Members
                         .OfType<GlobalImportBlockSyntax>())
            {
                foreach (var import in globalImport.Imports)
                    yield return import;
            }
        }
    }

    private static string GetCurrentNamespace(SyntaxNode context)
    {
        var names = context.AncestorsAndSelf()
            .OfType<BaseNamespaceDeclarationSyntax>()
            .Reverse()
            .Select(static declaration => declaration.Name.ToString())
            .Where(static name => !string.IsNullOrWhiteSpace(name));
        return string.Join(".", names);
    }

    private static bool IsQualifiedName(string name)
        => name.Contains('.', StringComparison.Ordinal) ||
           name.Contains("::", StringComparison.Ordinal);

    private static string GetNamespace(string canonicalName)
    {
        var separator = canonicalName.LastIndexOf('.');
        return separator < 0 ? string.Empty : canonicalName[..separator];
    }

    private static string GetSimpleName(string canonicalName)
    {
        var separator = canonicalName.LastIndexOf('.');
        return separator < 0 ? canonicalName : canonicalName[(separator + 1)..];
    }
}

internal readonly record struct LoadedAttachedMacro(
    string Origin,
    IAttachedDeclarationMacro Macro,
    string CanonicalName,
    ImmutableArray<string> Aliases);

internal readonly record struct LoadedFreestandingMacro(
    string Origin,
    IMacroDefinition Macro,
    string CanonicalName,
    ImmutableArray<string> Aliases);
