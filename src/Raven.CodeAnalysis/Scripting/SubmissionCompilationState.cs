using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;

using Raven.CodeAnalysis.Symbols;

namespace Raven.CodeAnalysis.Scripting;

/// <summary>
/// Owns the lazily derived compiler state for one script submission.
/// </summary>
internal sealed class SubmissionCompilationState
{
    private readonly Compilation _compilation;
    private readonly object _declarationsGate = new();
    private readonly Dictionary<LocalSymbol, SubmissionVariableSymbol> _variables = new(SymbolEqualityComparer.Default);
    private ImmutableArray<Symbol> _visibleDeclarations;

    internal SubmissionCompilationState(Compilation compilation)
    {
        _compilation = compilation;
    }

    internal static MetadataReference[] AddPreviousReferences(
        IEnumerable<MetadataReference> references,
        Compilation previousCompilation,
        MetadataReference? previousCompilationReference)
    {
        var builder = references.ToList();
        var previous = previousCompilation;
        var emittedReference = previousCompilationReference;

        while (previous is not null)
        {
            var referenceToAdd = emittedReference ?? previous.ToMetadataReference();
            if (!builder.Contains(referenceToAdd))
                builder.Add(referenceToAdd);

            emittedReference = previous.ScriptCompilationInfo?.PreviousScriptCompilationReference;
            previous = previous.ScriptCompilationInfo?.PreviousScriptCompilation;
        }

        return builder.ToArray();
    }

    internal ImmutableArray<Symbol> GetPreviousDeclarations()
    {
        var declarations = _compilation.ScriptCompilationInfo?.PreviousScriptCompilation?
            .SubmissionState.GetVisibleDeclarations() ?? ImmutableArray<Symbol>.Empty;

        return declarations.IsDefaultOrEmpty
            ? declarations
            : declarations.Select(MapPreviousDeclaration).ToImmutableArray();
    }

    internal bool TryGetVariable(LocalSymbol local, out SubmissionVariableSymbol variable)
    {
        _ = GetVisibleDeclarations();
        return _variables.TryGetValue(local, out variable!);
    }

    internal int VariableCount
        => GetVisibleDeclarations()
            .OfType<SubmissionVariableSymbol>()
            .Select(static variable => variable.Slot + 1)
            .DefaultIfEmpty()
            .Max();

    internal bool IsPreviousAssembly(AssemblySymbol? assembly)
    {
        if (assembly is null)
            return false;

        for (var previous = _compilation.ScriptCompilationInfo?.PreviousScriptCompilation;
             previous is not null;
             previous = previous.ScriptCompilationInfo?.PreviousScriptCompilation)
        {
            if (SymbolEqualityComparer.Default.Equals(previous.Assembly, assembly))
                return true;
        }

        return false;
    }

    internal ImmutableArray<Symbol> GetVisibleDeclarations()
    {
        if (!_visibleDeclarations.IsDefault)
            return _visibleDeclarations;

        lock (_declarationsGate)
        {
            if (!_visibleDeclarations.IsDefault)
                return _visibleDeclarations;

            var values = new Dictionary<string, Symbol>(StringComparer.Ordinal);
            var functions = new List<MethodSymbol>();
            var nextVariableSlot = 0;

            if (_compilation.ScriptCompilationInfo?.PreviousScriptCompilation is { } previous)
            {
                foreach (var declaration in previous.SubmissionState.GetVisibleDeclarations())
                {
                    AddDeclaration(declaration);
                    if (declaration is SubmissionVariableSymbol previousVariable)
                        nextVariableSlot = Math.Max(nextVariableSlot, previousVariable.Slot + 1);
                }
            }

            foreach (var syntaxTree in _compilation.SyntaxTrees)
            {
                var semanticModel = _compilation.GetSemanticModel(syntaxTree);
                foreach (var declaration in semanticModel.GetSubmissionDeclarations())
                {
                    switch (declaration)
                    {
                        case LocalSymbol local:
                            if (!_variables.TryGetValue(local, out var variable))
                            {
                                variable = new SubmissionVariableSymbol(local, nextVariableSlot);
                                _variables.Add(local, variable);
                            }

                            nextVariableSlot = Math.Max(nextVariableSlot, variable.Slot + 1);
                            AddDeclaration(variable);
                            break;

                        case MethodSymbol method:
                            AddDeclaration(method);
                            break;
                    }
                }
            }

            _visibleDeclarations = values.Values
                .Concat<Symbol>(functions)
                .ToImmutableArray();
            return _visibleDeclarations;

            void AddDeclaration(Symbol declaration)
            {
                if (declaration is MethodSymbol method)
                    functions.Add(method);
                else
                    values[declaration.Name] = declaration;
            }
        }
    }

    private Symbol MapPreviousDeclaration(Symbol declaration)
    {
        if (declaration is not MethodSymbol method ||
            method.ContainingAssembly is not SourceAssemblySymbol sourceAssembly ||
            !TryGetPreviousReference(sourceAssembly.Compilation, out var reference) ||
            _compilation.GetAssemblyOrModuleSymbol(reference) is not AssemblySymbol assembly ||
            method.ContainingType is not { } containingType ||
            assembly.GetTypeByMetadataName(containingType.MetadataName) is not { } metadataType)
        {
            return declaration;
        }

        return metadataType.GetMembers(method.MetadataName)
            .OfType<MethodSymbol>()
            .FirstOrDefault(candidate => HaveEquivalentSignature(method, candidate))
            ?? declaration;
    }

    private bool TryGetPreviousReference(Compilation previousCompilation, out MetadataReference reference)
    {
        for (var info = _compilation.ScriptCompilationInfo;
             info?.PreviousScriptCompilation is { } previous;
             info = previous.ScriptCompilationInfo)
        {
            if (ReferenceEquals(previous, previousCompilation) &&
                info.PreviousScriptCompilationReference is { } emittedReference)
            {
                reference = emittedReference;
                return true;
            }
        }

        reference = null!;
        return false;
    }

    private static bool HaveEquivalentSignature(MethodSymbol source, MethodSymbol metadata)
    {
        if (source.Arity != metadata.Arity || source.Parameters.Length != metadata.Parameters.Length)
            return false;

        for (var i = 0; i < source.Parameters.Length; i++)
        {
            if (source.Parameters[i].RefKind != metadata.Parameters[i].RefKind ||
                !HaveEquivalentType(source.Parameters[i].Type, metadata.Parameters[i].Type))
            {
                return false;
            }
        }

        return true;
    }

    private static bool HaveEquivalentType(TypeSymbol source, TypeSymbol metadata)
    {
        if (source.SpecialType != SpecialType.None || metadata.SpecialType != SpecialType.None)
            return source.SpecialType == metadata.SpecialType;

        if (!string.Equals(source.MetadataName, metadata.MetadataName, StringComparison.Ordinal))
            return false;

        return source is not NamedTypeSymbol sourceNamed ||
            metadata is not NamedTypeSymbol metadataNamed ||
            sourceNamed.TypeArguments.Length == metadataNamed.TypeArguments.Length;
    }
}
