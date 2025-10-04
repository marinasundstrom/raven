using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;
using System.Threading;

using Raven.CodeAnalysis.Symbols;

namespace Raven.CodeAnalysis;

public partial class Compilation
{
    private readonly object _entryPointGate = new();
    private bool _entryPointComputed;
    private IMethodSymbol? _entryPoint;
    private ImmutableArray<Diagnostic> _entryPointDiagnostics = ImmutableArray<Diagnostic>.Empty;

    public IMethodSymbol? GetEntryPoint(CancellationToken cancellationToken = default)
    {
        EnsureEntryPointComputed();
        return _entryPoint;
    }

    internal ImmutableArray<Diagnostic> GetEntryPointDiagnostics(CancellationToken cancellationToken = default)
    {
        EnsureEntryPointComputed();
        return _entryPointDiagnostics;
    }

    internal bool IsEntryPointCandidate(IMethodSymbol method)
    {
        if (method.Name != "Main" || !method.IsStatic || method.IsGenericMethod)
            return false;

        if (!method.TypeParameters.IsDefaultOrEmpty)
            return false;

        if (!IsValidEntryPointReturnType(method.ReturnType))
            return false;

        var parameters = method.Parameters;

        if (parameters.Length == 0)
            return true;

        if (parameters.Length > 1)
            return false;

        var parameter = parameters[0];

        if (parameter.RefKind != RefKind.None)
            return false;

        if (parameter.Type is not IArrayTypeSymbol arrayType)
            return false;

        var stringType = GetSpecialType(SpecialType.System_String);
        return SymbolEqualityComparer.Default.Equals(arrayType.ElementType, stringType);
    }

    private bool IsValidEntryPointReturnType(ITypeSymbol returnType)
    {
        switch (returnType.SpecialType)
        {
            case SpecialType.System_Int32:
            case SpecialType.System_Void:
            case SpecialType.System_Unit:
                return true;
        }

        var taskType = GetTypeByMetadataName("System.Threading.Tasks.Task");
        if (taskType is not null && SymbolEqualityComparer.Default.Equals(returnType, taskType))
            return true;

        if (returnType is INamedTypeSymbol named && !named.IsUnboundGenericType && named.Arity == 1)
        {
            var taskOfT = GetTypeByMetadataName("System.Threading.Tasks.Task`1");
            if (taskOfT is INamedTypeSymbol definition && SymbolEqualityComparer.Default.Equals(named.ConstructedFrom, definition))
            {
                var intType = GetSpecialType(SpecialType.System_Int32);
                if (!named.TypeArguments.IsDefaultOrEmpty && SymbolEqualityComparer.Default.Equals(named.TypeArguments[0], intType))
                    return true;
            }
        }

        return false;
    }

    private void EnsureEntryPointComputed()
    {
        if (_entryPointComputed)
            return;

        lock (_entryPointGate)
        {
            if (_entryPointComputed)
                return;

            var uniqueCandidates = new Dictionary<string, IMethodSymbol>(StringComparer.Ordinal);

            foreach (var method in SourceGlobalNamespace
                .GetAllMembersRecursive()
                .OfType<IMethodSymbol>()
                .Where(IsEntryPointCandidate))
            {
                var key = method.ToDisplayString(SymbolDisplayFormat.CSharpSymbolKeyFormat);
                if (!uniqueCandidates.ContainsKey(key))
                    uniqueCandidates.Add(key, method);
            }

            var candidates = uniqueCandidates.Values.ToImmutableArray();

            if (Options.OutputKind != OutputKind.ConsoleApplication)
            {
                _entryPoint = candidates.Length == 1 ? candidates[0] : null;
                _entryPointDiagnostics = ImmutableArray<Diagnostic>.Empty;
            }
            else if (candidates.Length == 1)
            {
                _entryPoint = candidates[0];
                _entryPointDiagnostics = ImmutableArray<Diagnostic>.Empty;
            }
            else if (candidates.Length > 1)
            {
                _entryPoint = null;
                var builder = ImmutableArray.CreateBuilder<Diagnostic>(candidates.Length);

                foreach (var candidate in candidates)
                {
                    var location = candidate.Locations.FirstOrDefault() ?? Location.None;
                    builder.Add(Diagnostic.Create(CompilerDiagnostics.EntryPointIsAmbiguous, location));
                }

                _entryPointDiagnostics = builder.ToImmutable();
            }
            else
            {
                _entryPoint = null;
                _entryPointDiagnostics = ImmutableArray<Diagnostic>.Empty;
            }

            _entryPointComputed = true;
        }
    }
}
