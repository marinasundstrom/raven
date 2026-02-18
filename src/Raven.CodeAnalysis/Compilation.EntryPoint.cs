using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;
using System.Threading;

using Raven.CodeAnalysis.Symbols;
using Raven.CodeAnalysis.Syntax;

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
        if (method is SynthesizedMainMethodSymbol synthesizedMain && !synthesizedMain.ContainsExecutableCode)
            return false;

        return EntryPointSignature.Matches(method, this);
    }

    private void EnsureEntryPointComputed()
    {
        EnsureSetup();
        EnsureSourceDeclarationsComplete();

        if (_entryPointComputed)
            return;

        lock (_entryPointGate)
        {
            if (_entryPointComputed)
                return;

            var uniqueCandidates = new Dictionary<string, IMethodSymbol>(StringComparer.Ordinal);
            var diagnostics = ImmutableArray.CreateBuilder<Diagnostic>();

            foreach (var method in SourceGlobalNamespace
                .GetAllMembersRecursive()
                .OfType<IMethodSymbol>())
            {
                if (method.Name != EntryPointSignature.EntryPointName)
                    continue;

                if (method is SynthesizedMainMethodSymbol synthesizedMain && !synthesizedMain.ContainsExecutableCode)
                    continue;

                var hasValidReturn = EntryPointSignature.HasValidReturnType(method.ReturnType, this);
                var hasValidParameters = EntryPointSignature.HasValidParameters(method.Parameters, this);
                var hasValidShape = method.IsStatic
                    && !method.IsGenericMethod
                    && method.TypeParameters.IsDefaultOrEmpty
                    && hasValidReturn
                    && hasValidParameters;

                if (!hasValidShape)
                {
                    var location = GetEntryPointDiagnosticLocation(method);
                    diagnostics.Add(Diagnostic.Create(CompilerDiagnostics.EntryPointHasInvalidSignature, location));
                    continue;
                }

                var key = method.ToDisplayString(SymbolDisplayFormat.RavenSymbolKeyFormat);
                if (!uniqueCandidates.ContainsKey(key))
                    uniqueCandidates.Add(key, method);
            }

            var candidates = uniqueCandidates.Values.ToImmutableArray();

            if (candidates.Length == 1)
            {
                _entryPoint = TrySynthesizeEntryPointBridge(candidates[0]);
                _entryPointDiagnostics = diagnostics.ToImmutable();
            }
            else if (candidates.Length > 1)
            {
                _entryPoint = null;
                var builder = ImmutableArray.CreateBuilder<Diagnostic>(candidates.Length + diagnostics.Count);

                builder.AddRange(diagnostics);

                foreach (var candidate in candidates)
                {
                    var location = GetEntryPointDiagnosticLocation(candidate);
                    builder.Add(Diagnostic.Create(CompilerDiagnostics.EntryPointIsAmbiguous, location));
                }

                _entryPointDiagnostics = builder.ToImmutable();
            }
            else
            {
                _entryPoint = null;
                _entryPointDiagnostics = diagnostics.ToImmutable();
            }

            _entryPointComputed = true;
        }
    }

    private IMethodSymbol TrySynthesizeEntryPointBridge(IMethodSymbol entryPointCandidate)
    {
        if (!EntryPointSignature.RequiresEntryPointBridge(entryPointCandidate.ReturnType, this))
            return entryPointCandidate;

        if (entryPointCandidate is SynthesizedMainMethodSymbol { AsyncImplementation: not null })
            return entryPointCandidate;

        if (entryPointCandidate.ContainingSymbol is not SourceNamedTypeSymbol containingType)
            return entryPointCandidate;

        var existingBridge = containingType
            .GetMembers("<Main>_EntryPoint")
            .OfType<SynthesizedEntryPointBridgeMethodSymbol>()
            .FirstOrDefault(b => SymbolEqualityComparer.Default.Equals(b.AsyncImplementation, entryPointCandidate));

        if (existingBridge is not null)
            return existingBridge;

        var locations = entryPointCandidate.Locations.ToArray();
        var syntaxReferences = entryPointCandidate.DeclaringSyntaxReferences.ToArray();

        var bridge = new SynthesizedEntryPointBridgeMethodSymbol(
            containingType,
            locations,
            syntaxReferences,
            EntryPointSignature.ResolveBridgeReturnType(this, entryPointCandidate.ReturnType),
            entryPointCandidate);

        containingType.AddMember(bridge);

        return bridge;
    }

    private static Location GetEntryPointDiagnosticLocation(IMethodSymbol method)
    {
        var syntax = method.DeclaringSyntaxReferences.FirstOrDefault()?.GetSyntax();
        if (syntax is MethodDeclarationSyntax methodSyntax)
            return methodSyntax.Identifier.GetLocation();

        if (syntax is FunctionStatementSyntax functionSyntax)
            return functionSyntax.Identifier.GetLocation();

        return method.Locations.FirstOrDefault() ?? Location.None;
    }
}
