using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;

using Raven.CodeAnalysis.Symbols;

namespace Raven.CodeAnalysis;

public sealed class SubstitutionTypeResolver
{
    public enum ResolutionFailureKind
    {
        None = 0,

        // General resolution
        UnsupportedTypeSyntax,
        TypeNotFound,
        GenericTypeNotFound,
        QualifiedTypeNotFound,
        QualifiedGenericTypeNotFound,
        UnknownPredefinedKeyword,
        FrameworkTypeNotFound,

        // Generic construction
        ArityMismatch,

        // Type arguments
        TypeArgumentAmbiguous,
        TypeArgumentFailed,

        // Wrappers
        ArrayElementFailed,
        ByRefElementFailed,
        NullableUnderlyingFailed,

        // Lookup
        Ambiguous
    }
    private readonly Compilation _compilation;
    private readonly Func<string[], IEnumerable<INamedTypeSymbol>>? _resolveNamedTypesOverride;
    private readonly ITypeSymbol _errorType;
    private readonly IReadOnlyList<INamespaceOrTypeSymbol>? _importedScopes;

    public SubstitutionTypeResolver(
        Compilation compilation,
        ITypeSymbol errorType,
        IReadOnlyList<INamespaceOrTypeSymbol>? importedScopes = null,
        Func<string[], IEnumerable<INamedTypeSymbol>>? resolveNamedTypesOverride = null)
    {
        _compilation = compilation;
        _errorType = errorType;
        _importedScopes = importedScopes;
        _resolveNamedTypesOverride = resolveNamedTypesOverride;
    }

    // -----------------------------
    // Result object
    // -----------------------------

    public sealed record ResolveTypeResult
    {
        public required ITypeSymbol ResolvedType { get; init; }
        public INamedTypeSymbol? ResolvedNamedDefinition { get; init; }

        public bool IsAmbiguous { get; init; }
        public ImmutableArray<INamedTypeSymbol> AmbiguousCandidates { get; init; } = ImmutableArray<INamedTypeSymbol>.Empty;

        public bool SubstitutionFailed { get; init; }
        public ImmutableArray<ResolutionFailureKind> FailureKinds { get; init; } = ImmutableArray<ResolutionFailureKind>.Empty;

        public sealed record ResolutionIssue(
            TypeSyntax Syntax,
            ResolutionFailureKind Kind,
            string? Message,
            ImmutableArray<INamedTypeSymbol> Candidates)
        {
            public bool HasCandidates => !Candidates.IsDefaultOrEmpty && Candidates.Length > 0;

            public static ResolutionIssue Failure(TypeSyntax syntax, ResolutionFailureKind kind, string? message = null) =>
                new(syntax, kind, message, ImmutableArray<INamedTypeSymbol>.Empty);

            public static ResolutionIssue Ambiguous(TypeSyntax syntax, string? message, IEnumerable<INamedTypeSymbol> candidates) =>
                new(syntax, ResolutionFailureKind.Ambiguous, message, candidates is null ? ImmutableArray<INamedTypeSymbol>.Empty : candidates.ToImmutableArray());
        }

        public ImmutableArray<ResolutionIssue> Issues { get; init; } = ImmutableArray<ResolutionIssue>.Empty;

        public string? LookupOrigin { get; init; } // e.g. "imported scope #2", "global"
        public ImmutableArray<string> Notes { get; init; } = ImmutableArray<string>.Empty;

        public bool Success => ResolvedType is not null && ResolvedType.TypeKind != TypeKind.Error && !IsAmbiguous && !SubstitutionFailed;
    }

    public ResolveTypeResult ResolveType(
        TypeSyntax syntax,
        IDictionary<string, ITypeSymbol> typeParameters)
    {
        return ResolveType(syntax, typeParameters, _importedScopes);
    }

    public ResolveTypeResult ResolveType(
        TypeSyntax syntax,
        IDictionary<string, ITypeSymbol> typeParameters,
        IReadOnlyList<INamespaceOrTypeSymbol>? importedScopes)
    {
        return ResolveCore(syntax, typeParameters, importedScopes);
    }

    // -----------------------------
    // Core dispatch
    // -----------------------------

    private ResolveTypeResult ResolveCore(
        TypeSyntax syntax,
        IDictionary<string, ITypeSymbol> scope,
        IReadOnlyList<INamespaceOrTypeSymbol>? importedScopes)
    {
        return syntax switch
        {
            NameSyntax n => ResolveName(n, scope, importedScopes),
            PredefinedTypeSyntax p => ResolvePredefined(p, importedScopes),
            ArrayTypeSyntax a => ResolveArray(a, scope, importedScopes),
            ByRefTypeSyntax br => ResolveByRef(br, scope, importedScopes),
            NullableTypeSyntax n => ResolveNullable(n, scope, importedScopes),
            _ => Fail(ResolutionFailureKind.UnsupportedTypeSyntax)
        };
    }


    // -----------------------------
    // Name resolution (supports generic segments anywhere)
    // -----------------------------

    private sealed record NameSegment(
        NameSyntax Syntax,
        string Name,
        TypeArgumentListSyntax? TypeArguments);

    private ResolveTypeResult ResolveName(
        NameSyntax name,
        IDictionary<string, ITypeSymbol> scope,
        IReadOnlyList<INamespaceOrTypeSymbol>? importedScopes)
    {
        // Single identifier/generic identifier fast-path: allow substitution map to win.
        if (name is IdentifierNameSyntax id)
        {
            var text = id.Identifier.ValueText;
            if (scope.TryGetValue(text, out var substituted))
            {
                return new ResolveTypeResult
                {
                    ResolvedType = substituted,
                    Notes = ImmutableArray.Create("Resolved from type parameter substitution map.")
                };
            }
        }
        else if (name is GenericNameSyntax g)
        {
            // If the generic name is actually a type parameter (rare but possible in some syntaxes),
            // we still want the map to win when the identifier matches.
            var text = g.Identifier.ValueText;
            if (scope.TryGetValue(text, out var substituted))
            {
                return new ResolveTypeResult
                {
                    ResolvedType = substituted,
                    Notes = ImmutableArray.Create("Resolved from type parameter substitution map.")
                };
            }
        }

        var segments = FlattenSegments(name);
        if (segments.Length == 0)
            return Fail(name, ResolutionFailureKind.TypeNotFound, "Empty name.");

        // Resolve type arguments for each segment once (independent of root), and propagate failures.
        var resolvedArgsByIndex = new (bool Failed, ImmutableArray<ResolutionFailureKind> FailureKinds, ImmutableArray<ResolveTypeResult.ResolutionIssue> Issues, ITypeSymbol[] Args)[segments.Length];
        for (int i = 0; i < segments.Length; i++)
        {
            if (segments[i].TypeArguments is null)
            {
                resolvedArgsByIndex[i] = (false, ImmutableArray<ResolutionFailureKind>.Empty, ImmutableArray<ResolveTypeResult.ResolutionIssue>.Empty, Array.Empty<ITypeSymbol>());
                continue;
            }

            var args = ResolveTypeArguments(segments[i].TypeArguments!, scope, importedScopes);
            if (args.SubstitutionFailed)
            {
                resolvedArgsByIndex[i] = (true, args.FailureKinds, args.Issues, Array.Empty<ITypeSymbol>());
            }
            else
            {
                resolvedArgsByIndex[i] = (false, ImmutableArray<ResolutionFailureKind>.Empty, ImmutableArray<ResolveTypeResult.ResolutionIssue>.Empty, args.ResolvedTypeArguments);
            }
        }

        // Start states from imported scopes + global.
        var startStates = new List<(INamespaceOrTypeSymbol Root, string Origin)>();

        if (importedScopes is not null)
        {
            for (int i = 0; i < importedScopes.Count; i++)
                startStates.Add((importedScopes[i], $"imported scope #{i}"));
        }

        startStates.Add((_compilation.GlobalNamespace, "global"));

        // Walk segments with limited backtracking across namespace-vs-type at each step.
        var finalCandidates = new List<(INamedTypeSymbol Type, string Origin)>();
        var ambiguityCandidates = new List<INamedTypeSymbol>();

        foreach (var (root, origin) in startStates)
        {
            var states = new List<INamespaceOrTypeSymbol> { root };

            // Track whether we encountered a type-argument failure for this root.
            bool argFailure = false;
            var argFailureKinds = ImmutableArray<ResolutionFailureKind>.Empty;
            var argFailureIssues = ImmutableArray<ResolveTypeResult.ResolutionIssue>.Empty;

            for (int segIndex = 0; segIndex < segments.Length; segIndex++)
            {
                var seg = segments[segIndex];
                var isLast = segIndex == segments.Length - 1;

                // If this segment's type args failed to resolve, we can fail immediately.
                var argInfo = resolvedArgsByIndex[segIndex];
                if (seg.TypeArguments is not null && argInfo.Failed)
                {
                    argFailure = true;
                    argFailureKinds = argInfo.FailureKinds;
                    argFailureIssues = argInfo.Issues;
                    break;
                }

                var nextStates = new List<INamespaceOrTypeSymbol>();

                foreach (var st in states)
                {
                    foreach (var next in ExpandSegment(st, seg, isLast))
                    {
                        if (seg.TypeArguments is null)
                        {
                            nextStates.Add(next);
                        }
                        else
                        {
                            // Generic segment must be a named type.
                            if (next is INamedTypeSymbol nt)
                            {
                                // Arity check and construction.
                                if (nt.TypeParameters.Length != argInfo.Args.Length)
                                {
                                    argFailure = true;
                                    argFailureKinds = ImmutableArray.Create(ResolutionFailureKind.ArityMismatch);
                                    argFailureIssues = ImmutableArray.Create(
                                        ResolveTypeResult.ResolutionIssue.Failure(seg.Syntax, ResolutionFailureKind.ArityMismatch,
                                            $"Arity mismatch for '{seg.Name}': expected {nt.TypeParameters.Length}, got {argInfo.Args.Length}."));
                                    break;
                                }

                                nextStates.Add(nt.Construct(argInfo.Args));
                            }
                        }
                    }

                    if (argFailure)
                        break;
                }

                if (argFailure)
                    break;

                // De-dupe states by symbol equality to control explosion.
                states = DedupStates(nextStates).ToList();

                if (states.Count == 0)
                    break;
            }

            if (argFailure)
            {
                // Return substitution failure rooted at the segment that failed.
                return new ResolveTypeResult
                {
                    ResolvedType = _errorType,
                    SubstitutionFailed = true,
                    FailureKinds = argFailureKinds,
                    Issues = argFailureIssues
                };
            }

            // The final symbol must be a type.
            foreach (var st in states)
            {
                if (st is INamedTypeSymbol nt)
                    finalCandidates.Add((nt, origin));
            }
        }

        // De-dupe candidates by symbol equality.
        var dedupedFinal = new List<(INamedTypeSymbol Type, string Origin)>();
        foreach (var c in finalCandidates)
        {
            if (!dedupedFinal.Any(d => SymbolEqualityComparer.Default.Equals(d.Type, c.Type)))
                dedupedFinal.Add(c);
        }

        if (dedupedFinal.Count == 0)
        {
            // Choose a failure kind that matches whether any segment was generic.
            bool anyGeneric = segments.Any(s => s.TypeArguments is not null);
            var kind = anyGeneric ? ResolutionFailureKind.QualifiedGenericTypeNotFound : ResolutionFailureKind.QualifiedTypeNotFound;
            return Fail(name, kind, $"Type '{DescribeNameSegments(segments)}' not found.");
        }

        if (dedupedFinal.Count > 1)
        {
            ambiguityCandidates.AddRange(dedupedFinal.Select(x => x.Type));
            return new ResolveTypeResult
            {
                ResolvedType = _errorType,
                IsAmbiguous = true,
                AmbiguousCandidates = ambiguityCandidates.Distinct(SymbolEqualityComparer.Default).OfType<INamedTypeSymbol>().ToImmutableArray(),
                FailureKinds = ImmutableArray.Create(ResolutionFailureKind.Ambiguous),
                Issues = ImmutableArray.Create(
                    ResolveTypeResult.ResolutionIssue.Ambiguous(
                        name,
                        "Name resolution was ambiguous; multiple candidate types matched.",
                        ambiguityCandidates.Distinct(SymbolEqualityComparer.Default).OfType<INamedTypeSymbol>())),
                Notes = ImmutableArray.Create("Name resolution was ambiguous; multiple candidate types matched.")
            };
        }

        var chosen = dedupedFinal[0];
        return new ResolveTypeResult
        {
            ResolvedType = chosen.Type,
            ResolvedNamedDefinition = (chosen.Type.ConstructedFrom as INamedTypeSymbol) ?? (chosen.Type.OriginalDefinition as INamedTypeSymbol) ?? chosen.Type,
            LookupOrigin = chosen.Origin
        };
    }

    private static IReadOnlyList<INamespaceOrTypeSymbol> DedupStates(List<INamespaceOrTypeSymbol> states)
    {
        var result = new List<INamespaceOrTypeSymbol>();
        foreach (var s in states)
        {
            if (!result.Any(r => SymbolEqualityComparer.Default.Equals(r, s)))
                result.Add(s);
        }

        return result;
    }

    private static IEnumerable<INamespaceOrTypeSymbol> ExpandSegment(
        INamespaceOrTypeSymbol current,
        NameSegment seg,
        bool isLast)
    {
        // Generic segments cannot bind to namespaces.
        var wantsType = seg.TypeArguments is not null || isLast;

        // When inside a type, only nested types are valid.
        if (current is INamedTypeSymbol ct)
        {
            foreach (var t in ct.GetTypeMembers(seg.Name).OfType<INamedTypeSymbol>())
                yield return t;
            yield break;
        }

        // Namespace context: allow namespace traversal and/or type traversal.
        if (current is INamespaceSymbol ns)
        {
            // Prefer namespaces when not forced to take a type and not at last segment.
            if (!wantsType)
            {
                foreach (var n in ns.GetMembers(seg.Name).OfType<INamespaceSymbol>())
                    yield return n;
            }

            foreach (var t in ns.GetMembers(seg.Name).OfType<INamedTypeSymbol>())
                yield return t;
        }
    }

    private static NameSegment[] FlattenSegments(NameSyntax syntax)
    {
        var parts = new List<NameSegment>();

        void Walk(NameSyntax n)
        {
            switch (n)
            {
                case QualifiedNameSyntax q:
                    Walk(q.Left);
                    Walk(q.Right);
                    break;

                case IdentifierNameSyntax id:
                    parts.Add(new NameSegment(id, id.Identifier.ValueText, null));
                    break;

                case GenericNameSyntax g:
                    parts.Add(new NameSegment(g, g.Identifier.ValueText, g.TypeArgumentList));
                    break;
            }
        }

        Walk(syntax);
        return parts.ToArray();
    }

    private static string DescribeNameSegments(NameSegment[] segments)
    {
        // Minimal display name; avoid depending on printers.
        return string.Join(".", segments.Select(s => s.TypeArguments is null ? s.Name : $"{s.Name}<...>"));
    }

    // -----------------------------
    // Predefined: int, bool, string...
    // -----------------------------

    private ResolveTypeResult ResolvePredefined(
        PredefinedTypeSyntax p,
        IReadOnlyList<INamespaceOrTypeSymbol>? importedScopes)
    {
        string[]? parts = p.Keyword.Kind switch
        {
            SyntaxKind.IntKeyword => new[] { "System", "Int32" },
            SyntaxKind.UIntKeyword => new[] { "System", "UInt32" },
            SyntaxKind.LongKeyword => new[] { "System", "Int64" },
            SyntaxKind.ULongKeyword => new[] { "System", "UInt64" },
            SyntaxKind.ShortKeyword => new[] { "System", "Int16" },
            SyntaxKind.UShortKeyword => new[] { "System", "UInt16" },
            SyntaxKind.SByteKeyword => new[] { "System", "SByte" },
            SyntaxKind.BoolKeyword => new[] { "System", "Boolean" },
            SyntaxKind.StringKeyword => new[] { "System", "String" },
            SyntaxKind.CharKeyword => new[] { "System", "Char" },
            SyntaxKind.FloatKeyword => new[] { "System", "Single" },
            SyntaxKind.DoubleKeyword => new[] { "System", "Double" },
            SyntaxKind.ByteKeyword => new[] { "System", "Byte" },
            SyntaxKind.ObjectKeyword => new[] { "System", "Object" },
            SyntaxKind.NIntKeyword => new[] { "System", "IntPtr" },
            SyntaxKind.NUIntKeyword => new[] { "System", "UIntPtr" },
            SyntaxKind.UnitKeyword => new[] { "System", "Unit" },
            _ => null
        };

        if (parts is null)
            return Fail(p, ResolutionFailureKind.UnknownPredefinedKeyword, $"Unknown predefined type keyword '{p.Keyword.ValueText}'.");

        var lookup = ResolveNamedTypeDefinitions(parts, importedScopes);
        if (lookup.IsAmbiguous)
            return Ambiguous(p, lookup);

        if (lookup.Definition is null)
            return Fail(p, ResolutionFailureKind.FrameworkTypeNotFound, $"Framework type '{string.Join(".", parts)}' not found.");

        return new ResolveTypeResult
        {
            ResolvedType = lookup.Definition,
            ResolvedNamedDefinition = lookup.Definition,
            LookupOrigin = lookup.Origin
        };
    }

    // -----------------------------
    // Arrays
    // -----------------------------

    private ResolveTypeResult ResolveArray(
        ArrayTypeSyntax a,
        IDictionary<string, ITypeSymbol> scope,
        IReadOnlyList<INamespaceOrTypeSymbol>? importedScopes)
    {
        var element = ResolveCore(a.ElementType, scope, importedScopes);
        if (!element.Success)
            return element with
            {
                SubstitutionFailed = true,
                FailureKinds = element.FailureKinds.Add(ResolutionFailureKind.ArrayElementFailed),
                Issues = element.Issues.Add(ResolveTypeResult.ResolutionIssue.Failure(a.ElementType, ResolutionFailureKind.ArrayElementFailed))
            };

        return new ResolveTypeResult
        {
            ResolvedType = _compilation.CreateArrayTypeSymbol(element.ResolvedType, a.RankSpecifiers.Count),
            Notes = ImmutableArray.Create("Created array type symbol from resolved element type.")
        };
    }

    // -----------------------------
    // ByRef
    // -----------------------------

    private ResolveTypeResult ResolveByRef(
        ByRefTypeSyntax br,
        IDictionary<string, ITypeSymbol> scope,
        IReadOnlyList<INamespaceOrTypeSymbol>? importedScopes)
    {
        var element = ResolveCore(br.ElementType, scope, importedScopes);
        if (!element.Success)
            return element with
            {
                SubstitutionFailed = true,
                FailureKinds = element.FailureKinds.Add(ResolutionFailureKind.ByRefElementFailed),
                Issues = element.Issues.Add(ResolveTypeResult.ResolutionIssue.Failure(br.ElementType, ResolutionFailureKind.ByRefElementFailed))
            };

        return new ResolveTypeResult
        {
            ResolvedType = new RefTypeSymbol(element.ResolvedType),
            Notes = ImmutableArray.Create("Created byref type symbol from resolved element type.")
        };
    }

    // -----------------------------
    // Nullable
    // -----------------------------

    private ResolveTypeResult ResolveNullable(
        NullableTypeSyntax n,
        IDictionary<string, ITypeSymbol> scope,
        IReadOnlyList<INamespaceOrTypeSymbol>? importedScopes)
    {
        var underlying = ResolveCore(n.ElementType, scope, importedScopes);
        if (!underlying.Success)
            return underlying with
            {
                SubstitutionFailed = true,
                FailureKinds = underlying.FailureKinds.Add(ResolutionFailureKind.NullableUnderlyingFailed),
                Issues = underlying.Issues.Add(ResolveTypeResult.ResolutionIssue.Failure(n.ElementType, ResolutionFailureKind.NullableUnderlyingFailed))
            };

        return new ResolveTypeResult
        {
            ResolvedType = underlying.ResolvedType.MakeNullable(),
            Notes = ImmutableArray.Create("Created nullable type symbol from resolved underlying type.")
        };
    }

    // -----------------------------
    // Type argument resolution with substitution-failure reporting
    // -----------------------------

    private sealed record TypeArgumentsResult
    {
        public required ITypeSymbol[] ResolvedTypeArguments { get; init; }
        public bool SubstitutionFailed { get; init; }
        public ImmutableArray<ResolutionFailureKind> FailureKinds { get; init; } = ImmutableArray<ResolutionFailureKind>.Empty;
        public ImmutableArray<ResolveTypeResult.ResolutionIssue> Issues { get; init; } = ImmutableArray<ResolveTypeResult.ResolutionIssue>.Empty;

        public ResolveTypeResult ToResolveTypeResult(ITypeSymbol errorType) =>
            new ResolveTypeResult
            {
                ResolvedType = errorType,
                SubstitutionFailed = true,
                FailureKinds = FailureKinds,
                Issues = Issues
            };
    }

    private TypeArgumentsResult ResolveTypeArguments(
        TypeArgumentListSyntax list,
        IDictionary<string, ITypeSymbol> scope,
        IReadOnlyList<INamespaceOrTypeSymbol>? importedScopes)
    {
        var result = new ITypeSymbol[list.Arguments.Count];
        var failures = ImmutableArray.CreateBuilder<ResolutionFailureKind>();
        var issues = ImmutableArray.CreateBuilder<ResolveTypeResult.ResolutionIssue>();

        for (int i = 0; i < list.Arguments.Count; i++)
        {
            var argSyntax = list.Arguments[i].Type;
            var resolved = ResolveCore(argSyntax, scope, importedScopes);

            if (resolved.IsAmbiguous)
            {
                failures.Add(ResolutionFailureKind.TypeArgumentAmbiguous);
                issues.Add(
                    ResolveTypeResult.ResolutionIssue.Ambiguous(
                        argSyntax,
                        $"Type argument #{i} was ambiguous.",
                        resolved.AmbiguousCandidates));
                continue;
            }

            if (resolved.SubstitutionFailed || resolved.ResolvedType.TypeKind == TypeKind.Error)
            {
                failures.Add(ResolutionFailureKind.TypeArgumentFailed);

                issues.Add(ResolveTypeResult.ResolutionIssue.Failure(
                    argSyntax,
                    ResolutionFailureKind.TypeArgumentFailed,
                    $"Type argument #{i} failed to resolve."));

                if (!resolved.Issues.IsDefaultOrEmpty)
                    issues.AddRange(resolved.Issues);

                if (!resolved.FailureKinds.IsDefaultOrEmpty)
                    failures.AddRange(resolved.FailureKinds);

                continue;
            }

            result[i] = resolved.ResolvedType;
        }

        if (failures.Count > 0)
        {
            return new TypeArgumentsResult
            {
                ResolvedTypeArguments = Array.Empty<ITypeSymbol>(),
                SubstitutionFailed = true,
                FailureKinds = failures.ToImmutable(),
                Issues = issues.ToImmutable()
            };
        }

        return new TypeArgumentsResult
        {
            ResolvedTypeArguments = result,
            SubstitutionFailed = false,
            FailureKinds = ImmutableArray<ResolutionFailureKind>.Empty,
            Issues = ImmutableArray<ResolveTypeResult.ResolutionIssue>.Empty
        };
    }

    // -----------------------------
    // Construct w/ arity checks and substitution-failure reporting
    // -----------------------------

    private ResolveTypeResult Construct(
        INamedTypeSymbol def,
        ITypeSymbol[] args,
        string? origin)
    {
        var definition =
            def.ConstructedFrom as INamedTypeSymbol ??
            def.OriginalDefinition as INamedTypeSymbol ??
            def;

        if (definition.TypeParameters.Length != args.Length)
        {
            return new ResolveTypeResult
            {
                ResolvedType = _errorType,
                ResolvedNamedDefinition = definition,
                LookupOrigin = origin,
                SubstitutionFailed = true,
                FailureKinds = ImmutableArray.Create(ResolutionFailureKind.ArityMismatch)
            };
        }

        // TODO (your note): type-parameter compatibility (variance + conversions + constraints)
        // - variance checks apply mostly to interface/delegate type params
        // - constraint checks apply to generic constraints if you model them
        // - conversion checks apply if your language allows implicit conversions in type args (often it does NOT)
        //
        // If you later add these, return SubstitutionFailed=true with a reason per failing argument.

        return new ResolveTypeResult
        {
            ResolvedType = definition.Construct(args),
            ResolvedNamedDefinition = definition,
            LookupOrigin = origin,
            Notes = ImmutableArray.Create("Constructed named type from resolved definition and type arguments.")
        };
    }

    // -----------------------------
    // Name flattening
    // -----------------------------

    private static string[] Flatten(NameSyntax syntax)
    {
        var parts = new List<string>();

        void Walk(NameSyntax n)
        {
            switch (n)
            {
                case IdentifierNameSyntax id:
                    parts.Add(id.Identifier.ValueText);
                    break;
                case GenericNameSyntax g:
                    parts.Add(g.Identifier.ValueText);
                    break;
                case QualifiedNameSyntax q:
                    Walk(q.Left);
                    Walk(q.Right);
                    break;
            }
        }

        Walk(syntax);
        return parts.ToArray();
    }

    // -----------------------------
    // Imported scopes + global lookup that can yield ambiguity
    // -----------------------------

    private sealed record NamedLookup
    {
        public INamedTypeSymbol? Definition { get; init; }
        public bool IsAmbiguous { get; init; }
        public ImmutableArray<INamedTypeSymbol> Candidates { get; init; } = ImmutableArray<INamedTypeSymbol>.Empty;
        public string? Origin { get; init; } // where the chosen def came from
    }

    private NamedLookup ResolveNamedTypeDefinitions(
        string[] parts,
        IReadOnlyList<INamespaceOrTypeSymbol>? importedScopes)
    {
        // Optional override (tests / special binder behavior)
        if (_resolveNamedTypesOverride is not null)
        {
            var overridden = _resolveNamedTypesOverride(parts)?.Where(x => x is not null).ToArray() ?? Array.Empty<INamedTypeSymbol>();
            return CollapseCandidates(overridden, origin: "override");
        }

        var allCandidates = new List<(INamedTypeSymbol Type, string Origin)>();

        // 1) Imported scopes first
        if (importedScopes is not null && importedScopes.Count > 0)
        {
            for (int i = 0; i < importedScopes.Count; i++)
            {
                var root = importedScopes[i];
                var found = ResolveFromRoot(root, parts);
                if (found is not null)
                    allCandidates.Add((found, $"imported scope #{i}"));
            }
        }

        // 2) Always also try global namespace as fallback
        {
            var globalFound = ResolveFromRoot(_compilation.GlobalNamespace, parts);
            if (globalFound is not null)
                allCandidates.Add((globalFound, "global"));
        }

        // De-dupe by symbol equality (important when same type is reachable via multiple roots)
        var deduped = new List<(INamedTypeSymbol Type, string Origin)>();
        foreach (var c in allCandidates)
        {
            if (!deduped.Any(d => SymbolEqualityComparer.Default.Equals(d.Type, c.Type)))
                deduped.Add(c);
        }

        if (deduped.Count == 0)
            return new NamedLookup { Definition = null };

        if (deduped.Count == 1)
            return new NamedLookup { Definition = deduped[0].Type, Origin = deduped[0].Origin };

        // Ambiguous: multiple different types matched
        return new NamedLookup
        {
            IsAmbiguous = true,
            Candidates = deduped.Select(x => x.Type).ToImmutableArray(),
            Origin = null
        };
    }

    private static NamedLookup CollapseCandidates(INamedTypeSymbol[] candidates, string origin)
    {
        if (candidates.Length == 0)
            return new NamedLookup { Definition = null };

        var deduped = new List<INamedTypeSymbol>();
        foreach (var c in candidates)
        {
            if (!deduped.Any(d => SymbolEqualityComparer.Default.Equals(d, c)))
                deduped.Add(c);
        }

        if (deduped.Count == 1)
            return new NamedLookup { Definition = deduped[0], Origin = origin };

        return new NamedLookup
        {
            IsAmbiguous = true,
            Candidates = deduped.ToImmutableArray()
        };
    }

    /// <summary>
    /// Walk segments under a root which can be a namespace or a type.
    /// Prefers namespaces at each step, but allows nested-type traversal.
    /// </summary>
    private static INamedTypeSymbol? ResolveFromRoot(INamespaceOrTypeSymbol root, string[] parts)
    {
        INamespaceOrTypeSymbol current = root;

        for (int i = 0; i < parts.Length; i++)
        {
            var segment = parts[i];
            var isLast = i == parts.Length - 1;

            if (isLast)
            {
                return current
                    .GetMembers(segment)
                    .OfType<INamedTypeSymbol>()
                    .FirstOrDefault();
            }

            var nextNs = current
                .GetMembers(segment)
                .OfType<INamespaceSymbol>()
                .FirstOrDefault();

            if (nextNs is not null)
            {
                current = nextNs;
                continue;
            }

            var nextType = current
                .GetMembers(segment)
                .OfType<INamedTypeSymbol>()
                .FirstOrDefault();

            if (nextType is null)
                return null;

            current = nextType;
        }

        return null;
    }

    // -----------------------------
    // Result helpers
    // -----------------------------

    private ResolveTypeResult Ambiguous(TypeSyntax syntax, NamedLookup lookup)
    {
        return new ResolveTypeResult
        {
            ResolvedType = _errorType,
            IsAmbiguous = true,
            AmbiguousCandidates = lookup.Candidates,
            FailureKinds = ImmutableArray.Create(ResolutionFailureKind.Ambiguous),
            Issues = ImmutableArray.Create(
                ResolveTypeResult.ResolutionIssue.Ambiguous(
                    syntax,
                    "Name resolution was ambiguous; multiple candidate types matched.",
                    lookup.Candidates)),
            Notes = ImmutableArray.Create("Name resolution was ambiguous; multiple candidate types matched.")
        };
    }

    private ResolveTypeResult Fail(ResolutionFailureKind kind)
    {
        return new ResolveTypeResult
        {
            ResolvedType = _errorType,
            SubstitutionFailed = true,
            FailureKinds = ImmutableArray.Create(kind)
        };
    }

    private ResolveTypeResult Fail(TypeSyntax syntax, ResolutionFailureKind kind, string? message = null)
    {
        return new ResolveTypeResult
        {
            ResolvedType = _errorType,
            SubstitutionFailed = true,
            FailureKinds = ImmutableArray.Create(kind),
            Issues = ImmutableArray.Create(ResolveTypeResult.ResolutionIssue.Failure(syntax, kind, message))
        };
    }

    private static string DescribeSyntax(TypeSyntax syntax)
    {
        // Minimal, safe stringification; replace with your pretty printer if you want.
        return syntax.Kind.ToString();
    }
}
