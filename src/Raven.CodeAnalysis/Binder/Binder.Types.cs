using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;

using Raven.CodeAnalysis.Symbols;

namespace Raven.CodeAnalysis;

internal abstract partial class Binder
{
    // -----------------------------
    // Options + Result
    // -----------------------------

    public enum TypeResolutionFailureKind
    {
        None = 0,

        UnsupportedTypeSyntax,
        TypeNotFound,
        GenericTypeNotFound,
        QualifiedTypeNotFound,
        QualifiedGenericTypeNotFound,
        UnknownPredefinedKeyword,
        FrameworkTypeNotFound,

        ArityMismatch,

        TypeArgumentAmbiguous,
        TypeArgumentFailed,

        ArrayElementFailed,
        ByRefElementFailed,
        NullableUnderlyingFailed,

        Ambiguous
    }

    public enum SubstitutionPrecedence
    {
        BinderWins = 0,
        OptionsWin = 1
    }

    public sealed class TypeResolutionOptions
    {
        /// <summary>
        /// Optional substitutions used when binding identifier type syntax (e.g. "T").
        /// If a name exists here, it wins over normal name lookup (subject to precedence).
        /// </summary>
        public IReadOnlyDictionary<string, ITypeSymbol>? TypeParameterSubstitutions { get; init; }

        /// <summary>
        /// If provided, these scopes are used for name lookup instead of the binder’s usual imports.
        /// </summary>
        public IReadOnlyList<INamespaceOrTypeSymbol>? ImportedScopesOverride { get; init; }

        public SubstitutionPrecedence SubstitutionPrecedence { get; init; } = SubstitutionPrecedence.OptionsWin;
    }

    public sealed record ResolveTypeResult
    {
        public required ITypeSymbol ResolvedType { get; init; }
        public INamedTypeSymbol? ResolvedNamedDefinition { get; init; }
        public ImmutableArray<ITypeSymbol> ResolvedTypeArguments { get; init; } = ImmutableArray<ITypeSymbol>.Empty;

        public bool IsAmbiguous { get; init; }
        public ImmutableArray<INamedTypeSymbol> AmbiguousCandidates { get; init; } = ImmutableArray<INamedTypeSymbol>.Empty;

        public bool Failed { get; init; }
        public ImmutableArray<TypeResolutionFailureKind> FailureKinds { get; init; } = ImmutableArray<TypeResolutionFailureKind>.Empty;

        public sealed record ResolutionIssue(
            TypeSyntax Syntax,
            TypeResolutionFailureKind Kind,
            ImmutableArray<INamedTypeSymbol> Candidates)
        {
            public static ResolutionIssue Failure(TypeSyntax syntax, TypeResolutionFailureKind kind) =>
                new(syntax, kind, ImmutableArray<INamedTypeSymbol>.Empty);

            public static ResolutionIssue Ambiguous(TypeSyntax syntax, IEnumerable<INamedTypeSymbol> candidates) =>
                new(syntax, TypeResolutionFailureKind.Ambiguous, candidates.ToImmutableArray());
        }

        public ImmutableArray<ResolutionIssue> Issues { get; init; } = ImmutableArray<ResolutionIssue>.Empty;

        public bool Success =>
            ResolvedType is not null &&
            ResolvedType.TypeKind != TypeKind.Error &&
            !IsAmbiguous &&
            !Failed;
    }

    // -----------------------------
    // Public/Protected entry points (Roslyn-ish)
    // -----------------------------

    public ResolveTypeResult BindType(TypeSyntax syntax)
        => BindTypeSyntax(syntax, options: null);

    public ResolveTypeResult BindType(TypeSyntax syntax, TypeResolutionOptions? options)
        => BindTypeSyntax(syntax, options);

    public ResolveTypeResult BindTypeSyntax(TypeSyntax syntax)
        => BindTypeSyntax(syntax, options: null);

    public ResolveTypeResult BindTypeSyntax(TypeSyntax syntax, TypeResolutionOptions? options)
    {
        options ??= new TypeResolutionOptions();

        // Binder-scope type parameters (method + containing types etc)
        var binderTypeParams = GetInScopeTypeParameters();

        // Merge optional substitutions
        var mergedTypeParams = MergeTypeParameterMaps(binderTypeParams, options.TypeParameterSubstitutions, options.SubstitutionPrecedence);

        // Imported scopes
        var importedScopes = options.ImportedScopesOverride ?? GetImportedScopesForTypeResolution();

        return BindTypeCore(syntax, mergedTypeParams, importedScopes);
    }

    // -----------------------------
    // Core dispatcher (unifies generic arg resolution)
    // -----------------------------

    private ResolveTypeResult BindTypeCore(
        TypeSyntax syntax,
        IReadOnlyDictionary<string, ITypeSymbol> typeParams,
        IReadOnlyList<INamespaceOrTypeSymbol> importedScopes)
    {
        return syntax switch
        {
            IdentifierNameSyntax id => BindIdentifier(id, typeParams, importedScopes),
            GenericNameSyntax g => BindGenericName(g, typeParams, importedScopes),
            QualifiedNameSyntax q => BindQualifiedName(q, typeParams, importedScopes),
            PredefinedTypeSyntax p => BindPredefined(p, importedScopes),
            ArrayTypeSyntax a => BindArray(a, typeParams, importedScopes),
            ByRefTypeSyntax br => BindByRef(br, typeParams, importedScopes),
            NullableTypeSyntax n => BindNullable(n, typeParams, importedScopes),
            _ => Fail(syntax, TypeResolutionFailureKind.UnsupportedTypeSyntax)
        };
    }

    // -----------------------------
    // Identifier type: consult merged map first, then binder name lookup
    // -----------------------------

    private ResolveTypeResult BindIdentifier(
        IdentifierNameSyntax id,
        IReadOnlyDictionary<string, ITypeSymbol> typeParams,
        IReadOnlyList<INamespaceOrTypeSymbol> importedScopes)
    {
        var name = id.Identifier.ValueText;

        if (typeParams.TryGetValue(name, out var substituted))
        {
            return new ResolveTypeResult { ResolvedType = substituted };
        }

        var lookup = LookupNamedTypeByParts(new[] { name }, importedScopes);
        if (lookup.IsAmbiguous)
            return Ambiguous(id, lookup.Candidates);

        if (lookup.Definition is null)
            return Fail(id, TypeResolutionFailureKind.TypeNotFound);

        if (lookup.Definition.Arity > 0 && lookup.Definition.IsUnboundGenericType)
        {
            return new ResolveTypeResult
            {
                ResolvedType = Compilation.ErrorTypeSymbol,
                ResolvedNamedDefinition = lookup.Definition,
                Failed = true,
                FailureKinds = ImmutableArray.Create(TypeResolutionFailureKind.ArityMismatch),
                Issues = ImmutableArray.Create(ResolveTypeResult.ResolutionIssue.Failure(id, TypeResolutionFailureKind.ArityMismatch))
            };
        }

        return new ResolveTypeResult
        {
            ResolvedType = lookup.Definition,
            ResolvedNamedDefinition = lookup.Definition
        };
    }

    // -----------------------------
    // GenericName: Foo<T>
    // -----------------------------

    private ResolveTypeResult BindGenericName(
        GenericNameSyntax g,
        IReadOnlyDictionary<string, ITypeSymbol> typeParams,
        IReadOnlyList<INamespaceOrTypeSymbol> importedScopes)
    {
        var name = g.Identifier.ValueText;

        var arity = g.TypeArgumentList.Arguments.Count;
        var lookup = LookupNamedTypeByParts(new[] { name }, importedScopes, arity);
        if (lookup.IsAmbiguous)
            return Ambiguous(g, lookup.Candidates);

        if (lookup.Definition is null)
            return Fail(g, TypeResolutionFailureKind.GenericTypeNotFound);

        if (HasOmittedTypeArguments(g.TypeArgumentList))
        {
            return new ResolveTypeResult
            {
                ResolvedType = lookup.Definition,
                ResolvedNamedDefinition = lookup.Definition
            };
        }

        var args = BindTypeArguments(g.TypeArgumentList, typeParams, importedScopes);
        if (!args.Success)
            return args;

        return Construct(lookup.Definition, args.ResolvedTypeArguments);
    }

    // -----------------------------
    // QualifiedName: A.B.C or A.B.C<T>  (critical: uses SAME arg resolver)
    // -----------------------------

    private ResolveTypeResult BindQualifiedName(
        QualifiedNameSyntax q,
        IReadOnlyDictionary<string, ITypeSymbol> typeParams,
        IReadOnlyList<INamespaceOrTypeSymbol> importedScopes)
    {
        // Fast-path: nested type lookup when the left side is a TYPE (including constructed generic types).
        // This is required for forms like `Outer<int>.Inner<string>` and `Foo<int>.Bar`.
        // If the left side resolves to a named type, we bind the right side as a nested type member.
        {
            var leftAsType = BindTypeCore((TypeSyntax)q.Left, typeParams, importedScopes);
            if (leftAsType.Success && leftAsType.ResolvedType is INamedTypeSymbol leftNamed)
            {
                // Right side: Identifier (non-generic nested type)
                if (q.Right is IdentifierNameSyntax rid)
                {
                    var nestedName = rid.Identifier.ValueText;
                    var nestedCandidates = leftNamed.GetTypeMembers(nestedName).ToArray();

                    if (nestedCandidates.Length == 1)
                    {
                        var nested = nestedCandidates[0];
                        return new ResolveTypeResult
                        {
                            ResolvedType = nested,
                            ResolvedNamedDefinition = nested
                        };
                    }

                    if (nestedCandidates.Length > 1)
                        return Ambiguous(q, nestedCandidates.ToImmutableArray());

                    // If not found as nested type, fall back to namespace-qualified lookup below.
                }

                // Right side: GenericName (generic nested type)
                if (q.Right is GenericNameSyntax rg)
                {
                    var nestedName = rg.Identifier.ValueText;
                    var nestedArity = rg.TypeArgumentList.Arguments.Count;
                    var nestedCandidates = leftNamed
                        .GetTypeMembers(nestedName)
                        .Where(t => t.Arity == nestedArity)
                        .ToArray();

                    if (nestedCandidates.Length == 1)
                    {
                        var nestedDef = nestedCandidates[0];

                        if (HasOmittedTypeArguments(rg.TypeArgumentList))
                        {
                            return new ResolveTypeResult
                            {
                                ResolvedType = nestedDef,
                                ResolvedNamedDefinition = nestedDef
                            };
                        }

                        var args = BindTypeArguments(rg.TypeArgumentList, typeParams, importedScopes);
                        if (!args.Success)
                            return args;

                        return Construct(nestedDef, args.ResolvedTypeArguments);
                    }

                    if (nestedCandidates.Length > 1)
                        return Ambiguous(q, nestedCandidates.ToImmutableArray());

                    // If not found as nested type, fall back to namespace-qualified lookup below.
                }
            }
        }

        if (q.Right is GenericNameSyntax g)
        {
            var left = Flatten(q.Left);
            var parts = left.Concat(new[] { g.Identifier.ValueText }).ToArray();

            var arity = g.TypeArgumentList.Arguments.Count;
            var lookup = LookupNamedTypeByParts(parts, importedScopes, arity);
            if (lookup.IsAmbiguous)
                return Ambiguous(q, lookup.Candidates);

            if (lookup.Definition is null)
                return Fail(q, TypeResolutionFailureKind.QualifiedGenericTypeNotFound);

            if (HasOmittedTypeArguments(g.TypeArgumentList))
            {
                return new ResolveTypeResult
                {
                    ResolvedType = lookup.Definition,
                    ResolvedNamedDefinition = lookup.Definition
                };
            }

            // ✅ same arg resolution as GenericName
            var args = BindTypeArguments(g.TypeArgumentList, typeParams, importedScopes);
            if (!args.Success)
                return args;

            return Construct(lookup.Definition, args.ResolvedTypeArguments);
        }

        var nameParts = Flatten(q);

        var lookup2 = LookupNamedTypeByParts(nameParts, importedScopes);
        if (lookup2.IsAmbiguous)
            return Ambiguous(q, lookup2.Candidates);

        if (lookup2.Definition is null)
            return Fail(q, TypeResolutionFailureKind.QualifiedTypeNotFound);

        return new ResolveTypeResult
        {
            ResolvedType = lookup2.Definition,
            ResolvedNamedDefinition = lookup2.Definition
        };
    }

    // -----------------------------
    // Predefined
    // -----------------------------

    private ResolveTypeResult BindPredefined(
        PredefinedTypeSyntax p,
        IReadOnlyList<INamespaceOrTypeSymbol> importedScopes)
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
            return Fail(p, TypeResolutionFailureKind.UnknownPredefinedKeyword);

        var lookup = LookupNamedTypeByParts(parts, importedScopes);
        if (lookup.IsAmbiguous)
            return Ambiguous(p, lookup.Candidates);

        if (lookup.Definition is null)
            return Fail(p, TypeResolutionFailureKind.FrameworkTypeNotFound);

        return new ResolveTypeResult
        {
            ResolvedType = lookup.Definition,
            ResolvedNamedDefinition = lookup.Definition
        };
    }

    // -----------------------------
    // Wrappers (recurse through BindTypeCore, preserving params/imports)
    // -----------------------------

    private ResolveTypeResult BindArray(
        ArrayTypeSyntax a,
        IReadOnlyDictionary<string, ITypeSymbol> typeParams,
        IReadOnlyList<INamespaceOrTypeSymbol> importedScopes)
    {
        var element = BindTypeCore(a.ElementType, typeParams, importedScopes);
        if (!element.Success)
            return element with
            {
                Failed = true,
                FailureKinds = element.FailureKinds.Add(TypeResolutionFailureKind.ArrayElementFailed),
                Issues = element.Issues.Add(ResolveTypeResult.ResolutionIssue.Failure(a.ElementType, TypeResolutionFailureKind.ArrayElementFailed))
            };

        return new ResolveTypeResult { ResolvedType = Compilation.CreateArrayTypeSymbol(element.ResolvedType, a.RankSpecifiers.Count) };
    }

    private ResolveTypeResult BindByRef(
        ByRefTypeSyntax br,
        IReadOnlyDictionary<string, ITypeSymbol> typeParams,
        IReadOnlyList<INamespaceOrTypeSymbol> importedScopes)
    {
        var element = BindTypeCore(br.ElementType, typeParams, importedScopes);
        if (!element.Success)
            return element with
            {
                Failed = true,
                FailureKinds = element.FailureKinds.Add(TypeResolutionFailureKind.ByRefElementFailed),
                Issues = element.Issues.Add(ResolveTypeResult.ResolutionIssue.Failure(br.ElementType, TypeResolutionFailureKind.ByRefElementFailed))
            };

        return new ResolveTypeResult { ResolvedType = new ByRefTypeSymbol(element.ResolvedType) };
    }

    private ResolveTypeResult BindNullable(
        NullableTypeSyntax n,
        IReadOnlyDictionary<string, ITypeSymbol> typeParams,
        IReadOnlyList<INamespaceOrTypeSymbol> importedScopes)
    {
        var underlying = BindTypeCore(n.ElementType, typeParams, importedScopes);
        if (!underlying.Success)
            return underlying with
            {
                Failed = true,
                FailureKinds = underlying.FailureKinds.Add(TypeResolutionFailureKind.NullableUnderlyingFailed),
                Issues = underlying.Issues.Add(ResolveTypeResult.ResolutionIssue.Failure(n.ElementType, TypeResolutionFailureKind.NullableUnderlyingFailed))
            };

        return new ResolveTypeResult { ResolvedType = underlying.ResolvedType.MakeNullable() };
    }

    // -----------------------------
    // Type arguments (shared, binder-aware, options-preserving)
    // -----------------------------

    private ResolveTypeResult BindTypeArguments(
        TypeArgumentListSyntax list,
        IReadOnlyDictionary<string, ITypeSymbol> typeParams,
        IReadOnlyList<INamespaceOrTypeSymbol> importedScopes)
    {
        var failures = ImmutableArray.CreateBuilder<TypeResolutionFailureKind>();
        var issues = ImmutableArray.CreateBuilder<ResolveTypeResult.ResolutionIssue>();
        var args = ImmutableArray.CreateBuilder<ITypeSymbol>(list.Arguments.Count);

        for (int i = 0; i < list.Arguments.Count; i++)
        {
            var argSyntax = list.Arguments[i].Type;
            var resolved = BindTypeCore(argSyntax, typeParams, importedScopes);

            if (resolved.IsAmbiguous)
            {
                failures.Add(TypeResolutionFailureKind.TypeArgumentAmbiguous);
                issues.Add(ResolveTypeResult.ResolutionIssue.Ambiguous(argSyntax, resolved.AmbiguousCandidates));
                continue;
            }

            if (!resolved.Success)
            {
                failures.Add(TypeResolutionFailureKind.TypeArgumentFailed);
                issues.Add(ResolveTypeResult.ResolutionIssue.Failure(argSyntax, TypeResolutionFailureKind.TypeArgumentFailed));
                if (!resolved.Issues.IsDefaultOrEmpty) issues.AddRange(resolved.Issues);
                if (!resolved.FailureKinds.IsDefaultOrEmpty) failures.AddRange(resolved.FailureKinds);
                continue;
            }

            args.Add(resolved.ResolvedType);
        }

        if (failures.Count > 0)
        {
            return new ResolveTypeResult
            {
                ResolvedType = Compilation.ErrorTypeSymbol,
                Failed = true,
                FailureKinds = failures.ToImmutable(),
                Issues = issues.ToImmutable()
            };
        }

        // Note: this result is used as an intermediate carrier for resolved type arguments.
        // Callers construct the final named/constructed type, so `ResolvedType` must be a
        // non-error placeholder to make `ResolveTypeResult.Success` true.
        return new ResolveTypeResult
        {
            ResolvedType = Compilation.GetSpecialType(SpecialType.System_Object),
            ResolvedTypeArguments = args.ToImmutable()
        };
    }

    private static bool HasOmittedTypeArguments(TypeArgumentListSyntax list)
    {
        if (list.Arguments.Count == 0)
            return false;

        foreach (var argument in list.Arguments)
        {
            if (!argument.Type.IsMissing)
                return false;
        }

        return true;
    }

    // -----------------------------
    // Construct + helpers
    // -----------------------------

    private ResolveTypeResult Construct(INamedTypeSymbol def, ImmutableArray<ITypeSymbol> args)
    {
        // IMPORTANT: For nested types found on a constructed containing type (e.g. Outer<int>.Inner<B>),
        // we must preserve the containing-type context when constructing the nested type.
        // Using ConstructedFrom/OriginalDefinition can drop the containing override and revert to
        // Outer<A>.Inner<B>, which then breaks substitution and display.
        INamedTypeSymbol definition;

        if (def.ContainingType is null)
        {
            // Non-nested type: normalize to the unconstructed definition.
            definition =
                def.ConstructedFrom as INamedTypeSymbol ??
                def.OriginalDefinition as INamedTypeSymbol ??
                def;
        }
        else
        {
            // Nested type: only normalize if the containing type is itself not constructed.
            // If the containing type is constructed, keep `def` so we retain the containing override.
            if (def.ContainingType is INamedTypeSymbol ct &&
                !ct.TypeArguments.IsDefaultOrEmpty &&
                ct.TypeArguments.Length > 0 &&
                !SymbolEqualityComparer.Default.Equals(ct, ct.OriginalDefinition))
            {
                definition = def;
            }
            else
            {
                definition =
                    def.ConstructedFrom as INamedTypeSymbol ??
                    def.OriginalDefinition as INamedTypeSymbol ??
                    def;
            }
        }

        if (definition.TypeParameters.Length != args.Length)
        {
            return new ResolveTypeResult
            {
                ResolvedType = Compilation.ErrorTypeSymbol,
                ResolvedNamedDefinition = definition,
                ResolvedTypeArguments = args,
                Failed = true,
                FailureKinds = ImmutableArray.Create(TypeResolutionFailureKind.ArityMismatch)
            };
        }

        return new ResolveTypeResult
        {
            ResolvedType = definition.Construct(args.ToArray()),
            ResolvedNamedDefinition = definition,
            ResolvedTypeArguments = args
        };
    }

    private ResolveTypeResult Ambiguous(TypeSyntax syntax, ImmutableArray<INamedTypeSymbol> candidates)
    {
        return new ResolveTypeResult
        {
            ResolvedType = Compilation.ErrorTypeSymbol,
            IsAmbiguous = true,
            AmbiguousCandidates = candidates,
            Failed = true,
            FailureKinds = ImmutableArray.Create(TypeResolutionFailureKind.Ambiguous),
            Issues = ImmutableArray.Create(ResolveTypeResult.ResolutionIssue.Ambiguous(syntax, candidates))
        };
    }

    private ResolveTypeResult Fail(TypeSyntax syntax, TypeResolutionFailureKind kind)
    {
        return new ResolveTypeResult
        {
            ResolvedType = Compilation.ErrorTypeSymbol,
            Failed = true,
            FailureKinds = ImmutableArray.Create(kind),
            Issues = ImmutableArray.Create(ResolveTypeResult.ResolutionIssue.Failure(syntax, kind))
        };
    }

    private static IReadOnlyDictionary<string, ITypeSymbol> MergeTypeParameterMaps(
        IReadOnlyDictionary<string, ITypeSymbol> binderMap,
        IReadOnlyDictionary<string, ITypeSymbol>? optionMap,
        SubstitutionPrecedence precedence)
    {
        if (optionMap is null || optionMap.Count == 0)
            return binderMap;

        var merged = new Dictionary<string, ITypeSymbol>(binderMap, StringComparer.Ordinal);

        if (precedence == SubstitutionPrecedence.BinderWins)
        {
            foreach (var kv in optionMap)
                merged.TryAdd(kv.Key, kv.Value);
        }
        else
        {
            foreach (var kv in optionMap)
                merged[kv.Key] = kv.Value;
        }

        return merged;
    }

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
    // Default hooks (can be overridden by specialized binders)
    // -----------------------------

    /// <summary>
    /// Returns a map of in-scope type parameters keyed by name.
    /// Default implementation collects type parameters from the containing symbol chain.
    /// </summary>
    protected virtual IReadOnlyDictionary<string, ITypeSymbol> GetInScopeTypeParameters()
    {
        var map = new Dictionary<string, ITypeSymbol>(StringComparer.Ordinal);

        // Walk containing symbols (method -> type -> namespace/assembly)
        for (ISymbol? symbol = ContainingSymbol; symbol is not null; symbol = symbol.ContainingSymbol)
        {
            if (symbol is IMethodSymbol m && !m.TypeParameters.IsDefaultOrEmpty)
            {
                foreach (var tp in m.TypeParameters)
                    map.TryAdd(tp.Name, tp);
            }

            if (symbol is INamedTypeSymbol nt && !nt.TypeParameters.IsDefaultOrEmpty)
            {
                foreach (var tp in nt.TypeParameters)
                    map.TryAdd(tp.Name, tp);
            }
        }

        return map;
    }

    /// <summary>
    /// Returns the scopes used for type name lookup.
    /// Default implementation includes the current namespace (if any) and the global namespace.
    /// Specialized binders (e.g. ImportBinder) should override to include imported namespaces/types.
    /// </summary>
    protected virtual IReadOnlyList<INamespaceOrTypeSymbol> GetImportedScopesForTypeResolution()
    {
        // If a more-derived binder (ImportBinder) has better info, it should override.
        // If not overridden, fall back to parent binder’s scopes (so scopes accumulate).
        var scopes = ParentBinder?.GetImportedScopesForTypeResolution()?.ToList()
                     ?? new List<INamespaceOrTypeSymbol>(capacity: 2);

        // Ensure current namespace is present once.
        if (CurrentNamespace is not null && !scopes.Contains(CurrentNamespace, SymbolEqualityComparer.Default))
            scopes.Add(CurrentNamespace);

        // Ensure global namespace is always present once.
        if (!scopes.Contains(Compilation.GlobalNamespace, SymbolEqualityComparer.Default))
            scopes.Add(Compilation.GlobalNamespace);

        return scopes;
    }

    /// <summary>
    /// Lookup a named type by a sequence of name parts (e.g. ["System","Collections","Generic","IEnumerable"]).
    /// The search starts from each provided scope; ambiguity is reported if multiple distinct types match.
    /// </summary>
    protected virtual (INamedTypeSymbol? Definition, bool IsAmbiguous, ImmutableArray<INamedTypeSymbol> Candidates)
        LookupNamedTypeByParts(string[] parts, IReadOnlyList<INamespaceOrTypeSymbol> importedScopes, int? arity = null)
    {
        if (parts is null || parts.Length == 0)
            return (null, false, ImmutableArray<INamedTypeSymbol>.Empty);

        var candidates = ImmutableArray.CreateBuilder<INamedTypeSymbol>();
        var seen = new HashSet<INamedTypeSymbol>(SymbolEqualityComparer.Default);

        foreach (var scope in importedScopes)
        {
            var resolved = BindFromScope(scope, parts, arity);
            if (resolved is null)
                continue;

            if (seen.Add(resolved))
                candidates.Add(resolved);
        }

        if (candidates.Count == 1)
            return (candidates[0], false, candidates.ToImmutable());

        if (candidates.Count > 1)
            return (null, true, candidates.ToImmutable());

        // Fallback: try metadata lookup for fully qualified names.
        // Note: This cannot reliably represent nested types without '+' separators,
        // but helps for common namespace-qualified types.
        var metadataName = string.Join(".", parts);
        if (arity is not null)
            metadataName += $"`{arity.Value}";
        var metadataType = Compilation.GetTypeByMetadataName(metadataName);
        if (metadataType is not null)
            return (metadataType, false, ImmutableArray.Create(metadataType));

        return (null, false, ImmutableArray<INamedTypeSymbol>.Empty);

        INamedTypeSymbol? BindFromScope(INamespaceOrTypeSymbol start, string[] nameParts, int? arity)
        {
            INamespaceOrTypeSymbol? current = start;

            // If the start is a namespace, we can walk namespaces/types.
            // If it's a type, we walk nested types.
            for (int i = 0; i < nameParts.Length; i++)
            {
                var part = nameParts[i];
                if (string.IsNullOrEmpty(part))
                    return null;

                if (current is INamespaceSymbol ns)
                {
                    // Prefer namespace continuation if possible
                    var nextNs = ns.LookupNamespace(part) ?? ns.GetMembers(part).OfType<INamespaceSymbol>().FirstOrDefault();
                    if (nextNs is not null)
                    {
                        current = nextNs;
                        continue;
                    }

                    // Otherwise resolve as a type in this namespace
                    var named = ns.GetMembers(part).OfType<INamedTypeSymbol>().ToArray();
                    if (arity is not null && i == nameParts.Length - 1)
                        named = named.Where(t => t.Arity == arity.Value).ToArray();
                    if (named.Length == 0)
                    {
                        var viaLookupType = ns.LookupType(part);
                        if (viaLookupType is INamedTypeSymbol lt)
                        {
                            var normalized = NormalizeDefinition(lt);
                            if (arity is not null && i == nameParts.Length - 1 && normalized.Arity != arity.Value)
                            {
                                // Mismatched generic arity for the requested terminal segment.
                            }
                            else
                            {
                                current = normalized;
                                continue;
                            }
                        }

                        if (i == nameParts.Length - 1)
                        {
                            var namespaceName = ns.ToString();
                            var metadataName = string.IsNullOrEmpty(namespaceName)
                                ? part
                                : namespaceName + "." + part;
                            if (arity is not null)
                                metadataName += $"`{arity.Value}";

                            if (Compilation.GetTypeByMetadataName(metadataName) is INamedTypeSymbol metadataMatch)
                            {
                                current = metadataMatch;
                                continue;
                            }
                        }

                        return null;
                    }

                    if (named.Length == 1)
                    {
                        current = named[0];
                        continue;
                    }

                    // Multiple types with the same name in the same namespace (possible with metadata + source merges).
                    // Treat as ambiguous by returning null here; caller aggregates ambiguity.
                    return null;
                }

                if (current is ITypeSymbol type)
                {
                    var nested = type.GetMembers(part).OfType<INamedTypeSymbol>().ToArray();
                    if (arity is not null && i == nameParts.Length - 1)
                        nested = nested.Where(t => t.Arity == arity.Value).ToArray();
                    if (nested.Length == 1)
                    {
                        current = nested[0];
                        continue;
                    }

                    // Not found or ambiguous
                    return null;
                }

                return null;
            }

            return current as INamedTypeSymbol;
        }
    }

    internal bool TryResolveNamedTypeFromTypeSyntax(TypeSyntax syntax, out INamedTypeSymbol? namedType)
    {
        if (TryBindNamedTypeFromTypeSyntax(syntax, out namedType))
            return true;

        namedType = BindTypeSyntaxDirect(syntax) as INamedTypeSymbol;
        return namedType is not null;
    }

    internal bool TryBindNamedTypeFromTypeSyntax(
        TypeSyntax syntax,
        out INamedTypeSymbol? namedType,
        TypeResolutionOptions? options = null,
        bool reportDiagnostics = false)
    {
        ResolveTypeResult result;
        if (reportDiagnostics)
        {
            result = BindTypeSyntax(syntax, options);
        }
        else
        {
            using (_diagnostics.CreateNonReportingScope())
                result = BindTypeSyntax(syntax, options);
        }

        if (result.Success && result.ResolvedType is INamedTypeSymbol boundNamedType)
        {
            namedType = boundNamedType;
            return true;
        }

        if (reportDiagnostics)
            ReportResolveTypeResultDiagnostics(result, syntax);

        namedType = null;
        return false;
    }
}
