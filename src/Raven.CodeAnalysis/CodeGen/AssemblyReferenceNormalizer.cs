using Mono.Cecil;
using Mono.Cecil.Cil;

using Raven.CodeAnalysis.Symbols;

namespace Raven.CodeAnalysis.CodeGen;

internal static class AssemblyReferenceNormalizer
{
    private static readonly byte[] SystemRuntimePublicKeyToken = [0xb0, 0x3f, 0x5f, 0x7f, 0x11, 0xd5, 0x0a, 0x3a];
    private static readonly string[] KeepCoreLibNamespacePrefixes =
    [
        "System.Collections"
    ];

    internal static void NormalizeCoreLibReference(
        Stream peInput,
        Stream peOutput,
        IAssemblyResolver? assemblyResolver = null,
        IReadOnlyDictionary<string, AssemblyNameReference>? targetReferences = null,
        IReadOnlyDictionary<string, IMethodSymbol>? metadataMethodProxies = null,
        Stream? pdbInput = null,
        Stream? pdbOutput = null)
    {
        if (peInput is null)
            throw new ArgumentNullException(nameof(peInput));
        if (peOutput is null)
            throw new ArgumentNullException(nameof(peOutput));

        peInput.Position = 0;

        var readerParameters = new ReaderParameters
        {
            InMemory = true,
            ReadingMode = ReadingMode.Deferred
        };
        ConfigureSymbols(readerParameters, pdbInput, pdbOutput);
        if (assemblyResolver is not null)
            readerParameters.AssemblyResolver = assemblyResolver;

        var assembly = AssemblyDefinition.ReadAssembly(peInput, readerParameters);

        var module = assembly.MainModule;
        var rewroteMetadataMethods = metadataMethodProxies is { Count: > 0 };
        RewriteMetadataMethodProxies(module, metadataMethodProxies, targetReferences);
        var coreLibRefs = module.AssemblyReferences
            .Where(reference => string.Equals(reference.Name, "System.Private.CoreLib", StringComparison.OrdinalIgnoreCase))
            .ToArray();

        if (coreLibRefs.Length == 0)
        {
            if (rewroteMetadataMethods || targetReferences is { Count: > 0 })
            {
                RetargetAssemblyIdentities(module, targetReferences);
                assembly.Write(peOutput, CreateWriterParameters(pdbOutput));
                return;
            }

            peInput.Position = 0;
            peInput.CopyTo(peOutput);
            CopySymbols(pdbInput, pdbOutput);
            return;
        }

        var runtimeRef = module.AssemblyReferences
            .FirstOrDefault(reference => string.Equals(reference.Name, "System.Runtime", StringComparison.OrdinalIgnoreCase));

        foreach (var coreLibRef in coreLibRefs)
        {
            runtimeRef ??= CreateSystemRuntimeReference(coreLibRef);
            RewriteReferenceScope(module, coreLibRef, runtimeRef);

            if (!ModuleStillUsesReference(module, coreLibRef))
                module.AssemblyReferences.Remove(coreLibRef);
        }

        if (!module.AssemblyReferences.Contains(runtimeRef))
            module.AssemblyReferences.Add(runtimeRef);

        RetargetAssemblyIdentities(module, targetReferences);
        assembly.Write(peOutput, CreateWriterParameters(pdbOutput));
    }

    internal static void RetargetAssemblyReferences(
        Stream peInput,
        Stream peOutput,
        IReadOnlyDictionary<string, AssemblyNameReference> targetReferences,
        IAssemblyResolver? assemblyResolver = null,
        Stream? pdbInput = null,
        Stream? pdbOutput = null)
    {
        ArgumentNullException.ThrowIfNull(peInput);
        ArgumentNullException.ThrowIfNull(peOutput);
        ArgumentNullException.ThrowIfNull(targetReferences);

        peInput.Position = 0;
        var readerParameters = new ReaderParameters
        {
            InMemory = true,
            ReadingMode = ReadingMode.Deferred
        };
        ConfigureSymbols(readerParameters, pdbInput, pdbOutput);
        if (assemblyResolver is not null)
            readerParameters.AssemblyResolver = assemblyResolver;

        var assembly = AssemblyDefinition.ReadAssembly(peInput, readerParameters);
        RetargetAssemblyIdentities(assembly.MainModule, targetReferences);
        assembly.Write(peOutput, CreateWriterParameters(pdbOutput));
    }

    internal static void RetargetCoreLibraryReference(
        Stream peInput,
        Stream peOutput,
        AssemblyNameReference targetCoreLibrary,
        IAssemblyResolver? assemblyResolver = null,
        IReadOnlyDictionary<string, AssemblyNameReference>? targetReferences = null,
        IReadOnlyDictionary<string, IMethodSymbol>? metadataMethodProxies = null,
        Stream? pdbInput = null,
        Stream? pdbOutput = null)
    {
        ArgumentNullException.ThrowIfNull(peInput);
        ArgumentNullException.ThrowIfNull(peOutput);
        ArgumentNullException.ThrowIfNull(targetCoreLibrary);

        peInput.Position = 0;

        var readerParameters = new ReaderParameters
        {
            InMemory = true,
            ReadingMode = ReadingMode.Deferred
        };
        ConfigureSymbols(readerParameters, pdbInput, pdbOutput);
        if (assemblyResolver is not null)
            readerParameters.AssemblyResolver = assemblyResolver;

        var assembly = AssemblyDefinition.ReadAssembly(peInput, readerParameters);
        var module = assembly.MainModule;
        RewriteMetadataMethodProxies(module, metadataMethodProxies, targetReferences);
        RetargetAssemblyIdentities(module, targetReferences);
        var targetReference = module.AssemblyReferences.FirstOrDefault(reference =>
            string.Equals(reference.FullName, targetCoreLibrary.FullName, StringComparison.OrdinalIgnoreCase));
        targetReference ??= CloneAssemblyReference(targetCoreLibrary);

        var sourceReferences = module.AssemblyReferences
            .Where(reference =>
                !ReferenceEquals(reference, targetReference) &&
                (string.Equals(reference.Name, "System.Private.CoreLib", StringComparison.OrdinalIgnoreCase) ||
                 string.Equals(reference.Name, "System.Runtime", StringComparison.OrdinalIgnoreCase)))
            .ToArray();

        foreach (var sourceReference in sourceReferences)
        {
            RewriteReferenceScope(module, sourceReference, targetReference, rewriteAll: true);

            if (!ModuleStillUsesReference(module, sourceReference))
                module.AssemblyReferences.Remove(sourceReference);
        }

        if (sourceReferences.Length > 0 && !module.AssemblyReferences.Contains(targetReference))
            module.AssemblyReferences.Add(targetReference);

        assembly.Write(peOutput, CreateWriterParameters(pdbOutput));
    }

    private static void RewriteMetadataMethodProxies(
        ModuleDefinition module,
        IReadOnlyDictionary<string, IMethodSymbol>? proxies,
        IReadOnlyDictionary<string, AssemblyNameReference>? targetReferences)
    {
        if (proxies is null || proxies.Count == 0)
            return;

        var proxyType = module.Types.FirstOrDefault(type => type.Name == "<RavenMetadataMethodReferences>");
        if (proxyType is null)
            throw new InvalidOperationException("Metadata method proxy type was not emitted.");

        var replacements = proxies.ToDictionary(
            pair => pair.Key,
            pair => CreateMethodReference(module, pair.Value, targetReferences),
            StringComparer.Ordinal);

        foreach (var type in EnumerateTypes(module.Types))
        {
            foreach (var method in type.Methods)
            {
                if (!method.HasBody)
                    continue;

                foreach (var instruction in method.Body.Instructions)
                {
                    if (instruction.Operand is MethodReference operand &&
                        operand.DeclaringType.Name == proxyType.Name &&
                        replacements.TryGetValue(operand.Name, out var replacement))
                    {
                        instruction.Operand = replacement;
                    }
                }
            }
        }

        module.Types.Remove(proxyType);
    }

    private static MethodReference CreateMethodReference(
        ModuleDefinition module,
        IMethodSymbol method,
        IReadOnlyDictionary<string, AssemblyNameReference>? targetReferences)
    {
        var reference = new MethodReference(
            method.MetadataName,
            CreateTypeReference(module, method.ReturnType, targetReferences),
            CreateTypeReference(module, method.ContainingType!, targetReferences))
        {
            HasThis = !method.IsStatic,
            ExplicitThis = false
        };

        foreach (var parameter in method.Parameters)
        {
            var parameterType = CreateTypeReference(module, parameter.Type, targetReferences);
            if (parameter.RefKind is RefKind.Ref or RefKind.Out or RefKind.In)
                parameterType = new ByReferenceType(parameterType);
            reference.Parameters.Add(new ParameterDefinition(parameterType));
        }

        return reference;
    }

    private static TypeReference CreateTypeReference(
        ModuleDefinition module,
        ITypeSymbol symbol,
        IReadOnlyDictionary<string, AssemblyNameReference>? targetReferences)
    {
        if (symbol is NullableTypeSymbol nullable)
        {
            var underlying = CreateTypeReference(module, nullable.UnderlyingType, targetReferences);
            if (nullable.GetNullableAbiProjection() != NullableAbiProjection.NullableValueType)
                return underlying;

            var nullableDefinition = new TypeReference("System", "Nullable`1", module, underlying.Scope)
            {
                IsValueType = true
            };
            var constructedNullable = new GenericInstanceType(nullableDefinition);
            constructedNullable.GenericArguments.Add(underlying);
            return constructedNullable;
        }

        if (symbol is IArrayTypeSymbol array)
            return new ArrayType(CreateTypeReference(module, array.ElementType, targetReferences), array.Rank);

        if (symbol is ConstructedNamedTypeSymbol constructed &&
            constructed.ConstructedFrom is INamedTypeSymbol definition &&
            !SymbolEqualityComparer.Default.Equals(constructed, definition))
        {
            var generic = new GenericInstanceType(CreateTypeReference(module, definition, targetReferences));
            foreach (var argument in constructed.TypeArguments)
                generic.GenericArguments.Add(CreateTypeReference(module, argument, targetReferences));
            return generic;
        }

        if (symbol is not INamedTypeSymbol named)
            throw new NotSupportedException($"Metadata emission does not yet support type '{symbol}'.");

        IMetadataScope scope = module;
        if (named.ContainingAssembly is { } assembly)
        {
            if (targetReferences is null || !targetReferences.TryGetValue(assembly.Name, out var targetReference))
                targetReference = new AssemblyNameReference(assembly.Name, new Version(0, 0, 0, 0));

            var existing = module.AssemblyReferences.FirstOrDefault(candidate =>
                string.Equals(candidate.Name, targetReference.Name, StringComparison.OrdinalIgnoreCase));
            if (existing is null)
            {
                existing = CloneAssemblyReference(targetReference);
                module.AssemblyReferences.Add(existing);
            }

            scope = existing;
        }

        if (named.ContainingType is { } containingType)
        {
            return new TypeReference(string.Empty, named.MetadataName, module, scope)
            {
                DeclaringType = CreateTypeReference(module, containingType, targetReferences)
            };
        }

        var fullMetadataName = named.ToFullyQualifiedMetadataName();
        var namespaceSeparator = fullMetadataName.LastIndexOf('.');
        var namespaceName = namespaceSeparator >= 0
            ? fullMetadataName[..namespaceSeparator]
            : string.Empty;
        var typeName = namespaceSeparator >= 0
            ? fullMetadataName[(namespaceSeparator + 1)..]
            : fullMetadataName;

        return new TypeReference(namespaceName, typeName, module, scope)
        {
            IsValueType = named.IsValueType
        };
    }

    private static IEnumerable<TypeDefinition> EnumerateTypes(IEnumerable<TypeDefinition> types)
    {
        foreach (var type in types)
        {
            yield return type;
            foreach (var nested in EnumerateTypes(type.NestedTypes))
                yield return nested;
        }
    }

    private static void RetargetAssemblyIdentities(
        ModuleDefinition module,
        IReadOnlyDictionary<string, AssemblyNameReference>? targetReferences)
    {
        if (targetReferences is null || targetReferences.Count == 0)
            return;

        foreach (var reference in module.AssemblyReferences)
        {
            if (!targetReferences.TryGetValue(reference.Name, out var targetReference))
                continue;

            reference.Version = targetReference.Version;
            reference.Culture = targetReference.Culture;
            reference.IsRetargetable = targetReference.IsRetargetable;
            reference.IsWindowsRuntime = targetReference.IsWindowsRuntime;
            reference.HasPublicKey = targetReference.HasPublicKey;
            if (targetReference.HasPublicKey)
            {
                reference.PublicKey = [.. targetReference.PublicKey];
                reference.PublicKeyToken = null;
            }
            else
            {
                reference.PublicKey = null;
                reference.PublicKeyToken = targetReference.PublicKeyToken is { Length: > 0 }
                    ? [.. targetReference.PublicKeyToken]
                    : null;
            }
        }
    }

    private static void ConfigureSymbols(
        ReaderParameters readerParameters,
        Stream? pdbInput,
        Stream? pdbOutput)
    {
        if (pdbInput is null || pdbOutput is null)
            return;

        pdbInput.Position = 0;
        readerParameters.ReadSymbols = true;
        readerParameters.SymbolReaderProvider = new PortablePdbReaderProvider();
        readerParameters.SymbolStream = pdbInput;
    }

    private static WriterParameters CreateWriterParameters(Stream? pdbOutput)
        => pdbOutput is null
            ? new WriterParameters()
            : new WriterParameters
            {
                WriteSymbols = true,
                SymbolWriterProvider = new PortablePdbWriterProvider(),
                SymbolStream = pdbOutput
            };

    private static void CopySymbols(Stream? pdbInput, Stream? pdbOutput)
    {
        if (pdbInput is null || pdbOutput is null)
            return;

        pdbInput.Position = 0;
        pdbInput.CopyTo(pdbOutput);
    }

    private static AssemblyNameReference CloneAssemblyReference(AssemblyNameReference source)
    {
        var clone = new AssemblyNameReference(source.Name, source.Version)
        {
            Culture = source.Culture,
            HasPublicKey = source.HasPublicKey,
            IsRetargetable = source.IsRetargetable,
            IsWindowsRuntime = source.IsWindowsRuntime
        };

        if (source.HasPublicKey)
            clone.PublicKey = [.. source.PublicKey];
        else
            clone.PublicKeyToken = [.. source.PublicKeyToken];

        return clone;
    }

    private static AssemblyNameReference CreateSystemRuntimeReference(AssemblyNameReference sourceReference)
    {
        var runtimeReference = new AssemblyNameReference("System.Runtime", sourceReference.Version)
        {
            Culture = sourceReference.Culture,
            HasPublicKey = false
        };

        runtimeReference.PublicKeyToken = [.. SystemRuntimePublicKeyToken];
        return runtimeReference;
    }

    private static void RewriteReferenceScope(
        ModuleDefinition module,
        AssemblyNameReference oldReference,
        AssemblyNameReference newReference,
        bool rewriteAll = false)
    {
        foreach (var typeReference in module.GetTypeReferences())
        {
            RewriteTypeReferenceScope(typeReference, oldReference, newReference, rewriteAll);
        }

        foreach (var memberReference in module.GetMemberReferences())
        {
            if (memberReference.DeclaringType is { } declaringType)
                RewriteTypeReferenceScope(declaringType, oldReference, newReference, rewriteAll);
        }
    }

    private static void RewriteTypeReferenceScope(
        TypeReference typeReference,
        AssemblyNameReference oldReference,
        AssemblyNameReference newReference,
        bool rewriteAll)
    {
        var innermost = GetInnermostTypeReference(typeReference);
        if (ReferenceEquals(innermost.Scope, oldReference) &&
            (rewriteAll || ShouldRewriteToSystemRuntime(typeReference)))
        {
            innermost.Scope = newReference;
        }
    }

    private static bool ShouldRewriteToSystemRuntime(TypeReference typeReference)
    {
        var namespaceName = GetInnermostTypeNamespace(typeReference);

        foreach (var prefix in KeepCoreLibNamespacePrefixes)
        {
            if (namespaceName.StartsWith(prefix, StringComparison.Ordinal))
                return false;
        }

        return true;
    }

    private static string GetInnermostTypeNamespace(TypeReference typeReference)
        => GetInnermostTypeReference(typeReference).Namespace ?? string.Empty;

    private static TypeReference GetInnermostTypeReference(TypeReference typeReference)
    {
        var current = typeReference;
        while (current is TypeSpecification specification)
            current = specification.ElementType;

        return current;
    }

    private static bool ModuleStillUsesReference(ModuleDefinition module, AssemblyNameReference reference)
    {
        foreach (var typeReference in module.GetTypeReferences())
        {
            if (ReferenceEquals(GetInnermostTypeReference(typeReference).Scope, reference))
                return true;
        }

        foreach (var memberReference in module.GetMemberReferences())
        {
            if (memberReference.DeclaringType is { } declaringType &&
                ReferenceEquals(GetInnermostTypeReference(declaringType).Scope, reference))
                return true;
        }

        return false;
    }
}
