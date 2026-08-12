using Mono.Cecil;
using Mono.Cecil.Cil;

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
        var coreLibRefs = module.AssemblyReferences
            .Where(reference => string.Equals(reference.Name, "System.Private.CoreLib", StringComparison.OrdinalIgnoreCase))
            .ToArray();

        if (coreLibRefs.Length == 0)
        {
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

        assembly.Write(peOutput, CreateWriterParameters(pdbOutput));
    }

    internal static void RetargetCoreLibraryReference(
        Stream peInput,
        Stream peOutput,
        AssemblyNameReference targetCoreLibrary,
        IAssemblyResolver? assemblyResolver = null,
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
