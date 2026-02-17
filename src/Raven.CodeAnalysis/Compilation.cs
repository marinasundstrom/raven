using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.IO;
using System.Linq;
using System.Reflection;
using System.Runtime.Loader;
using System.Threading;

using Raven.CodeAnalysis.Symbols;
using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis;

public partial class Compilation
{
    private readonly object _setupLock = new();
    private INamespaceSymbol? _globalNamespace;
    private readonly SyntaxTree[] _syntaxTrees;
    private readonly MetadataReference[] _references;
    internal SyntaxTree? SyntaxTreeWithFileScopedCode;
    private readonly Dictionary<MetadataReference, IAssemblySymbol> _metadataReferenceSymbols = new();
    private readonly Dictionary<Assembly, IAssemblySymbol> _assemblySymbols = new();
    private readonly Dictionary<string, Assembly> _lazyMetadataAssemblies = new();
    private readonly Dictionary<string, string> _assemblyPathMap = new(StringComparer.OrdinalIgnoreCase);
    private readonly Dictionary<Assembly, Assembly> _metadataToRuntimeAssemblyMap = new();
    private readonly Dictionary<string, Assembly> _runtimeAssemblyCache = new(StringComparer.OrdinalIgnoreCase);
    private bool _trustedPlatformAssembliesCached;
    private MetadataLoadContext _metadataLoadContext;
    private GlobalBinder _globalBinder;
    private bool setup;
    private ErrorTypeSymbol _errorTypeSymbol;
    private NullTypeSymbol _nullTypeSymbol;
    private UnitTypeSymbol _unitTypeSymbol;
    private ReflectionTypeLoader _reflectionTypeLoader;
    private bool _sourceTypesInitialized;
    private bool _isPopulatingSourceTypes;
    private readonly object _declarationGate = new();
    private bool _sourceDeclarationsComplete;
    private bool _isDeclaringSourceTypes;
    private readonly Dictionary<SyntaxTree, TopLevelProgramMembers> _topLevelProgramMembers = new();
    private BoundNodeFactory? _boundNodeFactory;
    private ErrorSymbol _errorSymbol;
    private bool isSettingUp;

    private Compilation(string? assemblyName, SyntaxTree[] syntaxTrees, MetadataReference[] references, CompilationOptions? options = null)
    {
        AssemblyName = assemblyName ?? "assembly";
        _syntaxTrees = syntaxTrees;
        _references = references;
        Options = options ?? new CompilationOptions();
    }

    internal GlobalBinder GlobalBinder => _globalBinder ??= new GlobalBinder(this);

    public string AssemblyName { get; }

    public CompilationOptions Options { get; }

    public PerformanceInstrumentation PerformanceInstrumentation => Options.PerformanceInstrumentation;

    public ILoweringTraceSink? LoweringTrace => Options.LoweringTrace;

    public IOverloadResolutionLogger? OverloadResolutionLogger => Options.OverloadResolutionLogger;

    public IAssemblySymbol Assembly { get; private set; }

    public IModuleSymbol Module { get; private set; }

    public IEnumerable<MetadataReference> References => _references;

    public IEnumerable<IAssemblySymbol> ReferencedAssemblySymbols => Module.ReferencedAssemblySymbols;

    public SyntaxTree[] SyntaxTrees => _syntaxTrees;

    public INamespaceSymbol GlobalNamespace
    {
        get
        {
            if (!isSettingUp)
            {
                EnsureSetup();
            }

            return _globalNamespace ??=
                new MergedNamespaceSymbol(
                    new INamespaceSymbol[] { SourceGlobalNamespace }.Concat(_metadataReferenceSymbols.Select(x => x.Value.GlobalNamespace)),
                    null);
        }
    }

    internal SourceNamespaceSymbol SourceGlobalNamespace { get; private set; }

    public INamespaceSymbol GetSourceGlobalNamespace() => SourceGlobalNamespace!.AsSourceNamespace();

    public Assembly CoreAssembly { get; private set; }
    public Assembly RuntimeCoreAssembly { get; private set; }
    internal Assembly EmitCoreAssembly { get; private set; }

    internal BinderFactory BinderFactory { get; private set; }

    internal DeclarationTable DeclarationTable { get; private set; }

    internal SymbolFactory SymbolFactory { get; } = new SymbolFactory();

    internal BoundNodeFactory BoundNodeFactory => _boundNodeFactory ??= new BoundNodeFactory(this);

    public ISymbol ErrorSymbol => _errorSymbol ??= CreateErrorSymbol();

    public ITypeSymbol ErrorTypeSymbol => _errorTypeSymbol ??= CreateErrorTypeSymbol();

    private ErrorSymbol CreateErrorSymbol()
    {
        EnsureSetup();
        return new ErrorSymbol(this, "Error", GlobalNamespace, [], []);
    }

    private ErrorTypeSymbol CreateErrorTypeSymbol()
    {
        EnsureSetup();
        return new ErrorTypeSymbol(this, "Error", GlobalNamespace, [], []);
    }

    public ITypeSymbol NullTypeSymbol => _nullTypeSymbol ??= new NullTypeSymbol(this);
    public INamedTypeSymbol UnitTypeSymbol => _unitTypeSymbol ??= CreateUnitTypeSymbol();

    public static Compilation Create(string assemblyName, SyntaxTree[] syntaxTrees, CompilationOptions? options = null)
    {
        return new Compilation(assemblyName, syntaxTrees, [], options);
    }

    public static Compilation Create(string assemblyName, CompilationOptions? options = null)
    {
        return new Compilation(assemblyName, [], [], options);
    }

    public static Compilation Create(string assemblyName, SyntaxTree[] syntaxTrees, MetadataReference[] references, CompilationOptions? options = null)
    {
        if (references.Length == 0)
            references = [MetadataReference.CreateFromFile(typeof(object).Assembly.Location)];
        return new Compilation(assemblyName, syntaxTrees, references, options);
    }

    public Compilation AddSyntaxTrees(params SyntaxTree[] syntaxTrees)
    {
        return new Compilation(AssemblyName, _syntaxTrees.Concat(syntaxTrees).ToArray(), _references, Options);
    }

    public Compilation AddReferences(params MetadataReference[] references)
    {
        return new Compilation(AssemblyName, _syntaxTrees, references, Options);
    }

    public Compilation WithAssemblyName(string? assemblyName)
    {
        return new Compilation(assemblyName, _syntaxTrees, _references, Options);
    }

    public MetadataReference ToMetadataReference() => new CompilationReference(this);

    internal void EnsureSetup()
    {
        if (setup || isSettingUp)
            return;

        lock (_setupLock)
        {
            if (setup || isSettingUp)
                return;

            isSettingUp = true;

            try
            {
                Setup();
                setup = true;
            }
            finally
            {
                isSettingUp = false;
            }
        }
    }

    private void Setup()
    {
        List<string> paths = _references
            .OfType<PortableExecutableReference>()
            .Select(portableExecutableReference => portableExecutableReference.FilePath)
            .ToList();

        var runtimeCorePath = typeof(object).Assembly.Location;
        if (!string.IsNullOrEmpty(runtimeCorePath) && !paths.Contains(runtimeCorePath, StringComparer.OrdinalIgnoreCase))
            paths.Add(runtimeCorePath);

        // Seed the metadata resolver with framework/runtime assemblies so MetadataLoadContext
        // can resolve transitive framework dependencies (for example System.Reflection.MetadataLoadContext).
        EnsureTrustedPlatformAssembliesCached();
        foreach (var knownPath in _assemblyPathMap.Values)
        {
            if (!string.IsNullOrEmpty(knownPath) && File.Exists(knownPath) && !paths.Contains(knownPath, StringComparer.OrdinalIgnoreCase))
                paths.Add(knownPath);
        }

        foreach (var loadedAssembly in AppDomain.CurrentDomain.GetAssemblies())
        {
            string location;
            try
            {
                location = loadedAssembly.Location;
            }
            catch (NotSupportedException)
            {
                continue;
            }

            if (!string.IsNullOrEmpty(location) && File.Exists(location) && !paths.Contains(location, StringComparer.OrdinalIgnoreCase))
                paths.Add(location);
        }

        _metadataLoadContext = CreateMetadataLoadContext(paths);

        CoreAssembly = _metadataLoadContext.CoreAssembly!;
        RuntimeCoreAssembly = typeof(object).Assembly;
        EmitCoreAssembly = ResolveEmitCoreAssembly() ?? RuntimeCoreAssembly;
        RegisterRuntimeAssembly(CoreAssembly, RuntimeCoreAssembly.Location);

        foreach (var metadataReference in References)
        {
            GetAssemblyOrModuleSymbol(metadataReference);
        }

        BinderFactory = new BinderFactory(this);
        DeclarationTable = new DeclarationTable(SyntaxTrees);

        var assemblyDeclaringSyntaxReferences = SyntaxTrees
            .Select(static tree => tree.GetRoot().GetReference())
            .ToArray();

        Assembly = new SourceAssemblySymbol(this, AssemblyName, assemblyDeclaringSyntaxReferences);

        Module = new SourceModuleSymbol(AssemblyName, (SourceAssemblySymbol)Assembly, _metadataReferenceSymbols.Values, []);

        SourceGlobalNamespace = (SourceNamespaceSymbol)Module.GlobalNamespace;

        if (Options.OutputKind == OutputKind.ConsoleApplication)
            InitializeTopLevelPrograms();
    }

    private static MetadataLoadContext CreateMetadataLoadContext(IEnumerable<string> paths)
    {
        var pathByAssemblyIdentity = new Dictionary<string, string>(StringComparer.OrdinalIgnoreCase);

        foreach (var path in paths)
        {
            if (string.IsNullOrWhiteSpace(path))
                continue;

            string fullPath;
            try
            {
                fullPath = Path.GetFullPath(path);
            }
            catch
            {
                continue;
            }

            if (!File.Exists(fullPath))
                continue;

            System.Reflection.AssemblyName assemblyIdentity;
            try
            {
                assemblyIdentity = System.Reflection.AssemblyName.GetAssemblyName(fullPath);
            }
            catch
            {
                continue;
            }

            if (string.IsNullOrWhiteSpace(assemblyIdentity.FullName))
                continue;

            var identityKey = assemblyIdentity.FullName;

            if (!pathByAssemblyIdentity.TryGetValue(identityKey, out var existingPath))
            {
                pathByAssemblyIdentity[identityKey] = fullPath;
                continue;
            }

            // Keep the first path for an identity (typically reference assemblies from project metadata).
            // Preferring runtime assemblies here can hide reference-surface namespaces during binding.
        }

        var normalizedPaths = pathByAssemblyIdentity.Values
            .OrderBy(static path => path, StringComparer.OrdinalIgnoreCase)
            .ToArray();

        var resolver = new PathAssemblyResolver(normalizedPaths);
        return new MetadataLoadContext(resolver);
    }

    private static bool IsReferenceAssemblyPath(string path)
    {
        var normalized = path.Replace('\\', '/');
        return normalized.Contains("/packs/", StringComparison.OrdinalIgnoreCase)
            && normalized.Contains("/ref/", StringComparison.OrdinalIgnoreCase);
    }

    internal ReflectionTypeLoader ReflectionTypeLoader => _reflectionTypeLoader ??= new ReflectionTypeLoader(this);

    private Assembly? ResolveEmitCoreAssembly()
    {
        var loadedSystemRuntime = AppDomain.CurrentDomain
            .GetAssemblies()
            .FirstOrDefault(static assembly =>
                string.Equals(assembly.GetName().Name, "System.Runtime", StringComparison.OrdinalIgnoreCase));
        if (loadedSystemRuntime is not null)
            return loadedSystemRuntime;

        if (_assemblyPathMap.TryGetValue("System.Runtime", out var systemRuntimePath))
        {
            try
            {
                var identity = System.Reflection.AssemblyName.GetAssemblyName(systemRuntimePath);
                var loaded = LoadRuntimeAssemblyFromPath(identity, systemRuntimePath);
                if (loaded is not null)
                    return loaded;
            }
            catch
            {
            }
        }

        try
        {
            return System.Reflection.Assembly.Load("System.Runtime");
        }
        catch
        {
            return null;
        }
    }

    private void InitializeTopLevelPrograms()
    {
        foreach (var tree in SyntaxTrees)
        {
            if (tree is null)
                continue;

            if (tree.GetRoot() is not CompilationUnitSyntax compilationUnit)
                continue;

            var bindableGlobals = CollectBindableGlobalStatements(compilationUnit);
            var hasNonGlobalMembers = HasNonGlobalMembers(compilationUnit);

            if (bindableGlobals.Count == 0 && hasNonGlobalMembers)
                continue;

            if (bindableGlobals.Count > 0 && SyntaxTreeWithFileScopedCode is null)
                SyntaxTreeWithFileScopedCode = tree;

            var fileScopedNamespace = compilationUnit.Members
                .OfType<FileScopedNamespaceDeclarationSyntax>()
                .FirstOrDefault();

            SourceNamespaceSymbol targetNamespace = fileScopedNamespace is null
                ? SourceGlobalNamespace
                : GetOrCreateNamespaceSymbol(fileScopedNamespace.Name.ToString())?.AsSourceNamespace()
                    ?? SourceGlobalNamespace;

            GetOrCreateTopLevelProgram(compilationUnit, targetNamespace, bindableGlobals);
        }
    }

    internal (SynthesizedProgramClassSymbol Program, SynthesizedMainMethodSymbol Main, SynthesizedMainAsyncMethodSymbol? Async)
        GetOrCreateTopLevelProgram(
            CompilationUnitSyntax compilationUnit,
            SourceNamespaceSymbol targetNamespace,
            IReadOnlyList<GlobalStatementSyntax> bindableGlobals)
    {
        if (_topLevelProgramMembers.TryGetValue(compilationUnit.SyntaxTree, out var existing))
        {
            return (existing.ProgramClass, existing.MainMethod!, existing.AsyncMainMethod);
        }

        var returnsInt = bindableGlobals.Any(static g => ContainsNonUnitReturnOutsideNestedFunctions(g.Statement));
        var requiresAsync = bindableGlobals.Any(static g => ContainsAwaitExpressionOutsideNestedFunctions(g.Statement));
        var hasTopLevelMainFunction = bindableGlobals.Any(static g => g.Statement is FunctionStatementSyntax { Identifier.ValueText: "Main" });
        var containsExecutableCode = !hasTopLevelMainFunction && (bindableGlobals.Count == 0
            || bindableGlobals.Any(static g => g.Statement is not FunctionStatementSyntax));

        var programClass = new SynthesizedProgramClassSymbol(this, targetNamespace, [compilationUnit.GetLocation()], [compilationUnit.GetReference()]);

        SynthesizedMainAsyncMethodSymbol? asyncImplementation = null;
        if (requiresAsync)
        {
            asyncImplementation = new SynthesizedMainAsyncMethodSymbol(
                programClass,
                [compilationUnit.GetLocation()],
                [compilationUnit.GetReference()],
                returnsInt);
        }

        var mainMethod = new SynthesizedMainMethodSymbol(
            programClass,
            [compilationUnit.GetLocation()],
            [compilationUnit.GetReference()],
            containsExecutableCode,
            returnsInt,
            asyncImplementation);

        var members = new TopLevelProgramMembers(programClass);
        members.MainMethod = mainMethod;
        members.AsyncMainMethod = asyncImplementation;
        _topLevelProgramMembers[compilationUnit.SyntaxTree] = members;

        return (programClass, mainMethod, asyncImplementation);
    }

    internal static List<GlobalStatementSyntax> CollectBindableGlobalStatements(CompilationUnitSyntax compilationUnit)
    {
        var bindableGlobals = new List<GlobalStatementSyntax>();

        foreach (var global in compilationUnit.DescendantNodes().OfType<GlobalStatementSyntax>())
        {
            if (global.Parent is CompilationUnitSyntax or FileScopedNamespaceDeclarationSyntax)
                bindableGlobals.Add(global);
        }

        return bindableGlobals;
    }

    internal static bool HasNonGlobalMembers(CompilationUnitSyntax compilationUnit)
    {
        foreach (var member in compilationUnit.Members)
        {
            switch (member)
            {
                case GlobalStatementSyntax:
                    continue;
                case FileScopedNamespaceDeclarationSyntax fileScoped when ContainsOnlyGlobalStatements(fileScoped):
                    continue;
                default:
                    return true;
            }
        }

        return false;

        static bool ContainsOnlyGlobalStatements(FileScopedNamespaceDeclarationSyntax fileScoped)
        {
            foreach (var nested in fileScoped.Members)
            {
                if (nested is not GlobalStatementSyntax)
                    return false;
            }

            return true;
        }
    }

    internal static bool ContainsAwaitExpressionOutsideNestedFunctions(StatementSyntax statement)
    {
        return ContainsAwaitExpressionOutsideNestedFunctions((SyntaxNode)statement);
    }

    internal static bool ContainsAwaitExpressionOutsideNestedFunctions(SyntaxNode node)
    {
        return ContainsAwaitExpressionOutsideNestedFunctionsCore(node);

        static bool ContainsAwaitExpressionOutsideNestedFunctionsCore(SyntaxNode current)
        {
            if (current is FunctionStatementSyntax or LambdaExpressionSyntax)
                return false;

            if (current.Kind == SyntaxKind.AwaitExpression)
                return true;

            foreach (var child in current.ChildNodes())
            {
                if (child is FunctionStatementSyntax or LambdaExpressionSyntax)
                    continue;

                if (ContainsAwaitExpressionOutsideNestedFunctionsCore(child))
                    return true;
            }

            return false;
        }
    }

    internal static bool ContainsNonUnitReturnOutsideNestedFunctions(StatementSyntax statement)
    {
        return ContainsNonUnitReturnOutsideNestedFunctions((SyntaxNode)statement);

        static bool ContainsNonUnitReturnOutsideNestedFunctions(SyntaxNode node)
        {
            if (node is FunctionStatementSyntax or LambdaExpressionSyntax)
                return false;

            if (node is ReturnStatementSyntax returnStatement)
            {
                if (returnStatement.Expression is null)
                    return false;

                // Explicit `return ()` keeps synthesized main as unit-returning.
                if (returnStatement.Expression is UnitExpressionSyntax)
                    return false;

                return true;
            }

            foreach (var child in node.ChildNodes())
            {
                if (child is FunctionStatementSyntax or LambdaExpressionSyntax)
                    continue;

                if (ContainsNonUnitReturnOutsideNestedFunctions(child))
                    return true;
            }

            return false;
        }
    }

    private sealed class TopLevelProgramMembers
    {
        public TopLevelProgramMembers(SynthesizedProgramClassSymbol programClass)
        {
            ProgramClass = programClass;
        }

        public SynthesizedProgramClassSymbol ProgramClass { get; }
        public SynthesizedMainMethodSymbol? MainMethod { get; set; }
        public SynthesizedMainAsyncMethodSymbol? AsyncMainMethod { get; set; }
    }

    private UnitTypeSymbol CreateUnitTypeSymbol()
    {
        var global = SourceGlobalNamespace;
        var system = global.LookupNamespace("System") as SourceNamespaceSymbol;

        if (system is null)
        {
            system = new SourceNamespaceSymbol((SourceModuleSymbol)Module, "System", Assembly, null, global, [], []);
            global.AddMember(system);
        }

        var unit = new UnitTypeSymbol(this, system);
        system.AddMember(unit);
        return unit;
    }

    internal INamespaceSymbol? GetOrCreateNamespaceSymbol(string? ns)
    {
        // Caller asked for the root. Return the merged root.
        if (ns is null)
            return GlobalNamespace;

        var namespaceParts = ns.Split('.', StringSplitOptions.RemoveEmptyEntries);

        // Empty string / only dots => treat as root (merged).
        if (namespaceParts.Length == 0)
            return GlobalNamespace;

        // 1) Ensure the SOURCE namespace chain exists.
        var currentSourceNamespace = SourceGlobalNamespace;

        foreach (var part in namespaceParts)
        {
            var nextSource = currentSourceNamespace
                .GetMembers(part)
                .OfType<SourceNamespaceSymbol>()
                .FirstOrDefault();

            if (nextSource is null)
            {
                nextSource = new SourceNamespaceSymbol(
                    part,
                    currentSourceNamespace,
                    currentSourceNamespace,
                    [],
                    []);

                currentSourceNamespace.AddMember(nextSource);
            }

            currentSourceNamespace = nextSource;
        }

        // 2) Now return the corresponding namespace symbol from the MERGED global root.
        // This relies on GlobalNamespace being the merged view that includes source namespaces.
        INamespaceSymbol currentMerged = GlobalNamespace;

        foreach (var part in namespaceParts)
        {
            // Prefer a dedicated namespace lookup API if you have it:
            var nextMerged =
                currentMerged.LookupNamespace(part) // if available on your namespace symbol
                ?? currentMerged.GetMembers(part).OfType<INamespaceSymbol>().FirstOrDefault();

            if (nextMerged is null)
            {
                // If this happens, it usually means the merged namespace view did not
                // observe the newly-added source namespace (lazy merge / caching issue).
                return null;
            }

            currentMerged = nextMerged;
        }

        return currentMerged;
    }

    /*
        internal INamespaceSymbol? GetOrCreateNamespaceSymbol(string? ns)
        {
            if (ns is null)
                return GlobalNamespace;

            var namespaceParts = ns.Split('.', StringSplitOptions.RemoveEmptyEntries);
            if (namespaceParts.Length == 0)
                return SourceGlobalNamespace;

            var currentSourceNamespace = SourceGlobalNamespace;

            foreach (var part in namespaceParts)
            {
                var next = currentSourceNamespace
                    .GetMembers(part)
                    .OfType<SourceNamespaceSymbol>()
                    .FirstOrDefault();

                if (next is null)
                {
                    next = new SourceNamespaceSymbol(
                        part,
                        currentSourceNamespace,
                        currentSourceNamespace,
                        [],
                        []);

                    currentSourceNamespace.AddMember(next);
                }

                currentSourceNamespace = next;
            }

            return currentSourceNamespace;
        }*/

    private void AnalyzeMemberDeclaration(SyntaxTree syntaxTree, ISymbol declaringSymbol, MemberDeclarationSyntax memberDeclaration)
    {
        if (memberDeclaration is BaseNamespaceDeclarationSyntax namespaceDeclarationSyntax)
        {
            Location[] locations = [syntaxTree.GetLocation(namespaceDeclarationSyntax.EffectiveSpan)];

            SyntaxReference[] references = [namespaceDeclarationSyntax.GetReference()];

            var symbol = new SourceNamespaceSymbol(
                namespaceDeclarationSyntax.Name.ToString(), declaringSymbol, (INamespaceSymbol?)declaringSymbol,
                locations, references);

            foreach (var memberDeclaration2 in namespaceDeclarationSyntax.Members)
            {
                AnalyzeMemberDeclaration(syntaxTree, symbol, memberDeclaration2);
            }
        }
        else if (memberDeclaration is TypeDeclarationSyntax classDeclaration &&
                 classDeclaration is ClassDeclarationSyntax or StructDeclarationSyntax or RecordDeclarationSyntax)
        {
            Location[] locations = [syntaxTree.GetLocation(classDeclaration.EffectiveSpan)];

            SyntaxReference[] references = [classDeclaration.GetReference()];

            var containingType = declaringSymbol as INamedTypeSymbol;
            var containingNamespace = declaringSymbol switch
            {
                INamespaceSymbol ns => ns,
                INamedTypeSymbol type => type.ContainingNamespace,
                _ => null
            };

            var declaredTypeKind = classDeclaration.Keyword.Kind == SyntaxKind.StructKeyword
                ? TypeKind.Struct
                : TypeKind.Class;

            INamedTypeSymbol baseTypeSymbol = declaredTypeKind == TypeKind.Struct
                ? GetSpecialType(SpecialType.System_ValueType)
                : GetSpecialType(SpecialType.System_Object);
            ImmutableArray<INamedTypeSymbol> interfaceList = ImmutableArray<INamedTypeSymbol>.Empty;
            var baseList = classDeclaration switch
            {
                ClassDeclarationSyntax concreteClass => concreteClass.BaseList,
                RecordDeclarationSyntax concreteRecord => concreteRecord.BaseList,
                StructDeclarationSyntax concreteStruct => concreteStruct.BaseList,
                _ => null
            };
            var typeParameterList = classDeclaration switch
            {
                ClassDeclarationSyntax concreteClass => concreteClass.TypeParameterList,
                RecordDeclarationSyntax concreteRecord => concreteRecord.TypeParameterList,
                StructDeclarationSyntax concreteStruct => concreteStruct.TypeParameterList,
                _ => null
            };
            var constraintClauses = classDeclaration switch
            {
                ClassDeclarationSyntax concreteClass => concreteClass.ConstraintClauses,
                RecordDeclarationSyntax concreteRecord => concreteRecord.ConstraintClauses,
                StructDeclarationSyntax concreteStruct => concreteStruct.ConstraintClauses,
                _ => SyntaxList<TypeParameterConstraintClauseSyntax>.Empty
            };

            if (baseList is not null)
            {
                var builder = ImmutableArray.CreateBuilder<INamedTypeSymbol>();
                foreach (var t in baseList.Types)
                {
                    if (ResolveSimpleType(t, declaringSymbol) is INamedTypeSymbol resolved)
                    {
                        if (resolved.TypeKind == TypeKind.Interface)
                            builder.Add(resolved);
                        else
                            baseTypeSymbol = resolved;
                    }
                }

                if (builder.Count > 0)
                    interfaceList = builder.ToImmutable();
            }

            var isStatic = classDeclaration.Modifiers.Any(m => m.Kind == SyntaxKind.StaticKeyword);
            var isAbstract = isStatic || classDeclaration.Modifiers.Any(m => m.Kind == SyntaxKind.AbstractKeyword);
            var isSealed = isStatic || (!classDeclaration.Modifiers.Any(m => m.Kind == SyntaxKind.OpenKeyword) && !isAbstract);
            var typeAccessibility = AccessibilityUtilities.DetermineAccessibility(
                classDeclaration.Modifiers,
                AccessibilityUtilities.GetDefaultTypeAccessibility(declaringSymbol));

            var symbol = new SourceNamedTypeSymbol(
                classDeclaration.Identifier.ValueText,
                baseTypeSymbol,
                declaredTypeKind,
                declaringSymbol,
                containingType,
                containingNamespace,
                locations,
                references,
                isSealed,
                isAbstract,
                isStatic,
                declaredAccessibility: typeAccessibility);

            InitializeTypeParameters(symbol, typeParameterList, constraintClauses, syntaxTree);

            if (!interfaceList.IsDefaultOrEmpty)
                symbol.SetInterfaces(interfaceList);

            foreach (var memberDeclaration2 in classDeclaration.Members)
            {
                AnalyzeMemberDeclaration(syntaxTree, symbol, memberDeclaration2);
            }

            static INamedTypeSymbol? ResolveSimpleType(TypeSyntax typeSyntax, ISymbol container)
            {
                if (typeSyntax is IdentifierNameSyntax id)
                {
                    return (container switch
                    {
                        INamespaceSymbol ns => ns.LookupType(id.Identifier.ValueText),
                        INamedTypeSymbol nt => nt.ContainingNamespace.LookupType(id.Identifier.ValueText),
                        _ => null
                    }) as INamedTypeSymbol;
                }

                return null;
            }
        }
        else if (memberDeclaration is InterfaceDeclarationSyntax interfaceDeclaration)
        {
            Location[] locations = [syntaxTree.GetLocation(interfaceDeclaration.EffectiveSpan)];

            SyntaxReference[] references = [interfaceDeclaration.GetReference()];

            var containingType = declaringSymbol as INamedTypeSymbol;
            var containingNamespace = declaringSymbol switch
            {
                INamespaceSymbol ns => ns,
                INamedTypeSymbol type => type.ContainingNamespace,
                _ => null
            };

            var symbol = new SourceNamedTypeSymbol(
                interfaceDeclaration.Identifier.ValueText,
                GetSpecialType(SpecialType.System_Object),
                TypeKind.Interface,
                declaringSymbol,
                containingType,
                containingNamespace,
                locations,
                references,
                true,
                isAbstract: true,
                declaredAccessibility: AccessibilityUtilities.DetermineAccessibility(
                    interfaceDeclaration.Modifiers,
                    AccessibilityUtilities.GetDefaultTypeAccessibility(declaringSymbol)));

            InitializeTypeParameters(symbol, interfaceDeclaration.TypeParameterList, interfaceDeclaration.ConstraintClauses, syntaxTree);

            foreach (var memberDeclaration2 in interfaceDeclaration.Members)
            {
                AnalyzeMemberDeclaration(syntaxTree, symbol, memberDeclaration2);
            }
        }
        else if (memberDeclaration is ExtensionDeclarationSyntax extensionDeclaration)
        {
            Location[] locations = [syntaxTree.GetLocation(extensionDeclaration.EffectiveSpan)];

            SyntaxReference[] references = [extensionDeclaration.GetReference()];

            var containingType = declaringSymbol as INamedTypeSymbol;
            var containingNamespace = declaringSymbol switch
            {
                INamespaceSymbol ns => ns,
                INamedTypeSymbol type => type.ContainingNamespace,
                _ => null
            };

            var baseTypeSymbol = GetSpecialType(SpecialType.System_Object);

            var extensionAccessibility = AccessibilityUtilities.DetermineAccessibility(
                extensionDeclaration.Modifiers,
                AccessibilityUtilities.GetDefaultTypeAccessibility(declaringSymbol));

            var symbol = new SourceNamedTypeSymbol(
                extensionDeclaration.Identifier.ValueText,
                baseTypeSymbol!,
                TypeKind.Class,
                declaringSymbol,
                containingType,
                containingNamespace,
                locations,
                references,
                isSealed: true,
                isAbstract: true,
                declaredAccessibility: extensionAccessibility);

            symbol.MarkAsExtensionContainer();

            InitializeTypeParameters(symbol, extensionDeclaration.TypeParameterList, extensionDeclaration.ConstraintClauses, syntaxTree);

            foreach (var memberDeclaration2 in extensionDeclaration.Members)
            {
                AnalyzeMemberDeclaration(syntaxTree, symbol, memberDeclaration2);
            }
        }
        else if (memberDeclaration is DelegateDeclarationSyntax delegateDeclaration)
        {
            Location[] locations = [syntaxTree.GetLocation(delegateDeclaration.EffectiveSpan)];
            SyntaxReference[] references = [delegateDeclaration.GetReference()];

            var containingType = declaringSymbol as INamedTypeSymbol;
            var containingNamespace = declaringSymbol switch
            {
                INamespaceSymbol ns => ns,
                INamedTypeSymbol type => type.ContainingNamespace,
                _ => null
            };

            var baseTypeSymbol = GetSpecialType(SpecialType.System_MulticastDelegate);

            var typeAccessibility = AccessibilityUtilities.DetermineAccessibility(
                delegateDeclaration.Modifiers,
                AccessibilityUtilities.GetDefaultTypeAccessibility(declaringSymbol));

            var symbol = new SourceNamedTypeSymbol(
                delegateDeclaration.Identifier.ValueText,
                baseTypeSymbol,
                TypeKind.Delegate,
                declaringSymbol,
                containingType,
                containingNamespace,
                locations,
                references,
                isSealed: true,
                isAbstract: true,
                isStatic: false,
                declaredAccessibility: typeAccessibility);

            InitializeTypeParameters(symbol, delegateDeclaration.TypeParameterList, delegateDeclaration.ConstraintClauses, syntaxTree);

            // .ctor(object, IntPtr)
            var ctorParameters = ImmutableArray.CreateBuilder<SourceParameterSymbol>(2);
            ctorParameters.Add(new SourceParameterSymbol(
                "object",
                GetSpecialType(SpecialType.System_Object),
                symbol,
                symbol,
                symbol.ContainingNamespace,
                locations,
                references,
                RefKind.None));
            ctorParameters.Add(new SourceParameterSymbol(
                "method",
                GetSpecialType(SpecialType.System_IntPtr),
                symbol,
                symbol,
                symbol.ContainingNamespace,
                locations,
                references,
                RefKind.None));

            _ = new SourceMethodSymbol(
                ".ctor",
                GetSpecialType(SpecialType.System_Unit),
                ctorParameters.MoveToImmutable(),
                symbol,
                symbol,
                symbol.ContainingNamespace,
                locations,
                references,
                isStatic: false,
                methodKind: MethodKind.Constructor,
                declaredAccessibility: Accessibility.Public);

            // Invoke
            var returnType = delegateDeclaration.ReturnType is null
                ? GetSpecialType(SpecialType.System_Unit)
                : ResolveTypeSyntax(delegateDeclaration.ReturnType.Type, symbol);

            var invokeParameters = ImmutableArray.CreateBuilder<SourceParameterSymbol>(delegateDeclaration.ParameterList.Parameters.Count);
            foreach (var p in delegateDeclaration.ParameterList.Parameters)
            {
                var refKindTokenKind = p.RefKindKeyword?.Kind;
                var typeSyntax = p.TypeAnnotation!.Type;
                var refKind = typeSyntax is ByRefTypeSyntax
                    ? refKindTokenKind switch
                    {
                        SyntaxKind.OutKeyword => RefKind.Out,
                        SyntaxKind.InKeyword => RefKind.In,
                        SyntaxKind.RefKeyword => RefKind.Ref,
                        _ => RefKind.Ref,
                    }
                    : refKindTokenKind switch
                    {
                        SyntaxKind.OutKeyword => RefKind.Out,
                        SyntaxKind.InKeyword => RefKind.In,
                        SyntaxKind.RefKeyword => RefKind.Ref,
                        _ => RefKind.None,
                    };

                var boundTypeSyntax = refKind.IsByRef() && typeSyntax is ByRefTypeSyntax byRefType
                    ? byRefType.ElementType
                    : typeSyntax;
                var pType = ResolveTypeSyntax(boundTypeSyntax, symbol);

                invokeParameters.Add(new SourceParameterSymbol(
                    p.Identifier.ValueText,
                    pType,
                    symbol,
                    symbol,
                    symbol.ContainingNamespace,
                    locations,
                    references,
                    refKind));
            }

            _ = new SourceMethodSymbol(
                "Invoke",
                returnType,
                invokeParameters.MoveToImmutable(),
                symbol,
                symbol,
                symbol.ContainingNamespace,
                locations,
                references,
                isStatic: false,
                declaredAccessibility: Accessibility.Public);
        }
        else if (memberDeclaration is MethodDeclarationSyntax methodDeclaration)
        {
            Location[] locations = [syntaxTree.GetLocation(methodDeclaration.EffectiveSpan)];

            SyntaxReference[] references = [methodDeclaration.GetReference()];

            var containingType = declaringSymbol as INamedTypeSymbol;
            var containingNamespace = declaringSymbol switch
            {
                INamespaceSymbol ns => ns,
                INamedTypeSymbol type => type.ContainingNamespace,
                _ => null
            };

            var returnType = GetSpecialType(SpecialType.System_Unit);
            var isStatic = methodDeclaration.Modifiers.Any(m => m.Kind == SyntaxKind.StaticKeyword);
            var declaredInExtension = declaringSymbol is SourceNamedTypeSymbol { IsExtensionDeclaration: true };
            if (declaredInExtension)
                isStatic = true;
            var defaultAccessibility = containingType is not null
                ? AccessibilityUtilities.GetDefaultMemberAccessibility(containingType)
                : AccessibilityUtilities.GetDefaultTypeAccessibility(declaringSymbol);
            var methodAccessibility = AccessibilityUtilities.DetermineAccessibility(
                methodDeclaration.Modifiers,
                defaultAccessibility);

            var methodSymbol = new SourceMethodSymbol(
                methodDeclaration.Identifier.ValueText, returnType,
                ImmutableArray<SourceParameterSymbol>.Empty,
                declaringSymbol,
                containingType,
                containingNamespace,
                locations, references,
                isStatic: isStatic,
                declaredAccessibility: methodAccessibility);

            if (declaredInExtension)
                methodSymbol.MarkDeclaredInExtension();
        }
    }

    public ITypeSymbol CreateArrayTypeSymbol(ITypeSymbol elementType, int rank = 1)
    {
        var ns = GlobalNamespace.LookupNamespace("System");
        return new ArrayTypeSymbol(GetSpecialType(SpecialType.System_Array), elementType, ns, null, ns, [], rank);
    }

    public ITypeSymbol CreatePointerTypeSymbol(ITypeSymbol pointedAtType)
    {
        return new PointerTypeSymbol(pointedAtType);
    }
    public ITypeSymbol CreateFunctionTypeSymbol(ITypeSymbol[] parameterTypes, ITypeSymbol returnType)
    {
        var systemNamespace = GlobalNamespace.LookupNamespace("System");

        var allTypes = parameterTypes.ToList();
        bool isAction = returnType.SpecialType == SpecialType.System_Void || returnType.SpecialType == SpecialType.System_Unit;

        if (!isAction)
            allTypes.Add(returnType);

        string delegateName = isAction ? "Action" : "Func";
        INamedTypeSymbol? delegateType = systemNamespace?.GetMembers(delegateName)
            .OfType<INamedTypeSymbol>()
            .FirstOrDefault(t => t.Arity == allTypes.Count);

        if (delegateType is null)
        {
            var metadataName = $"System.{delegateName}`{allTypes.Count}";
            delegateType = GetTypeByMetadataName(metadataName);
        }

        if (delegateType is not null)
            return delegateType.Construct(allTypes.ToArray());

        var parameterImmutable = parameterTypes.ToImmutableArray();
        var refKinds = ImmutableArray.CreateRange(Enumerable.Repeat(RefKind.None, parameterTypes.Length));
        return GetOrAddSynthesizedDelegate(parameterImmutable, refKinds, returnType);
    }

    public ITypeSymbol CreateTupleTypeSymbol(IEnumerable<(string? name, ITypeSymbol type)> elements)
    {
        var systemNamespace = GlobalNamespace.LookupNamespace("System");
        var elementArray = elements.ToArray();

        var tupleDefinition = systemNamespace?.GetMembers("ValueTuple")
            .OfType<INamedTypeSymbol>()
            .FirstOrDefault(t => t.Arity == elementArray.Length);

        if (tupleDefinition is null)
            return ErrorTypeSymbol;

        var underlying = (INamedTypeSymbol)tupleDefinition.Construct(elementArray.Select(e => e.type).ToArray());
        var tuple = new TupleTypeSymbol(underlying, null, null, null, []);

        var fields = new List<IFieldSymbol>();
        int i = 0;
        foreach (var tupleField in underlying.GetMembers().OfType<SubstitutedFieldSymbol>())
        {
            var name = elementArray[i].name ?? $"Item{i + 1}";
            fields.Add(new TupleFieldSymbol(name, tupleField, underlying, []));
            i++;
        }

        tuple.SetTupleElements(fields);

        return tuple;
    }

    public ITypeSymbol ConstructGenericType(INamedTypeSymbol genericDefinition, ITypeSymbol[] typeArgs)
    {
        return genericDefinition.Construct(typeArgs);
    }

    private ITypeSymbol ResolveTypeSyntax(TypeSyntax typeSyntax, ISymbol container)
    {
        switch (typeSyntax)
        {
            case ByRefTypeSyntax refType:
                {
                    var elementType = ResolveTypeSyntax(refType.ElementType, container);
                    return new RefTypeSymbol(elementType);
                }

            case PredefinedTypeSyntax predefined:
                return ResolvePredefinedType(predefined);

            case IdentifierNameSyntax id:
                if (container is INamedTypeSymbol namedType)
                {
                    var typeParameter = namedType.TypeParameters.FirstOrDefault(tp => tp.Name == id.Identifier.ValueText);
                    if (typeParameter is not null)
                        return typeParameter;
                }

                return (container switch
                {
                    INamespaceSymbol ns => ns.LookupType(id.Identifier.ValueText),
                    INamedTypeSymbol nt => nt.ContainingNamespace.LookupType(id.Identifier.ValueText),
                    _ => null
                }) as ITypeSymbol ?? ErrorTypeSymbol;

            case QualifiedNameSyntax qn:
                {
                    // Resolve left as namespace, then right as type
                    var leftName = qn.Left.ToString();
                    string rightName = string.Empty;
                    if (qn.Right is IdentifierNameSyntax ifname)
                    {
                        rightName = ifname.Identifier.ValueText;
                    }

                    var ns = (container switch
                    {
                        INamespaceSymbol nns => nns,
                        INamedTypeSymbol nt => nt.ContainingNamespace,
                        _ => null
                    });

                    var resolvedNs = ns?.LookupNamespace(leftName);
                    if (resolvedNs is not null)
                        return resolvedNs.LookupType(rightName) as ITypeSymbol ?? ErrorTypeSymbol;

                    // Fallback: treat as metadata name
                    var metadata = qn.ToString();
                    return GetTypeByMetadataName(metadata) ?? ErrorTypeSymbol;
                }

            case GenericNameSyntax gn:
                {
                    var def = (container switch
                    {
                        INamespaceSymbol ns => ns.LookupType(gn.Identifier.ValueText),
                        INamedTypeSymbol nt => nt.ContainingNamespace.LookupType(gn.Identifier.ValueText),
                        _ => null
                    }) as INamedTypeSymbol;

                    if (def is null)
                        return ErrorTypeSymbol;

                    var args = gn.TypeArgumentList.Arguments
                        .Select(a => ResolveTypeSyntax(a.Type, container))
                        .ToArray();

                    return def.Construct(args);
                }

            default:
                return ErrorTypeSymbol;
        }
    }

    public ITypeSymbol ResolvePredefinedType(PredefinedTypeSyntax predefinedType)
    {
        var keywordKind = predefinedType.Keyword.Kind;

        var specialType = keywordKind switch
        {
            SyntaxKind.BoolKeyword => SpecialType.System_Boolean,
            SyntaxKind.CharKeyword => SpecialType.System_Char,
            SyntaxKind.SByteKeyword => SpecialType.System_SByte,
            SyntaxKind.ShortKeyword => SpecialType.System_Int16,
            SyntaxKind.UShortKeyword => SpecialType.System_UInt16,
            SyntaxKind.DoubleKeyword => SpecialType.System_Double,
            SyntaxKind.DecimalKeyword => SpecialType.System_Decimal,
            SyntaxKind.FloatKeyword => SpecialType.System_Single,
            SyntaxKind.IntKeyword => SpecialType.System_Int32,
            SyntaxKind.UIntKeyword => SpecialType.System_UInt32,
            SyntaxKind.LongKeyword => SpecialType.System_Int64,
            SyntaxKind.ULongKeyword => SpecialType.System_UInt64,
            SyntaxKind.NIntKeyword => SpecialType.System_IntPtr,
            SyntaxKind.NUIntKeyword => SpecialType.System_UIntPtr,
            SyntaxKind.ByteKeyword => SpecialType.System_Byte,
            SyntaxKind.ObjectKeyword => SpecialType.System_Object,
            SyntaxKind.StringKeyword => SpecialType.System_String,
            SyntaxKind.UnitKeyword => SpecialType.System_Unit,
            _ => throw new Exception($"Unexpected predefined keyword: {keywordKind}")
        };

        return GetSpecialType(specialType)
               ?? throw new Exception($"Special type not found for: {specialType}");
    }

    public ITypeSymbol? GetType(Type type)
    {
        return ReflectionTypeLoader.ResolveType(type);
    }

    public ISymbol? GetAssemblyOrModuleSymbol(MetadataReference metadataReference)
    {
        if (!_metadataReferenceSymbols.TryGetValue(metadataReference, out var symbol))
        {
            switch (metadataReference)
            {
                case PortableExecutableReference per:
                    {
                        var assembly = LoadMetadataAssembly(per.FilePath);
                        RegisterRuntimeAssembly(assembly, per.FilePath);
                        symbol = GetAssembly(assembly);
                        break;
                    }
                case CompilationReference cr:
                    {
                        var compilation = cr.Compilation;
                        compilation.EnsureSetup();
                        symbol = compilation.Assembly;
                        break;
                    }
                default:
                    throw new InvalidOperationException();
            }

            _metadataReferenceSymbols[metadataReference] = (IAssemblySymbol)symbol!;
        }
        return symbol;
    }

    private Assembly LoadMetadataAssembly(string assemblyPath)
    {
        var fullPath = Path.GetFullPath(assemblyPath);
        if (_lazyMetadataAssemblies.TryGetValue(fullPath, out var cachedByPath))
            return cachedByPath;

        System.Reflection.AssemblyName? identity = null;
        try
        {
            identity = System.Reflection.AssemblyName.GetAssemblyName(fullPath);
        }
        catch
        {
            // Fall through and attempt to load by path directly.
        }

        Assembly assembly;
        if (identity is not null)
        {
            assembly = _metadataLoadContext.LoadFromAssemblyName(identity);
        }
        else
        {
            assembly = _metadataLoadContext.LoadFromAssemblyPath(fullPath);
        }

        _lazyMetadataAssemblies[fullPath] = assembly;

        return assembly;
    }

    private IAssemblySymbol GetAssembly(Assembly assembly)
    {
        RegisterRuntimeAssembly(assembly);

        if (_assemblySymbols.TryGetValue(assembly, out var asss))
        {
            return asss;
        }

        PEAssemblySymbol assemblySymbol = new PEAssemblySymbol(assembly, []);

        var refs = assembly.GetReferencedAssemblies();

        assemblySymbol.AddModules(
            new PEModuleSymbol(
                ReflectionTypeLoader,
                assemblySymbol,
                assembly.ManifestModule,
                [],
                refs.Select(x =>
                {
                    try
                    {
                        var loadedAssembly = _metadataLoadContext.LoadFromAssemblyName(x);
                        if (loadedAssembly is null)
                            return null;

                        RegisterRuntimeAssembly(loadedAssembly);
                        return GetAssembly(loadedAssembly);
                    }
                    catch
                    {
                        return null;
                    }
                }).Where(x => x is not null).ToArray()));

        _assemblySymbols[assembly] = assemblySymbol;

        return assemblySymbol;
    }

    private Assembly? RegisterRuntimeAssembly(Assembly metadataAssembly, string? explicitPath = null)
    {
        if (metadataAssembly is null)
            return null;

        EnsureTrustedPlatformAssembliesCached();

        var identity = metadataAssembly.GetName();
        if (!string.IsNullOrEmpty(explicitPath) && identity.Name is not null)
            _assemblyPathMap[identity.Name] = explicitPath;
        else if (identity.Name is { } identityName)
        {
            try
            {
                var metadataLocation = metadataAssembly.Location;
                if (!string.IsNullOrEmpty(metadataLocation) && !_assemblyPathMap.ContainsKey(identityName))
                {
                    _assemblyPathMap[identityName] = metadataLocation;
                }
            }
            catch (NotSupportedException)
            {
                // Dynamic assemblies can throw when querying Location.
            }
        }

        if (_metadataToRuntimeAssemblyMap.TryGetValue(metadataAssembly, out var cached))
            return cached;

        Assembly? runtimeAssembly = null;
        string? resolvedPath = null;

        if (identity.Name is not null)
        {
            var alreadyLoaded = AppDomain.CurrentDomain
                .GetAssemblies()
                .FirstOrDefault(a => string.Equals(a.GetName().Name, identity.Name, StringComparison.OrdinalIgnoreCase));

            if (alreadyLoaded is not null)
            {
                runtimeAssembly = alreadyLoaded;
                if (!string.IsNullOrEmpty(alreadyLoaded.Location))
                    resolvedPath = alreadyLoaded.Location;
            }
        }

        if (identity.Name is not null && _runtimeAssemblyCache.TryGetValue(identity.Name, out var fromCache))
        {
            runtimeAssembly = fromCache;
            if (!string.IsNullOrEmpty(runtimeAssembly.Location))
                resolvedPath = runtimeAssembly.Location;
        }
        else if (identity.Name is not null && _assemblyPathMap.TryGetValue(identity.Name, out var knownPath))
        {
            runtimeAssembly = LoadRuntimeAssemblyFromPath(identity, knownPath);
            if (runtimeAssembly is not null)
            {
                resolvedPath = !string.IsNullOrEmpty(runtimeAssembly.Location)
                    ? runtimeAssembly.Location
                    : knownPath;
            }
            else if (TryMapReferenceAssemblyToRuntimePath(knownPath) is { } mappedKnownPath)
            {
                runtimeAssembly = LoadRuntimeAssemblyFromPath(identity, mappedKnownPath);
                if (runtimeAssembly is not null)
                    resolvedPath = mappedKnownPath;
            }
        }

        if (runtimeAssembly is null && !string.IsNullOrEmpty(explicitPath))
        {
            runtimeAssembly = LoadRuntimeAssemblyFromPath(identity, explicitPath);
            if (runtimeAssembly is not null)
            {
                resolvedPath = !string.IsNullOrEmpty(runtimeAssembly.Location)
                    ? runtimeAssembly.Location
                    : explicitPath;
            }
            else if (TryMapReferenceAssemblyToRuntimePath(explicitPath) is { } mappedExplicitPath)
            {
                runtimeAssembly = LoadRuntimeAssemblyFromPath(identity, mappedExplicitPath);
                if (runtimeAssembly is not null)
                    resolvedPath = mappedExplicitPath;
            }
        }

        if (runtimeAssembly is null)
        {
            runtimeAssembly = LoadRuntimeAssemblyByName(identity);
            if (runtimeAssembly is not null && !string.IsNullOrEmpty(runtimeAssembly.Location))
                resolvedPath = runtimeAssembly.Location;
        }

        if (runtimeAssembly is null)
        {
            runtimeAssembly = MapToRuntimeImplementation(identity);
            if (runtimeAssembly is not null && !string.IsNullOrEmpty(runtimeAssembly.Location))
                resolvedPath = runtimeAssembly.Location;
        }

        if (runtimeAssembly is not null)
        {
            if (identity.Name is not null)
            {
                _runtimeAssemblyCache[identity.Name] = runtimeAssembly;

                if (!string.IsNullOrEmpty(resolvedPath))
                {
                    _assemblyPathMap[identity.Name] = resolvedPath;
                }
                else if (!string.IsNullOrEmpty(runtimeAssembly.Location))
                {
                    _assemblyPathMap[identity.Name] = runtimeAssembly.Location;
                }
            }

            _metadataToRuntimeAssemblyMap[metadataAssembly] = runtimeAssembly;
        }

        return runtimeAssembly;
    }

    private void EnsureTrustedPlatformAssembliesCached()
    {
        if (_trustedPlatformAssembliesCached)
            return;

        _trustedPlatformAssembliesCached = true;

        if (AppContext.GetData("TRUSTED_PLATFORM_ASSEMBLIES") is not string platformAssemblies)
            return;

        var candidates = platformAssemblies.Split(Path.PathSeparator, StringSplitOptions.RemoveEmptyEntries);

        foreach (var candidate in candidates)
        {
            if (string.IsNullOrWhiteSpace(candidate) || !File.Exists(candidate))
                continue;

            System.Reflection.AssemblyName? candidateIdentity;
            try
            {
                candidateIdentity = System.Reflection.AssemblyName.GetAssemblyName(candidate);
            }
            catch
            {
                continue;
            }

            if (candidateIdentity.Name is not { Length: > 0 } candidateName)
                continue;

            if (!_assemblyPathMap.ContainsKey(candidateName))
                _assemblyPathMap[candidateName] = candidate;

            if (_runtimeAssemblyCache.ContainsKey(candidateName))
                continue;

            var alreadyLoaded = AppDomain.CurrentDomain
                .GetAssemblies()
                .FirstOrDefault(a => string.Equals(a.GetName().Name, candidateName, StringComparison.OrdinalIgnoreCase));

            if (alreadyLoaded is not null)
                _runtimeAssemblyCache[candidateName] = alreadyLoaded;
        }
    }

    private static string? TryMapReferenceAssemblyToRuntimePath(string? metadataAssemblyPath)
    {
        if (string.IsNullOrEmpty(metadataAssemblyPath))
            return null;

        try
        {
            var assemblyFileName = Path.GetFileName(metadataAssemblyPath);
            if (string.IsNullOrEmpty(assemblyFileName))
                return null;

            var cursor = Path.GetDirectoryName(metadataAssemblyPath);
            if (cursor is null)
                return null;

            while (cursor is not null && !string.Equals(Path.GetFileName(cursor), "ref", StringComparison.OrdinalIgnoreCase))
            {
                cursor = Path.GetDirectoryName(cursor);
            }

            if (cursor is null)
                return null;

            var versionDirectory = Path.GetDirectoryName(cursor);
            if (versionDirectory is null)
                return null;

            var version = Path.GetFileName(versionDirectory);
            if (string.IsNullOrEmpty(version))
                return null;

            var packDirectory = Path.GetDirectoryName(versionDirectory);
            if (packDirectory is null)
                return null;

            var packId = Path.GetFileName(packDirectory);
            if (string.IsNullOrEmpty(packId))
                return null;

            var packsRoot = Path.GetDirectoryName(packDirectory);
            if (packsRoot is null || !string.Equals(Path.GetFileName(packsRoot), "packs", StringComparison.OrdinalIgnoreCase))
                return null;

            var dotnetRoot = Path.GetDirectoryName(packsRoot);
            if (string.IsNullOrEmpty(dotnetRoot))
                return null;

            var runtimePackId = packId.EndsWith(".Ref", StringComparison.OrdinalIgnoreCase)
                ? packId[..^4]
                : packId;

            var runtimeDirectory = Path.Combine(dotnetRoot, "shared", runtimePackId, version);
            var candidatePath = Path.Combine(runtimeDirectory, assemblyFileName);

            return File.Exists(candidatePath) ? candidatePath : null;
        }
        catch
        {
            return null;
        }
    }

    private Assembly? MapToRuntimeImplementation(AssemblyName identity)
    {
        if (identity.Name is null)
            return null;

        var runtimeCoreIdentity = RuntimeCoreAssembly.GetName();

        if (string.Equals(identity.Name, runtimeCoreIdentity.Name, StringComparison.OrdinalIgnoreCase))
            return RuntimeCoreAssembly;

        if (string.Equals(identity.Name, "System.Runtime", StringComparison.OrdinalIgnoreCase))
            return RuntimeCoreAssembly;

        if (string.Equals(identity.Name, "System.Private.CoreLib", StringComparison.OrdinalIgnoreCase))
            return RuntimeCoreAssembly;

        return null;
    }

    private static Assembly? LoadRuntimeAssemblyFromPath(AssemblyName identity, string? path)
    {
        if (string.IsNullOrEmpty(path))
            return null;

        try
        {
            return AssemblyLoadContext.Default.LoadFromAssemblyPath(path);
        }
        catch (FileLoadException)
        {
            return LoadRuntimeAssemblyByName(identity);
        }
        catch (BadImageFormatException)
        {
            return null;
        }
        catch (FileNotFoundException)
        {
            return LoadRuntimeAssemblyByName(identity);
        }
    }

    private static Assembly? LoadRuntimeAssemblyByName(AssemblyName identity)
    {
        try
        {
            return System.Reflection.Assembly.Load(identity);
        }
        catch
        {
            return null;
        }
    }

    internal Type? ResolveRuntimeType(PENamedTypeSymbol symbol)
    {
        if (symbol is null)
            throw new ArgumentNullException(nameof(symbol));

        EnsureSetup();

        if (symbol.ContainingAssembly is PEAssemblySymbol peAssembly)
            RegisterRuntimeAssembly(peAssembly.GetAssemblyInfo());

        var metadataName = ((INamedTypeSymbol)symbol).ToFullyQualifiedMetadataName();

        if (string.IsNullOrEmpty(metadataName))
            return null;

        var resolved = ResolveRuntimeType(metadataName);
        if (resolved is not null)
            return resolved;

        if (symbol.ContainingAssembly is PEAssemblySymbol peAssembly2)
        {
            if (!string.IsNullOrEmpty(peAssembly2.FullName))
            {
                var qualifiedName = $"{metadataName}, {peAssembly2.FullName}";
                var qualifiedType = Type.GetType(qualifiedName, throwOnError: false);
                if (qualifiedType is not null)
                {
                    RegisterRuntimeAssembly(qualifiedType.Assembly);
                    return qualifiedType;
                }
            }

            if (!string.IsNullOrEmpty(peAssembly2.Name))
            {
                try
                {
                    var assembly = System.Reflection.Assembly.Load(new AssemblyName(peAssembly2.Name));
                    RegisterRuntimeAssembly(assembly);
                    var type = assembly.GetType(metadataName, throwOnError: false, ignoreCase: false);
                    if (type is not null)
                        return type;
                }
                catch
                {
                    // Ignore load failures and fall through to null.
                }
            }
        }

        return null;
    }

    internal Type? ResolveRuntimeType(System.Reflection.TypeInfo metadataType)
    {
        if (metadataType is null)
            throw new ArgumentNullException(nameof(metadataType));

        EnsureSetup();

        RegisterRuntimeAssembly(metadataType.Assembly);

        if (metadataType.FullName is { Length: > 0 } fullName)
        {
            var resolved = ResolveRuntimeType(fullName);
            if (resolved is not null)
                return resolved;
        }

        if (metadataType.AssemblyQualifiedName is { Length: > 0 } qualifiedName)
            return Type.GetType(qualifiedName, throwOnError: false);

        return null;
    }

    internal Type? ResolveRuntimeType(string metadataName)
    {
        if (metadataName is null)
            throw new ArgumentNullException(nameof(metadataName));

        EnsureSetup();

        if (RuntimeCoreAssembly.GetType(metadataName, throwOnError: false, ignoreCase: false) is { } coreType)
            return coreType;

        foreach (var (key, runtimeAssembly) in _runtimeAssemblyCache)
        {
            var candidate = runtimeAssembly.GetType(metadataName, throwOnError: false, ignoreCase: false);
            if (candidate is not null)
                return candidate;
        }

        return Type.GetType(metadataName, throwOnError: false);
    }

    public INamedTypeSymbol? GetTypeByMetadataName(string metadataName)
    {
        EnsureSetup();

        if (!_sourceTypesInitialized && !_isPopulatingSourceTypes)
        {
            try
            {
                _isPopulatingSourceTypes = true;

                foreach (var syntaxTree in _syntaxTrees)
                    _ = GetSemanticModel(syntaxTree);

                _sourceTypesInitialized = true;
            }
            finally
            {
                _isPopulatingSourceTypes = false;
            }
        }

        if (Assembly.GetTypeByMetadataName(metadataName) is { } sourceType)
            return sourceType;

        INamedTypeSymbol? bestMatch = null;

        foreach (var assembly in _metadataReferenceSymbols.Values)
        {
            var type = assembly.GetTypeByMetadataName(metadataName);
            if (type is null)
                continue;

            if (bestMatch is null)
            {
                bestMatch = type;
                continue;
            }

            var currentAssembly = bestMatch.ContainingAssembly?.Name;
            if (!string.Equals(currentAssembly, "System.Runtime", StringComparison.OrdinalIgnoreCase) &&
                string.Equals(assembly.Name, "System.Runtime", StringComparison.OrdinalIgnoreCase))
            {
                bestMatch = type;
            }
        }

        return bestMatch;
    }

    private INamedTypeSymbol? GetTypeByMetadataName(string metadataName, string preferredAssembly)
    {
        foreach (var assembly in _metadataReferenceSymbols.Values)
        {
            if (!string.Equals(assembly.Name, preferredAssembly, StringComparison.OrdinalIgnoreCase))
                continue;

            var type = assembly.GetTypeByMetadataName(metadataName);
            if (type is not null)
                return type;
        }

        return null;
    }

    public INamedTypeSymbol GetSpecialType(SpecialType specialType)
    {
        if (specialType is SpecialType.System_Unit)
            return UnitTypeSymbol;

        var metadataName = specialType switch
        {
            SpecialType.System_Object => "System.Object",
            SpecialType.System_Enum => "System.Enum",
            SpecialType.System_MulticastDelegate => "System.MulticastDelegate",
            SpecialType.System_Delegate => "System.Delegate",
            SpecialType.System_ValueType => "System.ValueType",
            SpecialType.System_Void => "System.Void",
            SpecialType.System_Boolean => "System.Boolean",
            SpecialType.System_Char => "System.Char",
            SpecialType.System_SByte => "System.SByte",
            SpecialType.System_Byte => "System.Byte",
            SpecialType.System_Int16 => "System.Int16",
            SpecialType.System_UInt16 => "System.UInt16",
            SpecialType.System_Int32 => "System.Int32",
            SpecialType.System_UInt32 => "System.UInt32",
            SpecialType.System_Int64 => "System.Int64",
            SpecialType.System_UInt64 => "System.UInt64",
            SpecialType.System_Decimal => "System.Decimal",
            SpecialType.System_Single => "System.Single",
            SpecialType.System_Double => "System.Double",
            SpecialType.System_String => "System.String",
            SpecialType.System_IntPtr => "System.IntPtr",
            SpecialType.System_UIntPtr => "System.UIntPtr",
            SpecialType.System_Array => "System.Array",
            SpecialType.System_Collections_IEnumerable => "System.Collections.IEnumerable",
            SpecialType.System_Collections_Generic_IEnumerable_T => "System.Collections.Generic.IEnumerable`1",
            SpecialType.System_Collections_Generic_IList_T => "System.Collections.Generic.IList`1",
            SpecialType.System_Collections_Generic_ICollection_T => "System.Collections.Generic.ICollection`1",
            SpecialType.System_Collections_IEnumerator => "System.Collections.IEnumerator",
            SpecialType.System_Collections_Generic_IEnumerator_T => "System.Collections.Generic.IEnumerator`1",
            SpecialType.System_Nullable_T => "System.Nullable",
            SpecialType.System_DateTime => "System.DateTime",
            SpecialType.System_Runtime_CompilerServices_IsVolatile => "System.Runtime.CompilerServices.IsVolatile",
            SpecialType.System_IDisposable => "System.IDisposable",
            SpecialType.System_TypedReference => "System.TypedReference",
            SpecialType.System_ArgIterator => "System.ArgIterator",
            SpecialType.System_RuntimeArgumentHandle => "System.RuntimeArgumentHandle",
            SpecialType.System_RuntimeFieldHandle => "System.RuntimeFieldHandle",
            SpecialType.System_RuntimeMethodHandle => "System.RuntimeMethodHandle",
            SpecialType.System_RuntimeTypeHandle => "System.RuntimeTypeHandle",
            SpecialType.System_IAsyncResult => "System.IAsyncResult",
            SpecialType.System_AsyncCallback => "System.AsyncCallback",
            SpecialType.System_Runtime_CompilerServices_AsyncVoidMethodBuilder => "System.Runtime.CompilerServices.AsyncVoidMethodBuilder",
            SpecialType.System_Runtime_CompilerServices_AsyncTaskMethodBuilder => "System.Runtime.CompilerServices.AsyncTaskMethodBuilder",
            SpecialType.System_Runtime_CompilerServices_AsyncTaskMethodBuilder_T => "System.Runtime.CompilerServices.AsyncTaskMethodBuilder`1",
            SpecialType.System_Runtime_CompilerServices_AsyncStateMachineAttribute => "System.Runtime.CompilerServices.AsyncStateMachineAttribute",
            SpecialType.System_Runtime_CompilerServices_IteratorStateMachineAttribute => "System.Runtime.CompilerServices.IteratorStateMachineAttribute",
            SpecialType.System_Threading_Tasks_Task => "System.Threading.Tasks.Task",
            SpecialType.System_Threading_Tasks_Task_T => "System.Threading.Tasks.Task`1",
            SpecialType.System_Runtime_InteropServices_WindowsRuntime_EventRegistrationToken => "System.Runtime.InteropServices.WindowsRuntime.EventRegistrationToken",
            SpecialType.System_Runtime_InteropServices_WindowsRuntime_EventRegistrationTokenTable_T => "System.Runtime.InteropServices.WindowsRuntime.EventRegistrationTokenTable`1",
            SpecialType.System_ValueTuple_T1 => "System.ValueTuple`1",
            SpecialType.System_ValueTuple_T2 => "System.ValueTuple`2",
            SpecialType.System_ValueTuple_T3 => "System.ValueTuple`3",
            SpecialType.System_ValueTuple_T4 => "System.ValueTuple`4",
            SpecialType.System_ValueTuple_T5 => "System.ValueTuple`5",
            SpecialType.System_ValueTuple_T6 => "System.ValueTuple`6",
            SpecialType.System_ValueTuple_T7 => "System.ValueTuple`7",
            SpecialType.System_ValueTuple_TRest => "System.ValueTuple`8",
            SpecialType.System_Type => "System.Type",
            SpecialType.System_Exception => "System.Exception",
            SpecialType.System_Runtime_CompilerServices_IAsyncStateMachine => "System.Runtime.CompilerServices.IAsyncStateMachine",
            _ => throw new InvalidOperationException("Special type is not supported."),
        };

        var type = GetTypeByMetadataName(metadataName);

        if (type is INamedTypeSymbol { ContainingAssembly: { Name: var assemblyName } } &&
            !string.Equals(assemblyName, "System.Runtime", StringComparison.OrdinalIgnoreCase))
        {
            var preferred = GetTypeByMetadataName(metadataName, "System.Runtime");
            if (preferred is not null)
                type = preferred;
        }

        return type ?? (INamedTypeSymbol)ErrorTypeSymbol;
    }

    private static void InitializeTypeParameters(
      SourceNamedTypeSymbol typeSymbol,
      TypeParameterListSyntax? typeParameterList,
      SyntaxList<TypeParameterConstraintClauseSyntax> constraintClauses,
      SyntaxTree syntaxTree,
      DiagnosticBag? diagnostics = null) // optional: for unknown/dup clause reporting
    {
        if (typeParameterList is null || typeParameterList.Parameters.Count == 0)
            return;

        // Index clauses by parameter name (where T: ...)
        Dictionary<string, List<TypeParameterConstraintClauseSyntax>>? clausesByName = null;
        if (constraintClauses.Count > 0)
        {
            clausesByName = new Dictionary<string, List<TypeParameterConstraintClauseSyntax>>(StringComparer.Ordinal);

            foreach (var clause in constraintClauses)
            {
                var name = clause.TypeParameter.Identifier.ValueText;

                if (!clausesByName.TryGetValue(name, out var list))
                    clausesByName[name] = list = new List<TypeParameterConstraintClauseSyntax>();

                list.Add(clause);
            }
        }

        // Optional: validate clause names and duplicates
        if (diagnostics is not null && clausesByName is not null)
        {
            var declared = new HashSet<string>(
                typeParameterList.Parameters.Select(p => p.Identifier.ValueText),
                StringComparer.Ordinal);

            foreach (var (name, list) in clausesByName)
            {
                if (!declared.Contains(name))
                {
                    // You’ll want a real diagnostic here.
                    // diagnostics.Report... (unknown type parameter in constraint clause)
                }

                // If you want to forbid multiple where-clauses per parameter:
                // if (list.Count > 1) diagnostics.Report... (duplicate constraint clause)
            }
        }

        var builder = ImmutableArray.CreateBuilder<ITypeParameterSymbol>(typeParameterList.Parameters.Count);
        int ordinal = 0;

        foreach (var parameter in typeParameterList.Parameters)
        {
            var identifier = parameter.Identifier;
            var location = syntaxTree.GetLocation(identifier.Span);
            var reference = parameter.GetReference();

            // 1) inline constraints on the parameter node
            var (inlineKind, inlineRefs) = TypeParameterConstraintAnalyzer.AnalyzeInline(parameter);

            // 2) where-clauses targeting this parameter name
            var clauseKind = TypeParameterConstraintKind.None;
            var clauseRefsBuilder = ImmutableArray.CreateBuilder<SyntaxReference>();

            if (clausesByName is not null &&
                clausesByName.TryGetValue(parameter.Identifier.ValueText, out var matchingClauses))
            {
                foreach (var clause in matchingClauses)
                {
                    var (k, refs) = TypeParameterConstraintAnalyzer.AnalyzeClause(clause);
                    clauseKind |= k;
                    clauseRefsBuilder.AddRange(refs);
                }
            }

            var mergedKind = inlineKind | clauseKind;
            var mergedRefs = inlineRefs.AddRange(clauseRefsBuilder.ToImmutable());

            var variance = GetDeclaredVariance(parameter);

            var typeParameter = new SourceTypeParameterSymbol(
                identifier.Text,
                typeSymbol,
                typeSymbol,
                typeSymbol.ContainingNamespace,
                [location],
                [reference],
                ordinal++,
                mergedKind,
                mergedRefs,
                variance);

            builder.Add(typeParameter);
        }

        typeSymbol.SetTypeParameters(builder.MoveToImmutable());
    }

    private static VarianceKind GetDeclaredVariance(TypeParameterSyntax parameter)
    {
        return parameter.VarianceKeyword?.Kind switch
        {
            SyntaxKind.OutKeyword => VarianceKind.Out,
            SyntaxKind.InKeyword => VarianceKind.In,
            _ => VarianceKind.None,
        };
    }

    internal ITypeSymbol? TryBindTypeSyntaxWithoutBinder(TypeSyntax syntax)
    {
        // Minimal binding for explicit generic type arguments at overload-resolution time.
        // Supports predefined types and simple/qualified identifiers via metadata lookup.
        // Full fidelity binding still happens in the binder.

        switch (syntax)
        {
            case PredefinedTypeSyntax pts:
                return ResolvePredefinedType(pts);

            case IdentifierNameSyntax id:
                return GetTypeByMetadataName(id.Identifier.ValueText) ?? ErrorTypeSymbol;

            case QualifiedNameSyntax q:
                {
                    // Use the display string as a metadata name best-effort.
                    var name = q.ToString();
                    return GetTypeByMetadataName(name) ?? ErrorTypeSymbol;
                }

            case GenericNameSyntax:
                // Nested generic type args in explicit method arg lists are not supported here yet.
                return ErrorTypeSymbol;

            default:
                return ErrorTypeSymbol;
        }
    }
}
