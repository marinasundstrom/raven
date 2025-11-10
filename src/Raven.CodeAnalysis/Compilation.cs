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
    private TypeResolver _typeResolver;
    private bool _sourceTypesInitialized;
    private bool _isPopulatingSourceTypes;
    private readonly Dictionary<SyntaxTree, TopLevelProgramMembers> _topLevelProgramMembers = new();
    private BoundNodeFactory? _boundNodeFactory;

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

    public IAssemblySymbol Assembly { get; private set; }

    public IModuleSymbol Module { get; private set; }

    public IEnumerable<MetadataReference> References => _references;

    public IEnumerable<IAssemblySymbol> ReferencedAssemblySymbols => Module.ReferencedAssemblySymbols;

    public SyntaxTree[] SyntaxTrees => _syntaxTrees;

    public INamespaceSymbol GlobalNamespace
    {
        get
        {
            EnsureSetup();

            return _globalNamespace ??=
                new MergedNamespaceSymbol(
                    new INamespaceSymbol[] { SourceGlobalNamespace }.Concat(_metadataReferenceSymbols.Select(x => x.Value.GlobalNamespace)),
                    null);
        }
    }

    internal SourceNamespaceSymbol SourceGlobalNamespace { get; private set; }

    public Assembly CoreAssembly { get; private set; }
    public Assembly RuntimeCoreAssembly { get; private set; }

    internal BinderFactory BinderFactory { get; private set; }

    internal DeclarationTable DeclarationTable { get; private set; }

    internal SymbolFactory SymbolFactory { get; } = new SymbolFactory();

    internal BoundNodeFactory BoundNodeFactory => _boundNodeFactory ??= new BoundNodeFactory(this);

    public ITypeSymbol ErrorTypeSymbol => _errorTypeSymbol ??= new ErrorTypeSymbol(this, "Error", null, [], []);

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
        if (setup)
            return;

        lock (_setupLock)
        {
            if (!setup)
            {
                Setup();
                EnsureAsyncReturnTypes();
                setup = true;
            }
        }
    }

    private bool _asyncReturnTypesEnsured;

    private void EnsureAsyncReturnTypes()
    {
        if (_asyncReturnTypesEnsured)
            return;

        foreach (var tree in SyntaxTrees)
        {
            if (tree is null)
                continue;

            var semanticModel = GetSemanticModel(tree);
            var root = tree.GetRoot();

            foreach (var methodDeclaration in root.DescendantNodes().OfType<MethodDeclarationSyntax>())
            {
                if (semanticModel.GetDeclaredSymbol(methodDeclaration) is not SourceMethodSymbol methodSymbol)
                    continue;

                EnsureAsyncReturnType(semanticModel, methodSymbol, methodDeclaration.Body, methodDeclaration.ExpressionBody);
            }

            foreach (var functionStatement in root.DescendantNodes().OfType<FunctionStatementSyntax>())
            {
                if (semanticModel.GetDeclaredSymbol(functionStatement) is not SourceMethodSymbol functionSymbol)
                    continue;

                EnsureAsyncReturnType(semanticModel, functionSymbol, functionStatement.Body, functionStatement.ExpressionBody);
            }

            foreach (var accessor in root.DescendantNodes().OfType<AccessorDeclarationSyntax>())
            {
                if (semanticModel.GetDeclaredSymbol(accessor) is not SourceMethodSymbol accessorSymbol)
                    continue;

                EnsureAsyncReturnType(semanticModel, accessorSymbol, accessor.Body, accessor.ExpressionBody);
            }

            if (root is CompilationUnitSyntax compilationUnit)
                EnsureTopLevelAsyncReturnTypes(semanticModel, compilationUnit);
        }

        _asyncReturnTypesEnsured = true;
    }

    private void EnsureAsyncReturnType(
        SemanticModel semanticModel,
        SourceMethodSymbol methodSymbol,
        BlockStatementSyntax? bodySyntax,
        ArrowExpressionClauseSyntax? expressionBody)
    {
        if (!methodSymbol.IsAsync)
            return;

        if (!methodSymbol.RequiresAsyncReturnTypeInference || methodSymbol.AsyncReturnTypeInferenceComplete)
            return;

        TryBindAsyncBody(semanticModel, bodySyntax, expressionBody);
    }

    private void EnsureTopLevelAsyncReturnTypes(SemanticModel semanticModel, CompilationUnitSyntax compilationUnit)
    {
        var globalStatements = GetTopLevelGlobalStatements(compilationUnit).ToArray();
        if (globalStatements.Length == 0)
            return;

        static TopLevelBinder? FindTopLevelBinder(Binder? binder)
        {
            for (var current = binder; current is not null; current = current.ParentBinder)
            {
                if (current is TopLevelBinder topLevel)
                    return topLevel;
            }

            return null;
        }

        var binder = semanticModel.GetBinder(compilationUnit);
        var topLevelBinder = FindTopLevelBinder(binder) ?? FindTopLevelBinder(semanticModel.GetBinder(globalStatements[0]));
        if (topLevelBinder is null)
            return;

        if (topLevelBinder.MainMethod is not SourceMethodSymbol mainMethod)
            return;

        if (!mainMethod.IsAsync || mainMethod.AsyncReturnTypeInferenceComplete)
            return;

        topLevelBinder.BindGlobalStatements(globalStatements);
    }

    private void TryBindAsyncBody(
        SemanticModel semanticModel,
        BlockStatementSyntax? bodySyntax,
        ArrowExpressionClauseSyntax? expressionBody)
    {
        if (bodySyntax is not null)
        {
            _ = semanticModel.GetBoundNode(bodySyntax);
            return;
        }

        if (expressionBody is not null)
        {
            _ = semanticModel.GetBoundNode(expressionBody);
        }
    }

    private static IEnumerable<GlobalStatementSyntax> GetTopLevelGlobalStatements(CompilationUnitSyntax compilationUnit)
    {
        foreach (var member in compilationUnit.Members)
        {
            switch (member)
            {
                case GlobalStatementSyntax global:
                    yield return global;
                    break;
                case FileScopedNamespaceDeclarationSyntax fileScoped:
                    foreach (var nested in fileScoped.Members.OfType<GlobalStatementSyntax>())
                        yield return nested;
                    break;
            }
        }
    }

    private void Setup()
    {
        List<string> paths = _references
            .OfType<PortableExecutableReference>()
            .Select(portableExecutableReference => portableExecutableReference.FilePath)
            .ToList();

        var resolver = new PathAssemblyResolver(paths);
        _metadataLoadContext = new MetadataLoadContext(resolver);

        CoreAssembly = _metadataLoadContext.CoreAssembly!;
        RuntimeCoreAssembly = typeof(object).Assembly;
        RegisterRuntimeAssembly(CoreAssembly, RuntimeCoreAssembly.Location);

        foreach (var metadataReference in References)
        {
            GetAssemblyOrModuleSymbol(metadataReference);
        }

        BinderFactory = new BinderFactory(this);
        DeclarationTable = new DeclarationTable(SyntaxTrees);

        Assembly = new SourceAssemblySymbol(this, AssemblyName, []);

        Module = new SourceModuleSymbol(AssemblyName, (SourceAssemblySymbol)Assembly, _metadataReferenceSymbols.Values, []);

        SourceGlobalNamespace = (SourceNamespaceSymbol)Module.GlobalNamespace;

        InitializeTopLevelPrograms();
    }

    internal TypeResolver TypeResolver => _typeResolver ??= new TypeResolver(this);

    private void InitializeTopLevelPrograms()
    {
        foreach (var tree in SyntaxTrees)
        {
            if (tree is null)
                continue;

            if (tree.GetRoot() is not CompilationUnitSyntax compilationUnit)
                continue;

            var bindableGlobals = CollectBindableGlobalStatements(compilationUnit);
            if (bindableGlobals.Count == 0)
                continue;

            if (SyntaxTreeWithFileScopedCode is null)
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
            if (existing.MainMethod is null)
            {
                SpinWait.SpinUntil(() => existing.MainMethod is not null);
            }

            return (existing.ProgramClass, existing.MainMethod!, existing.AsyncMainMethod);
        }

        var returnsInt = bindableGlobals.Any(static g => ContainsReturnWithExpressionOutsideNestedFunctions(g.Statement));
        var requiresAsync = bindableGlobals.Any(static g => ContainsAwaitExpressionOutsideNestedFunctions(g.Statement));

        var programClass = new SynthesizedProgramClassSymbol(this, targetNamespace, [compilationUnit.GetLocation()], [compilationUnit.GetReference()]);

        var members = new TopLevelProgramMembers(programClass);
        _topLevelProgramMembers[compilationUnit.SyntaxTree] = members;

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
            returnsInt,
            asyncImplementation);

        members.MainMethod = mainMethod;
        members.AsyncMainMethod = asyncImplementation;

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

    internal static bool ContainsAwaitExpressionOutsideNestedFunctions(StatementSyntax statement)
    {
        return ContainsAwaitExpressionOutsideNestedFunctions((SyntaxNode)statement);

        static bool ContainsAwaitExpressionOutsideNestedFunctions(SyntaxNode node)
        {
            if (node is FunctionStatementSyntax or LambdaExpressionSyntax)
                return false;

            if (node.Kind == SyntaxKind.AwaitExpression)
                return true;

            foreach (var child in node.ChildNodes())
            {
                if (child is FunctionStatementSyntax or LambdaExpressionSyntax)
                    continue;

                if (ContainsAwaitExpressionOutsideNestedFunctions(child))
                    return true;
            }

            return false;
        }
    }

    internal static bool ContainsReturnWithExpressionOutsideNestedFunctions(StatementSyntax statement)
    {
        return ContainsReturnWithExpressionOutsideNestedFunctions((SyntaxNode)statement);

        static bool ContainsReturnWithExpressionOutsideNestedFunctions(SyntaxNode node)
        {
            if (node is FunctionStatementSyntax or LambdaExpressionSyntax)
                return false;

            if (node is ReturnStatementSyntax { Expression: not null })
                return true;

            foreach (var child in node.ChildNodes())
            {
                if (child is FunctionStatementSyntax or LambdaExpressionSyntax)
                    continue;

                if (ContainsReturnWithExpressionOutsideNestedFunctions(child))
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
    }

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
        else if (memberDeclaration is ClassDeclarationSyntax classDeclaration)
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

            INamedTypeSymbol baseTypeSymbol = GetSpecialType(SpecialType.System_Object);
            ImmutableArray<INamedTypeSymbol> interfaceList = ImmutableArray<INamedTypeSymbol>.Empty;

            if (classDeclaration.BaseList is not null)
            {
                var builder = ImmutableArray.CreateBuilder<INamedTypeSymbol>();
                foreach (var t in classDeclaration.BaseList.Types)
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

            var isAbstract = classDeclaration.Modifiers.Any(m => m.Kind == SyntaxKind.AbstractKeyword);
            var isSealed = !classDeclaration.Modifiers.Any(m => m.Kind == SyntaxKind.OpenKeyword) && !isAbstract;
            var typeAccessibility = AccessibilityUtilities.DetermineAccessibility(
                classDeclaration.Modifiers,
                AccessibilityUtilities.GetDefaultTypeAccessibility(declaringSymbol));

            var symbol = new SourceNamedTypeSymbol(
                classDeclaration.Identifier.ValueText,
                baseTypeSymbol,
                TypeKind.Class,
                declaringSymbol,
                containingType,
                containingNamespace,
                locations,
                references,
                isSealed,
                isAbstract,
                declaredAccessibility: typeAccessibility);

            InitializeTypeParameters(symbol, classDeclaration.TypeParameterList, syntaxTree);

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

            InitializeTypeParameters(symbol, interfaceDeclaration.TypeParameterList, syntaxTree);

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

            InitializeTypeParameters(symbol, extensionDeclaration.TypeParameterList, syntaxTree);

            foreach (var memberDeclaration2 in extensionDeclaration.Members)
            {
                AnalyzeMemberDeclaration(syntaxTree, symbol, memberDeclaration2);
            }
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

    public ITypeSymbol ResolvePredefinedType(PredefinedTypeSyntax predefinedType)
    {
        var keywordKind = predefinedType.Keyword.Kind;

        var specialType = keywordKind switch
        {
            SyntaxKind.BoolKeyword => SpecialType.System_Boolean,
            SyntaxKind.CharKeyword => SpecialType.System_Char,
            SyntaxKind.DoubleKeyword => SpecialType.System_Double,
            SyntaxKind.IntKeyword => SpecialType.System_Int32,
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
        return TypeResolver.ResolveType(type);
    }

    public ISymbol? GetAssemblyOrModuleSymbol(MetadataReference metadataReference)
    {
        if (!_metadataReferenceSymbols.TryGetValue(metadataReference, out var symbol))
        {
            switch (metadataReference)
            {
                case PortableExecutableReference per:
                {
                    var assembly = _metadataLoadContext.LoadFromAssemblyPath(per.FilePath);
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
                TypeResolver,
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

        return ResolveRuntimeType(metadataName);
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

        foreach (var runtimeAssembly in _runtimeAssemblyCache.Values)
        {
            var candidate = runtimeAssembly.GetType(metadataName, throwOnError: false, ignoreCase: false);
            if (candidate is not null)
                return candidate;
        }

        return Type.GetType(metadataName, throwOnError: false);
    }

    internal INamedTypeSymbol? TryGetTypeByMetadataNameAlreadySetup(string metadataName)
    {
        if (setup)
            return GetTypeByMetadataName(metadataName);

        if (Assembly is not null && Assembly.GetTypeByMetadataName(metadataName) is { } sourceType)
            return sourceType;

        foreach (var assembly in _metadataReferenceSymbols.Values)
        {
            var type = assembly.GetTypeByMetadataName(metadataName);
            if (type is not null)
                return type;
        }

        return null;
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

        foreach (var assembly in _metadataReferenceSymbols.Values)
        {
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
        return type ?? (INamedTypeSymbol)ErrorTypeSymbol;
    }

    private static void InitializeTypeParameters(SourceNamedTypeSymbol typeSymbol, TypeParameterListSyntax? typeParameterList, SyntaxTree syntaxTree)
    {
        if (typeParameterList is null || typeParameterList.Parameters.Count == 0)
            return;

        var builder = ImmutableArray.CreateBuilder<ITypeParameterSymbol>(typeParameterList.Parameters.Count);
        int ordinal = 0;
        foreach (var parameter in typeParameterList.Parameters)
        {
            var identifier = parameter.Identifier;
            var location = syntaxTree.GetLocation(identifier.Span);
            var reference = parameter.GetReference();

            var (constraintKind, constraintTypeReferences) = AnalyzeTypeParameterConstraints(parameter);
            var variance = GetDeclaredVariance(parameter);

            var typeParameter = new SourceTypeParameterSymbol(
                identifier.Text,
                typeSymbol,
                typeSymbol,
                typeSymbol.ContainingNamespace,
                [location],
                [reference],
                ordinal++,
                constraintKind,
                constraintTypeReferences,
                variance);

            builder.Add(typeParameter);
        }

        typeSymbol.SetTypeParameters(builder.MoveToImmutable());
    }

    private static (TypeParameterConstraintKind constraintKind, ImmutableArray<SyntaxReference> constraintTypeReferences) AnalyzeTypeParameterConstraints(TypeParameterSyntax parameter)
    {
        if (parameter.ColonToken is null)
            return (TypeParameterConstraintKind.None, ImmutableArray<SyntaxReference>.Empty);

        var constraints = parameter.Constraints;
        if (constraints.Count == 0)
            return (TypeParameterConstraintKind.None, ImmutableArray<SyntaxReference>.Empty);

        var constraintKind = TypeParameterConstraintKind.None;
        var typeConstraintReferences = ImmutableArray.CreateBuilder<SyntaxReference>();

        foreach (var constraint in constraints)
        {
            switch (constraint)
            {
                case ClassConstraintSyntax:
                    constraintKind |= TypeParameterConstraintKind.ReferenceType;
                    break;
                case StructConstraintSyntax:
                    constraintKind |= TypeParameterConstraintKind.ValueType;
                    break;
                case TypeConstraintSyntax typeConstraint:
                    constraintKind |= TypeParameterConstraintKind.TypeConstraint;
                    typeConstraintReferences.Add(typeConstraint.GetReference());
                    break;
            }
        }

        return (constraintKind, typeConstraintReferences.ToImmutable());
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
}
