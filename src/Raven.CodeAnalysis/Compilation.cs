using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;
using System.Reflection;

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
    private MetadataLoadContext _metadataLoadContext;
    private GlobalBinder _globalBinder;
    private bool setup;
    private ErrorTypeSymbol _errorTypeSymbol;
    private NullTypeSymbol _nullTypeSymbol;
    private UnitTypeSymbol _unitTypeSymbol;
    private TypeResolver _typeResolver;
    private bool _sourceTypesInitialized;
    private bool _isPopulatingSourceTypes;

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

    internal BinderFactory BinderFactory { get; private set; }

    internal DeclarationTable DeclarationTable { get; private set; }

    internal SymbolFactory SymbolFactory { get; } = new SymbolFactory();

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
                setup = true;
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

        foreach (var metadataReference in References)
        {
            GetAssemblyOrModuleSymbol(metadataReference);
        }

        BinderFactory = new BinderFactory(this);
        DeclarationTable = new DeclarationTable(SyntaxTrees);

        Assembly = new SourceAssemblySymbol(this, AssemblyName, []);

        Module = new SourceModuleSymbol(AssemblyName, (SourceAssemblySymbol)Assembly, _metadataReferenceSymbols.Values, []);

        SourceGlobalNamespace = (SourceNamespaceSymbol)Module.GlobalNamespace;
    }

    internal TypeResolver TypeResolver => _typeResolver ??= new TypeResolver(this);

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

        return this.GetNamespaceSymbol(ns) ?? currentSourceNamespace;
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
            var defaultAccessibility = containingType is not null
                ? AccessibilityUtilities.GetDefaultMemberAccessibility(containingType)
                : AccessibilityUtilities.GetDefaultTypeAccessibility(declaringSymbol);
            var methodAccessibility = AccessibilityUtilities.DetermineAccessibility(
                methodDeclaration.Modifiers,
                defaultAccessibility);

            _ = new SourceMethodSymbol(
                methodDeclaration.Identifier.ValueText, returnType,
                ImmutableArray<SourceParameterSymbol>.Empty,
                declaringSymbol,
                containingType,
                containingNamespace,
                locations, references,
                isStatic: isStatic,
                declaredAccessibility: methodAccessibility);
        }
    }

    public ITypeSymbol CreateArrayTypeSymbol(ITypeSymbol elementType, int rank = 1)
    {
        var ns = GlobalNamespace.LookupNamespace("System");
        return new ArrayTypeSymbol(GetSpecialType(SpecialType.System_Array), elementType, ns, null, ns, [], rank);
    }

    public ITypeSymbol CreateFunctionTypeSymbol(ITypeSymbol[] parameterTypes, ITypeSymbol returnType)
    {
        var systemNamespace = GlobalNamespace.LookupNamespace("System");

        var allTypes = parameterTypes.ToList();
        bool isAction = returnType.SpecialType == SpecialType.System_Void || returnType.SpecialType == SpecialType.System_Unit;

        if (!isAction)
            allTypes.Add(returnType);

        string delegateName = isAction ? "Action" : "Func";

        var delegateType = systemNamespace?.GetMembers(delegateName)
            .OfType<INamedTypeSymbol>()
            .FirstOrDefault(t => t.Arity == allTypes.Count);

        if (delegateType is null)
            return ErrorTypeSymbol;

        return delegateType.Construct(allTypes.ToArray());
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
