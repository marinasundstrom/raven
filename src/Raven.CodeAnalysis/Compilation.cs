using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;
using System.Reflection;

using Raven.CodeAnalysis.CodeGen;
using Raven.CodeAnalysis.Symbols;
using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis;

public class Compilation
{
    private INamespaceSymbol? _globalNamespace;
    private readonly SyntaxTree[] _syntaxTrees;
    private readonly MetadataReference[] _references;
    internal SyntaxTree? SyntaxTreeWithFileScopedCode;
    private readonly Dictionary<SyntaxTree, SemanticModel> _semanticModels = new Dictionary<SyntaxTree, SemanticModel>();
    private readonly Dictionary<MetadataReference, IAssemblySymbol> _metadataReferenceSymbols = new Dictionary<MetadataReference, IAssemblySymbol>();
    private readonly Dictionary<Assembly, IAssemblySymbol> _assemblySymbols = new Dictionary<Assembly, IAssemblySymbol>();

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

    public IAssemblySymbol Assembly { get; private set; }

    public IModuleSymbol Module { get; private set; }

    public IEnumerable<MetadataReference> References => _references;

    public IEnumerable<IAssemblySymbol> ReferencedAssemblySymbols => Module.ReferencedAssemblySymbols;

    public SyntaxTree[] SyntaxTrees => _syntaxTrees;

    public INamespaceSymbol GlobalNamespace =>
        _globalNamespace ??= new MergedNamespaceSymbol(new INamespaceSymbol[] { SourceGlobalNamespace }.Concat(_metadataReferenceSymbols.Select(x => x.Value.GlobalNamespace)), null);

    internal SourceNamespaceSymbol SourceGlobalNamespace { get; private set; }

    public Assembly CoreAssembly { get; private set; }

    internal BinderFactory BinderFactory { get; private set; }

    internal DeclarationTable DeclarationTable { get; private set; }

    internal SymbolFactory SymbolFactory { get; } = new SymbolFactory();

    public ITypeSymbol ErrorTypeSymbol => _errorTypeSymbol ??= new ErrorTypeSymbol(this, "Error", null, [], []);

    public ITypeSymbol NullTypeSymbol => _nullTypeSymbol ??= new NullTypeSymbol(this);
    public INamedTypeSymbol UnitTypeSymbol => _unitTypeSymbol ??= new UnitTypeSymbol(this);

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

    public SemanticModel GetSemanticModel(SyntaxTree syntaxTree)
    {
        EnsureSetup();

        if (_semanticModels.TryGetValue(syntaxTree, out var semanticModel))
        {
            return semanticModel;
        }

        if (!_syntaxTrees.Contains(syntaxTree))
        {
            throw new ArgumentNullException(nameof(syntaxTree), "Syntax tree is not part of compilation");
        }

        semanticModel = new SemanticModel(this, syntaxTree);
        _semanticModels[syntaxTree] = semanticModel;
        return semanticModel;
    }

    public MetadataReference ToMetadataReference() => new CompilationReference(this);

    public EmitResult Emit(Stream peStream, Stream? pdbStream = null)
    {
        var diagnostics = GetDiagnostics();

        if (diagnostics.Any(x => x.Severity == DiagnosticSeverity.Error))
        {
            return new EmitResult(false, diagnostics);
        }

        new CodeGenerator(this).Emit(peStream, pdbStream);

        return new EmitResult(true, diagnostics);
    }

    public IMethodSymbol? GetEntryPoint(CancellationToken cancellationToken = default)
    {
        var main = SourceGlobalNamespace
            .GetAllMembersRecursive()
            .OfType<IMethodSymbol>()
            .FirstOrDefault(x => x.Name == "Main" && x.ContainingType?.Name == "Program");

        return main;
    }

    private readonly object _setupLock = new();

    internal void EnsureSetup()
    {
        if (setup) return;

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

        Assembly = new SourceAssemblySymbol(AssemblyName, []);

        Module = new SourceModuleSymbol(AssemblyName, (SourceAssemblySymbol)Assembly, _metadataReferenceSymbols.Values, []);

        SourceGlobalNamespace = (SourceNamespaceSymbol)Module.GlobalNamespace;

        /*
        foreach (var syntaxTree in SyntaxTrees)
        {
            var root = syntaxTree.GetRoot();

            Location[] locations = [syntaxTree.GetLocation(root.Span)];

            SyntaxReference[] references = [root.GetReference()];

            foreach (var memberDeclaration in root.Members)
            {
                AnalyzeMemberDeclaration(syntaxTree, SourceGlobalNamespace, memberDeclaration);
            }
        }
        */
    }

    private readonly Dictionary<string, Assembly> _lazyMetadataAssemblies = new();
    private MetadataLoadContext _metadataLoadContext;
    private GlobalBinder _globalBinder;
    private bool setup;
    private ErrorTypeSymbol _errorTypeSymbol;
    private NullTypeSymbol _nullTypeSymbol;
    private UnitTypeSymbol _unitTypeSymbol;
    private TypeResolver _typeResolver;

    internal TypeResolver TypeResolver => _typeResolver ??= new TypeResolver(this);

    internal INamespaceSymbol? GetOrCreateNamespaceSymbol(string? ns)
    {
        if (ns is null)
            return GlobalNamespace;

        // Split the namespace into parts
        var namespaceParts = ns.Split('.');

        // Start with the global namespace
        var currentNamespace = GlobalNamespace;

        // Traverse the namespace hierarchy
        foreach (var part in namespaceParts)
        {
            var parent = currentNamespace;
            currentNamespace = currentNamespace.GetMembers().FirstOrDefault(n => n.Name == part) as INamespaceSymbol;

            if (currentNamespace == null)
            {
                currentNamespace = new PENamespaceSymbol(TypeResolver, part, null!, parent);
                return currentNamespace; // Namespace not found
            }
        }

        return currentNamespace;
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

            var symbol = new SourceNamedTypeSymbol(
                classDeclaration.Identifier.Text,
                GetSpecialType(SpecialType.System_Object),
                TypeKind.Class,
                declaringSymbol,
                containingType,
                containingNamespace,
            locations, references);

            foreach (var memberDeclaration2 in classDeclaration.Members)
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

            var symbol = new SourceMethodSymbol(
                methodDeclaration.Identifier.Text.ToString(), returnType,
                ImmutableArray<SourceParameterSymbol>.Empty,
                declaringSymbol,
                containingType,
                containingNamespace,
                locations, references);
        }
    }

    public ImmutableArray<Diagnostic> GetDiagnostics(CompilationWithAnalyzersOptions? analyzerOptions = null, CancellationToken cancellationToken = default)
    {
        var diagnostics = new List<Diagnostic>();

        foreach (var syntaxTree in SyntaxTrees)
        {
            foreach (var diagnostic in syntaxTree.GetDiagnostics(cancellationToken))
                Add(diagnostic);

            var model = GetSemanticModel(syntaxTree);
            foreach (var diagnostic in model.GetDiagnostics(cancellationToken))
                Add(diagnostic);
        }

        return diagnostics.OrderBy(x => x.Location).ToImmutableArray();

        void Add(Diagnostic diagnostic)
        {
            var mapped = ApplyCompilationOptions(diagnostic, analyzerOptions?.ReportSuppressedDiagnostics ?? false);
            if (mapped is not null)
                diagnostics.Add(mapped);
        }
    }

    internal Diagnostic? ApplyCompilationOptions(Diagnostic diagnostic, bool reportSuppressedDiagnostics = false)
    {
        if (Options.SpecificDiagnosticOptions.TryGetValue(diagnostic.Descriptor.Id, out var report))
        {
            if (report == ReportDiagnostic.Suppress)
                return reportSuppressedDiagnostics ? diagnostic.WithSuppression(true) : null;

            if (report != ReportDiagnostic.Default)
            {
                var severity = report switch
                {
                    ReportDiagnostic.Error => DiagnosticSeverity.Error,
                    ReportDiagnostic.Warn => DiagnosticSeverity.Warning,
                    ReportDiagnostic.Info => DiagnosticSeverity.Info,
                    ReportDiagnostic.Hidden => DiagnosticSeverity.Hidden,
                    _ => diagnostic.Severity
                };

                if (severity != diagnostic.Severity)
                    return diagnostic.WithSeverity(severity);
            }
        }

        return diagnostic;
    }

    public Conversion ClassifyConversion(ITypeSymbol source, ITypeSymbol destination)
    {
        // Temporary
        if (destination is null) return Conversion.None;

        if (source is LiteralTypeSymbol litSrc)
            return ClassifyConversion(litSrc.UnderlyingType, destination);
        if (destination is LiteralTypeSymbol litDest)
            return ClassifyConversion(source, litDest.UnderlyingType);

        if (SymbolEqualityComparer.Default.Equals(source, destination) &&
            source is not NullableTypeSymbol &&
            destination is not NullableTypeSymbol)
        {
            // Identity conversion
            return new Conversion(isImplicit: true, isIdentity: true);
        }

        if (source.TypeKind == TypeKind.Null)
        {
            if (destination.TypeKind == TypeKind.Nullable || destination.TypeKind == TypeKind.Union)
                return new Conversion(isImplicit: true, isReference: true);

            return Conversion.None;
        }

        if (source is NullableTypeSymbol nullableSource)
        {
            var conv = ClassifyConversion(nullableSource.UnderlyingType, destination);
            if (conv.Exists)
            {
                var isImplicit = !SymbolEqualityComparer.Default.Equals(nullableSource.UnderlyingType, destination) && conv.IsImplicit;
                return new Conversion(
                    isImplicit: isImplicit,
                    isIdentity: conv.IsIdentity,
                    isNumeric: conv.IsNumeric,
                    isReference: conv.IsReference,
                    isBoxing: conv.IsBoxing,
                    isUnboxing: conv.IsUnboxing,
                    isUserDefined: conv.IsUserDefined);
            }
        }

        if (destination is NullableTypeSymbol nullableDest)
        {
            if (source is IUnionTypeSymbol unionSource &&
                unionSource.Types.Count() == 2 &&
                unionSource.Types.Any(t => t.TypeKind == TypeKind.Null) &&
                unionSource.Types.Any(t => SymbolEqualityComparer.Default.Equals(t, nullableDest.UnderlyingType)))
            {
                return new Conversion(isImplicit: true, isReference: true);
            }

            var conv = ClassifyConversion(source, nullableDest.UnderlyingType);
            if (conv.Exists)
                return new Conversion(
                    isImplicit: true,
                    isIdentity: false,
                    isNumeric: conv.IsNumeric,
                    isReference: conv.IsReference || !source.IsValueType,
                    isBoxing: conv.IsBoxing,
                    isUnboxing: conv.IsUnboxing,
                    isUserDefined: conv.IsUserDefined);
        }

        if (source is IUnionTypeSymbol unionSource2)
        {
            var conversions = unionSource2.Types.Select(t => ClassifyConversion(t, destination)).ToArray();
            if (conversions.All(c => c.Exists))
            {
                var isImplicit = conversions.All(c => c.IsImplicit);
                var isReference = conversions.Any(c => c.IsReference);
                var isBoxing = conversions.Any(c => c.IsBoxing);
                return new Conversion(
                    isImplicit: isImplicit,
                    isReference: isReference,
                    isBoxing: isBoxing);
            }

            return Conversion.None;
        }

        if (source.SpecialType == SpecialType.System_Unit)
        {
            if (destination is IUnionTypeSymbol unionDest && unionDest.Types.Any(t => t.SpecialType == SpecialType.System_Unit))
                return new Conversion(isImplicit: true, isReference: true);
            return Conversion.None;
        }

        if (source.SpecialType == SpecialType.System_Void)
            return Conversion.None;

        var objType = GetSpecialType(SpecialType.System_Object);

        if (destination.Equals(objType, SymbolEqualityComparer.Default))
        {
            if (source.Equals(objType, SymbolEqualityComparer.Default))
            {
                return Conversion.None;
            }

            // Assigning to object
            return new Conversion(
                isImplicit: true,
                isReference: !source.IsValueType,
                isBoxing: source.IsValueType);
        }

        if (destination is IUnionTypeSymbol unionType)
        {
            var match = unionType.Types.FirstOrDefault(x => ClassifyConversion(source, x).Exists);
            if (match is null)
                return Conversion.None;

            return new Conversion(isImplicit: true, isBoxing: source.IsValueType);
        }

        if (IsReferenceConversion(source, destination))
        {
            // Reference conversion
            return new Conversion(isImplicit: true, isReference: true);
        }

        if (IsBoxingConversion(source, destination))
        {
            // Boxing conversion
            return new Conversion(isImplicit: true, isBoxing: true);
        }

        if (IsUnboxingConversion(source, destination))
        {
            // Unboxing conversion
            return new Conversion(isImplicit: false, isUnboxing: true);
        }

        if (IsImplicitNumericConversion(source, destination))
        {
            // Implicit numeric conversion
            return new Conversion(isImplicit: true, isNumeric: true);
        }

        if (IsExplicitNumericConversion(source, destination))
        {
            // Explicit numeric conversion
            return new Conversion(isImplicit: false, isNumeric: true);
        }

        // User-defined conversions
        var sourceNamed = source as INamedTypeSymbol;
        var destinationNamed = destination as INamedTypeSymbol;

        if (sourceNamed != null || destinationNamed != null)
        {
            IEnumerable<IMethodSymbol> candidateConversions =
                Enumerable.Empty<IMethodSymbol>();

            if (sourceNamed != null)
                candidateConversions = candidateConversions.Concat(sourceNamed.GetMembers().OfType<IMethodSymbol>());
            if (destinationNamed != null && !SymbolEqualityComparer.Default.Equals(source, destination))
                candidateConversions = candidateConversions.Concat(destinationNamed.GetMembers().OfType<IMethodSymbol>());

            foreach (var method in candidateConversions)
            {
                if (method.MethodKind is MethodKind.Conversion &&
                    method.Parameters.Length == 1 &&
                    SymbolEqualityComparer.Default.Equals(method.Parameters[0].Type, source) &&
                    SymbolEqualityComparer.Default.Equals(method.ReturnType, destination))
                {
                    var isImplicit = method.Name == "op_Implicit";
                    return new Conversion(isImplicit: isImplicit, isUserDefined: true);
                }
            }
        }

        // No valid conversion
        return Conversion.None;
    }

    private bool IsReferenceConversion(ITypeSymbol source, ITypeSymbol destination)
    {
        // Must both be reference types
        if (source.IsValueType || destination.IsValueType)
            return false;

        // Identity conversion is not a reference conversion
        if (SymbolEqualityComparer.Default.Equals(source, destination))
            return false;

        // Walk base types to see if destination is in the chain
        var current = source.BaseType;
        while (current is not null)
        {
            if (SymbolEqualityComparer.Default.Equals(current, destination))
                return true;

            current = current.BaseType;
        }

        return false;
    }

    private bool IsBoxingConversion(ITypeSymbol source, ITypeSymbol destination)
    {
        return source.IsValueType && destination.SpecialType is SpecialType.System_Object;
    }

    private bool IsUnboxingConversion(ITypeSymbol source, ITypeSymbol destination)
    {
        return source.SpecialType is SpecialType.System_Object && destination.IsValueType;
    }

    private bool IsImplicitNumericConversion(ITypeSymbol source, ITypeSymbol destination)
    {
        // Add logic for implicit numeric conversions (e.g., int to long, float to double)
        var sourceType = source.SpecialType;
        var destType = destination.SpecialType;

        return (sourceType is SpecialType.System_Int32 && destType is SpecialType.System_Int64) ||
               (sourceType is SpecialType.System_Single && destType is SpecialType.System_Double);
    }

    private bool IsExplicitNumericConversion(ITypeSymbol source, ITypeSymbol destination)
    {
        // Add logic for explicit numeric conversions (e.g., double to int)
        var sourceType = source.SpecialType;
        var destType = destination.SpecialType;

        return (sourceType is SpecialType.System_Double && destType is SpecialType.System_Int32) ||
               (sourceType is SpecialType.System_Int64 && destType is SpecialType.System_Int32);
    }

    public INamedTypeSymbol? GetTypeByMetadataName(string metadataName)
    {
        EnsureSetup();

        return _metadataReferenceSymbols
            .Select(x => x.Value)
            .Select(x => x.GetTypeByMetadataName(metadataName))
            .Where(x => x is not null)
            .FirstOrDefault();
    }

    public INamedTypeSymbol GetSpecialType(SpecialType specialType)
    {
        if (specialType is SpecialType.System_Unit)
        {
            return UnitTypeSymbol;
        }
        else if (specialType is SpecialType.System_Void)
        {
            return GetTypeByMetadataName("System.Void");
        }
        else if (specialType is SpecialType.System_Boolean)
        {
            return GetTypeByMetadataName("System.Boolean");
        }
        else if (specialType is SpecialType.System_Int32)
        {
            return GetTypeByMetadataName("System.Int32");
        }
        else if (specialType is SpecialType.System_Int64)
        {
            return GetTypeByMetadataName("System.Int64");
        }
        else if (specialType is SpecialType.System_Single)
        {
            return GetTypeByMetadataName("System.Single");
        }
        else if (specialType is SpecialType.System_Double)
        {
            return GetTypeByMetadataName("System.Double");
        }
        else if (specialType is SpecialType.System_String)
        {
            return GetTypeByMetadataName("System.String");
        }
        else if (specialType is SpecialType.System_Char)
        {
            return GetTypeByMetadataName("System.Char");
        }
        else if (specialType is SpecialType.System_Array)
        {
            return GetTypeByMetadataName("System.Array");
        }
        else if (specialType is SpecialType.System_Object)
        {
            return GetTypeByMetadataName("System.Object");
        }
        else if (specialType is SpecialType.System_Nullable_T)
        {
            return GetTypeByMetadataName("System.Nullable");
        }

        throw new InvalidOperationException("Special type is not supported.");
    }

    public ITypeSymbol ResolvePredefinedType(PredefinedTypeSyntax predefinedType)
    {
        var keywordKind = predefinedType.Keyword.Kind;

        var specialType = keywordKind switch
        {
            SyntaxKind.BoolKeyword => SpecialType.System_Boolean,
            //SyntaxKind.ByteKeyword => SpecialType.System_Byte,
            SyntaxKind.CharKeyword => SpecialType.System_Char,
            //SyntaxKind.DecimalKeyword => SpecialType.System_Decimal,
            //SyntaxKind.DoubleKeyword => SpecialType.System_Double,
            //SyntaxKind.FloatKeyword => SpecialType.System_Single,
            SyntaxKind.IntKeyword => SpecialType.System_Int32,
            //SyntaxKind.LongKeyword => SpecialType.System_Int64,
            //SyntaxKind.ObjectKeyword => SpecialType.System_Object,
            //SyntaxKind.SByteKeyword => SpecialType.System_SByte,
            //SyntaxKind.ShortKeyword => SpecialType.System_Int16,
            SyntaxKind.StringKeyword => SpecialType.System_String,
            //SyntaxKind.UIntKeyword => SpecialType.System_UInt32,
            //SyntaxKind.ULongKeyword => SpecialType.System_UInt64,
            //SyntaxKind.UShortKeyword => SpecialType.System_UInt16,
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
        //if (!_references.Contains(metadataReference))
        //    throw new InvalidOperationException();

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
                        // Ignore failed loads
                        return null;
                    }
                }).Where(x => x is not null).ToArray()));

        _assemblySymbols[assembly] = assemblySymbol;

        return assemblySymbol;
    }

    public ITypeSymbol CreateArrayTypeSymbol(ITypeSymbol elementType)
    {
        var ns = GlobalNamespace.LookupNamespace("System");
        return new ArrayTypeSymbol(GetSpecialType(SpecialType.System_Array), elementType, ns, null, ns, []);
    }

    /*
    internal object CreateDelegateTypeSymbol(List<ITypeSymbol> parameterTypes, ITypeSymbol returnType)
    {
        var ns = GlobalNamespace.LookupNamespace("System");
        return new DelegateTypeSymbol(GetSpecialType(SpecialType.System_Delegate), returnType, parameterTypes, ns, null, ns, []);
    }
    */

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

        // Construct the generic Func<> or Action<> with type arguments
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
}
