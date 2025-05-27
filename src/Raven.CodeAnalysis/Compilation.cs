using System.Collections.Immutable;
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

    public ITypeSymbol ErrorTypeSymbol => _errorTypeSymbol ??= new ErrorTypeSymbol(this, "Error", null, [], []);

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

        if (diagnostics.Any(x => x.Descriptor.DefaultSeverity == DiagnosticSeverity.Error))
        {
            return new EmitResult(false, diagnostics);
        }

        new CodeGenerator(this).Generate(peStream, pdbStream);

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

    private void EnsureSetup()
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

        // Test
        var x = GetCoreAssembly();

        foreach (var metadataReference in References)
        {
            GetAssemblyOrModuleSymbol(metadataReference);
        }

        BinderFactory = new BinderFactory(this);

        Assembly = new SourceAssemblySymbol(AssemblyName, []);

        Module = new SourceModuleSymbol(AssemblyName, (SourceAssemblySymbol)Assembly, _metadataReferenceSymbols.Values, []);

        SourceGlobalNamespace = (SourceNamespaceSymbol)Module.GlobalNamespace;

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
    }

    private readonly Dictionary<string, Assembly> _lazyMetadataAssemblies = new();
    private MetadataLoadContext _metadataLoadContext;
    private GlobalBinder _globalBinder;
    private bool setup;
    private ErrorTypeSymbol _errorTypeSymbol;

    private INamespaceSymbol? GetOrCreateNamespaceSymbol(string? ns)
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
                currentNamespace = new PENamespaceSymbol(part, null!, parent);
                return currentNamespace; // Namespace not found
            }
        }

        return currentNamespace;
    }

    private void AnalyzeMemberDeclaration(SyntaxTree syntaxTree, ISymbol declaringSymbol, MemberDeclarationSyntax memberDeclaration)
    {
        if (memberDeclaration is BaseNamespaceDeclarationSyntax namespaceDeclarationSyntax)
        {
            Location[] locations = [syntaxTree.GetLocation(namespaceDeclarationSyntax.Span)];

            SyntaxReference[] references = [namespaceDeclarationSyntax.GetReference()];

            var symbol = new SourceNamespaceSymbol(
                namespaceDeclarationSyntax.Name.ToString(), declaringSymbol, (INamespaceSymbol?)declaringSymbol,
                locations, references);

            foreach (var memberDeclaration2 in namespaceDeclarationSyntax.Members)
            {
                AnalyzeMemberDeclaration(syntaxTree, symbol, memberDeclaration2);
            }
        }
        else if (memberDeclaration is MethodDeclarationSyntax methodDeclaration)
        {
            Location[] locations = [syntaxTree.GetLocation(methodDeclaration.Span)];

            SyntaxReference[] references = [methodDeclaration.GetReference()];

            ITypeSymbol typeSymbol = null!;

            var symbol = new SourceMethodSymbol(
                methodDeclaration.Name.Identifier.Text.ToString(), typeSymbol, [], null!, null, null,
                locations, references);
        }
    }

    public ImmutableArray<Diagnostic> GetDiagnostics(CancellationToken cancellationToken = default)
    {
        List<Diagnostic> diagnostics = new List<Diagnostic>();

        foreach (var syntaxTree in SyntaxTrees)
        {
            diagnostics.AddRange(syntaxTree.GetDiagnostics(cancellationToken));

            var model = GetSemanticModel(syntaxTree);
            diagnostics.AddRange(model.GetDiagnostics(cancellationToken));
        }

        /*
        var diagnostics = BinderFactory
            .GetAllBinders()
            .SelectMany(b => b.Diagnostics.AsEnumerable())
            .ToImmutableArray();
        
        diagnostics.AddRange(model.GetDiagnostics(cancellationToken));
        */

        return diagnostics.OrderBy(x => x.Location).ToImmutableArray();
    }

    public Conversion ClassifyConversion(ITypeSymbol source, ITypeSymbol destination)
    {
        // INFO: Temp
        if (destination is null)
            return new Conversion(isImplicit: false);

        if (SymbolEqualityComparer.Default.Equals(source, destination))
        {
            // Identity conversion
            return new Conversion(isImplicit: true, isIdentity: true);
        }

        if (destination is IUnionTypeSymbol unionType)
        {
            var v = unionType.Types.First(x => x.Equals(source, SymbolEqualityComparer.Default));
            return new Conversion(isImplicit: true, isBoxing: v.IsValueType);
        }

        /*
                if (IsReferenceConversion(source, destination))
                {
                    // Reference conversion
                    return new Conversion(isImplicit: true, isReference: true);
                }
                */

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
            return new Conversion(isImplicit: true);
        }

        if (IsExplicitNumericConversion(source, destination))
        {
            // Explicit numeric conversion
            return new Conversion(isImplicit: false);
        }

        // No valid conversion
        return Conversion.None;
    }

    private bool IsReferenceConversion(ITypeSymbol source, ITypeSymbol destination)
    {
        return source is INamedTypeSymbol sourceNamed &&
               destination is INamedTypeSymbol destNamed &&
               this.ClassifyConversion(sourceNamed, destNamed).IsImplicit;
    }

    private bool IsBoxingConversion(ITypeSymbol source, ITypeSymbol destination)
    {
        return source.IsValueType && destination.SpecialType == SpecialType.System_Object;
    }

    private bool IsUnboxingConversion(ITypeSymbol source, ITypeSymbol destination)
    {
        return source.SpecialType == SpecialType.System_Object && destination.IsValueType;
    }

    private bool IsImplicitNumericConversion(ITypeSymbol source, ITypeSymbol destination)
    {
        // Add logic for implicit numeric conversions (e.g., int to long, float to double)
        var sourceType = source.SpecialType;
        var destType = destination.SpecialType;

        return (sourceType == SpecialType.System_Int32 && destType == SpecialType.System_Int64) ||
               (sourceType == SpecialType.System_Single && destType == SpecialType.System_Double);
    }

    private bool IsExplicitNumericConversion(ITypeSymbol source, ITypeSymbol destination)
    {
        // Add logic for explicit numeric conversions (e.g., double to int)
        var sourceType = source.SpecialType;
        var destType = destination.SpecialType;

        return (sourceType == SpecialType.System_Double && destType == SpecialType.System_Int32) ||
               (sourceType == SpecialType.System_Int64 && destType == SpecialType.System_Int32);
    }

    public INamedTypeSymbol? GetTypeByMetadataName(string metadataName)
    {
        return _metadataReferenceSymbols
            .Select(x => x.Value)
            .Select(x => x.GetTypeByMetadataName(metadataName))
            .FirstOrDefault();
    }

    public INamedTypeSymbol GetSpecialType(SpecialType specialType)
    {
        if (specialType == SpecialType.System_Void)
        {
            return GetTypeByMetadataName("System.Void");
        }
        else if (specialType == SpecialType.System_Boolean)
        {
            return GetTypeByMetadataName("System.Boolean");
        }
        else if (specialType == SpecialType.System_Int32)
        {
            return GetTypeByMetadataName("System.Int32");
        }
        else if (specialType == SpecialType.System_String)
        {
            return GetTypeByMetadataName("System.String");
        }
        else if (specialType == SpecialType.System_Char)
        {
            return GetTypeByMetadataName("System.Char");
        }
        else if (specialType == SpecialType.System_Array)
        {
            return GetTypeByMetadataName("System.Array");
        }

        return null;
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
            SyntaxKind.VoidKeyword => SpecialType.System_Void,
            _ => throw new Exception($"Unexpected predefined keyword: {keywordKind}")
        };

        return GetSpecialType(specialType)
               ?? throw new Exception($"Special type not found for: {specialType}");
    }

    public ITypeSymbol? GetType(Type type)
    {
        if (type.IsArray)
        {
            var elementType = GetType(type.GetElementType());
            return new ArrayTypeSymbol(GetSpecialType(SpecialType.System_Array), elementType, null, null, null, []);
        }

        return GetSimpleType(type);
    }

    private ITypeSymbol? GetSimpleType(Type type)
    {
        var typeInfo = type.GetTypeInfo();

        var ns = GetOrCreateNamespaceSymbol(type.Namespace);

        return ns.GetMembers(type.Name).FirstOrDefault() as ITypeSymbol;
    }

    public ISymbol? GetAssemblyOrModuleSymbol(MetadataReference metadataReference)
    {
        //if (!_references.Contains(metadataReference))
        //    throw new InvalidOperationException();

        if (!_metadataReferenceSymbols.TryGetValue(metadataReference, out var symbol))
        {
            var portableExecutableReference = metadataReference as PortableExecutableReference;
            if (portableExecutableReference is null)
                throw new InvalidOperationException();

            var assembly = _metadataLoadContext.LoadFromAssemblyPath(portableExecutableReference.FilePath);
            symbol = GetAssembly(assembly);
            _metadataReferenceSymbols[metadataReference] = symbol;
        }
        return symbol;
    }

    private IAssemblySymbol GetCoreAssembly()
    {
        return GetAssembly(CoreAssembly);
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

    internal Binder GetBinder(SyntaxNode node)
    {
        return BinderFactory.GetBinder(node);
    }
}