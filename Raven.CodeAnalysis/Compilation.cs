using System.Collections.Immutable;
using System.Reflection;

using Raven.CodeAnalysis.CodeGen;
using Raven.CodeAnalysis.Symbols;
using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis;

public class Compilation
{
    private SyntaxTree[] _syntaxTrees;
    private MetadataReference[] _references;
    private List<ISymbol> _symbols = new List<ISymbol>();
    private readonly Dictionary<SyntaxTree, SemanticModel> _semanticModels = new Dictionary<SyntaxTree, SemanticModel>();

    private Compilation(string? assemblyName, SyntaxTree[] syntaxTrees, MetadataReference[] references, CompilationOptions? options = null)
    {
        AssemblyName = assemblyName ?? "assembly";
        _syntaxTrees = syntaxTrees;
        _references = references;
        Options = options ?? new CompilationOptions();
    }

    public string AssemblyName { get; }

    public CompilationOptions Options { get; }

    public SyntaxTree[] SyntaxTrees => _syntaxTrees;

    public INamespaceSymbol GlobalNamespace { get; private set; }

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
        // TODO: Create new compilation
        return new Compilation(AssemblyName, syntaxTrees, _references, Options);
    }

    public Compilation AddReferences(MetadataReference[] references)
    {
        return new Compilation(AssemblyName, _syntaxTrees, references, Options);
    }

    public Compilation WithAssemblyName(string? assemblyName)
    {
        return new Compilation(assemblyName, _syntaxTrees, _references, Options);
    }

    public SemanticModel GetSemanticModel(SyntaxTree syntaxTree)
    {
        if (_semanticModels.TryGetValue(syntaxTree, out var semanticModel))
        {
            return semanticModel;
        }

        if (!_syntaxTrees.Contains(syntaxTree))
        {
            throw new ArgumentNullException(nameof(syntaxTree), "Syntax tree is not part of compilation");
        }

        semanticModel = new SemanticModel(this, _symbols, syntaxTree);
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

        new CodeGenerator().Generate(this, peStream, pdbStream);

        return new EmitResult(true, diagnostics);
    }

    public IMethodSymbol? GetEntryPoint(CancellationToken cancellationToken = default)
    {
        return _symbols.SingleOrDefault(x => x.Name == "Name" && x.ContainingType!.Name == "Program") as IMethodSymbol;
    }

    public Compilation AnalyzeCodeTemp()
    {
        var globalNamespace = new NamespaceSymbol(
            "", null!, null, null,
            [], []);

        GlobalNamespace = globalNamespace;

        LoadMetadataReferences();

        foreach (var syntaxTree in SyntaxTrees)
        {
            var root = syntaxTree.GetRoot();

            Location[] locations = [syntaxTree.GetLocation(root.Span)];

            SyntaxReference[] references = [new SyntaxReference(syntaxTree, root.Span)];

            var globalStatements = root.Members.OfType<GlobalStatementSyntax>();
            if (globalStatements.Any())
            {
                ITypeSymbol typeSymbol = null!;

                var symbol2 = new SourceTypeSymbol(
                    "Program", globalNamespace!, null, globalNamespace,
                    locations, references);

                _symbols.Add(symbol2);

                var symbol = new SourceMethodSymbol(
                    "Main", typeSymbol, symbol2!, symbol2, globalNamespace,
                    [syntaxTree.GetLocation(root.Span)], [new SyntaxReference(syntaxTree, root.Span)]);

                _symbols.Add(symbol);
            }
            else
            {
                foreach (var memberDeclaration in root.Members)
                {
                    AnalyzeMemberDeclaration(syntaxTree, globalNamespace, memberDeclaration);
                }
            }

            _symbols.Add(globalNamespace);
        }

        return this;
    }

    private void LoadMetadataReferences()
    {
        List<Assembly> assemblies = _references
            .OfType<PortableExecutableReference>()
            .Select(portableExecutableReference => Assembly.LoadFile(portableExecutableReference.Location))
            .ToList();

        assemblies.Insert(0, typeof(object).Assembly);

        foreach (var assembly in assemblies)
        {
            var module = assembly.GetModules().First();

            foreach (var type in module.GetTypes())
            {
                var ns = GetOrCreateNamespaceSymbol(type.Namespace);

                var symbol = new MetadataTypeSymbol(this,
                    type.GetTypeInfo(), ns, null, ns,
                    []);

                foreach (var mi in type.GetMethods())
                {
                    var symbol2 = new MetadataMethodSymbol(this,
                        mi, null, symbol!, symbol, ns,
                        []);

                    _symbols.Add(symbol2);
                }

                foreach (var pi in type.GetProperties())
                {
                    var symbol3 = new MetadataPropertySymbol(this,
                        pi, null, symbol!, symbol, ns,
                        []);

                    _symbols.Add(symbol3);
                }

                _symbols.Add(symbol);
            }
        }
    }

    private INamespaceSymbol? GetNamespaceSymbol(string? ns)
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
            currentNamespace = currentNamespace.GetMembers().FirstOrDefault(n => n.Name == part) as NamespaceSymbol;

            if (currentNamespace == null)
            {
                return null; // Namespace not found
            }
        }

        return currentNamespace;
    }

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
            currentNamespace = currentNamespace.GetMembers().FirstOrDefault(n => n.Name == part) as NamespaceSymbol;

            if (currentNamespace == null)
            {
                currentNamespace = new NamespaceSymbol(part, parent, null, parent, [], []);
                _symbols.Add(currentNamespace);
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

            SyntaxReference[] references = [new SyntaxReference(syntaxTree, namespaceDeclarationSyntax.Span)];

            ITypeSymbol typeSymbol = null!;

            var symbol = new NamespaceSymbol(
                namespaceDeclarationSyntax.Name.ToString(), declaringSymbol, null!, (INamespaceSymbol?)declaringSymbol,
                locations, references);

            _symbols.Add(symbol);

            foreach (var memberDeclaration2 in namespaceDeclarationSyntax.Members)
            {
                AnalyzeMemberDeclaration(syntaxTree, symbol, memberDeclaration2);
            }
        }
        else if (memberDeclaration is MethodDeclarationSyntax methodDeclaration)
        {
            Location[] locations = [syntaxTree.GetLocation(methodDeclaration.Span)];

            SyntaxReference[] references = [new SyntaxReference(syntaxTree, methodDeclaration.Span)];

            ITypeSymbol typeSymbol = null!;

            var symbol = new SourceMethodSymbol(
                methodDeclaration.Name.ToString(), typeSymbol, null!, null, null,
                locations, references);

            _symbols.Add(symbol);
        }
    }

    public ImmutableArray<Diagnostic> GetDiagnostics(CancellationToken cancellationToken = default)
    {
        List<Diagnostic> diagnostics = new List<Diagnostic>();

        foreach (var item in SyntaxTrees)
        {
            diagnostics.AddRange(item.GetDiagnostics(cancellationToken));

            var model = GetSemanticModel(item);
            diagnostics.AddRange(model.GetDiagnostics(cancellationToken));
        }

        return diagnostics.OrderBy(x => x.Location).ToImmutableArray();
    }

    public Conversion ClassifyConversion(ITypeSymbol source, ITypeSymbol destination)
    {
        if (SymbolEqualityComparer.Default.Equals(source, destination))
        {
            // Identity conversion
            return new Conversion(isImplicit: true, isIdentity: true);
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

    public INamedTypeSymbol? GetTypeByMetadataName(string fullyQualifiedMetadataName)
    {
        if (fullyQualifiedMetadataName is null)
            return null;

        // Split the namespace into parts
        var nameParts = fullyQualifiedMetadataName.Split('.');

        // Start with the global namespace
        INamespaceOrTypeSymbol currentPart = GlobalNamespace;

        // Traverse the namespace hierarchy
        foreach (var part in nameParts)
        {
            currentPart = currentPart
                .GetMembers()
                .FirstOrDefault(n => n.Name == part) as INamespaceOrTypeSymbol;

            if (currentPart == null)
            {
                return null; // Type not found
            }
        }

        return currentPart as INamedTypeSymbol;
    }

    public INamedTypeSymbol GetSpecialType(SpecialType specialType)
    {
        if (specialType == SpecialType.System_Void)
        {
            return GetTypeByMetadataName("System.Void");
        }

        return null;
    }

    Dictionary<TypeInfo, ITypeSymbol> typeSymbolMappings = new();

    public ITypeSymbol GetType(Type type)
    {
        var typeInfo = type.GetTypeInfo();

        if (!typeSymbolMappings.TryGetValue(typeInfo, out var symbol)) ;
        {
            var ns = GetOrCreateNamespaceSymbol(type.Namespace);

            symbol = new MetadataTypeSymbol(
                this,
                typeInfo, ns, null, ns,
                []);
        }
        return symbol;
    }
}

public enum SpecialType
{
    None = 0,

    // System namespace
    System_Object = 1,
    System_Enum = 2,
    System_MulticastDelegate = 3,
    System_Delegate = 4,
    System_ValueType = 5,
    System_Void = 6,
    System_Boolean = 7,
    System_Char = 8,
    System_SByte = 9,
    System_Byte = 10,
    System_Int16 = 11,
    System_UInt16 = 12,
    System_Int32 = 13,
    System_UInt32 = 14,
    System_Int64 = 15,
    System_UInt64 = 16,
    System_Decimal = 17,
    System_Single = 18,
    System_Double = 19,
    System_String = 20,
    System_IntPtr = 21,
    System_UIntPtr = 22,
    System_Array = 23,
    System_Collections_IEnumerable = 24,
    System_Collections_Generic_IEnumerable_T = 25,
    System_Collections_Generic_IList_T = 26,
    System_Collections_Generic_ICollection_T = 27,
    System_Collections_IEnumerator = 28,
    System_Collections_Generic_IEnumerator_T = 29,
    System_Nullable_T = 30,

    // Task and async
    System_DateTime = 31,
    System_Runtime_CompilerServices_IsVolatile = 32,
    System_IDisposable = 33,
    System_TypedReference = 34,
    System_ArgIterator = 35,
    System_RuntimeArgumentHandle = 36,
    System_RuntimeFieldHandle = 37,
    System_RuntimeMethodHandle = 38,
    System_RuntimeTypeHandle = 39,
    System_IAsyncResult = 40,
    System_AsyncCallback = 41,
    System_Runtime_CompilerServices_AsyncVoidMethodBuilder = 42,
    System_Runtime_CompilerServices_AsyncTaskMethodBuilder = 43,
    System_Runtime_CompilerServices_AsyncTaskMethodBuilder_T = 44,
    System_Runtime_CompilerServices_AsyncStateMachineAttribute = 45,
    System_Runtime_CompilerServices_IteratorStateMachineAttribute = 46,
    System_Threading_Tasks_Task = 47,
    System_Threading_Tasks_Task_T = 48,

    // Interop
    System_Runtime_InteropServices_WindowsRuntime_EventRegistrationToken = 49,
    System_Runtime_InteropServices_WindowsRuntime_EventRegistrationTokenTable_T = 50,

    // Tuple types
    System_ValueTuple_T1 = 51,
    System_ValueTuple_T2 = 52,
    System_ValueTuple_T3 = 53,
    System_ValueTuple_T4 = 54,
    System_ValueTuple_T5 = 55,
    System_ValueTuple_T6 = 56,
    System_ValueTuple_T7 = 57,
    System_ValueTuple_TRest = 58
}

public struct Conversion
{
    public bool IsImplicit { get; }
    public bool IsExplicit => !IsImplicit;
    public bool IsIdentity { get; }
    public bool IsReference { get; }
    public bool IsBoxing { get; }
    public bool IsUnboxing { get; }

    public Conversion(bool isImplicit, bool isIdentity = false, bool isReference = false, bool isBoxing = false, bool isUnboxing = false)
    {
        IsImplicit = isImplicit;
        IsIdentity = isIdentity;
        IsReference = isReference;
        IsBoxing = isBoxing;
        IsUnboxing = isUnboxing;
    }

    public static Conversion None => new Conversion(false);
}

public class EmitResult
{
    internal EmitResult(bool success, ImmutableArray<Diagnostic> diagnostics)
    {
        Success = success;
        Diagnostics = diagnostics;
    }

    public bool Success { get; }
    public ImmutableArray<Diagnostic> Diagnostics { get; }
}

public class CompilationOptions
{
    public CompilationOptions()
    {
        OutputKind = OutputKind.ConsoleApplication;
    }

    public CompilationOptions(OutputKind outputKind)
    {
        OutputKind = outputKind;
    }

    public OutputKind OutputKind { get; }
}

public enum OutputKind
{
    ConsoleApplication = 0,
    WindowsApplication = 1,
    DynamicallyLinkedLibrary = 2
}