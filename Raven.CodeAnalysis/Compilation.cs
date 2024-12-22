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

    private Compilation(string name, SyntaxTree[] syntaxTrees, MetadataReference[] references, CompilationOptions? options = null)
    {
        Name = name;
        _syntaxTrees = syntaxTrees;
        _references = references;
        Options = options ?? new CompilationOptions();
    }

    public string Name { get; }

    public CompilationOptions Options { get; }

    public SyntaxTree[] SyntaxTrees => _syntaxTrees;

    public INamespaceSymbol GlobalNamespace { get; private set; }

    public static Compilation Create(string name, SyntaxTree[] syntaxTrees, CompilationOptions? options = null)
    {
        return new Compilation(name, syntaxTrees, [], options);
    }

    public static Compilation Create(string name, CompilationOptions? options = null)
    {
        return new Compilation(name, [], [], options);
    }

    public static Compilation Create(string name, SyntaxTree[] syntaxTrees, MetadataReference[] references, CompilationOptions? options = null)
    {
        return new Compilation(name, syntaxTrees, references, options);
    }

    public Compilation AddSyntaxTrees(params SyntaxTree[] syntaxTrees)
    {
        // TODO: Create new compilation
        return new Compilation(Name, syntaxTrees, _references, Options);
    }

    public Compilation AddReferences(MetadataReference[] references)
    {
        // TODO: Create new compilation
        return new Compilation(Name, _syntaxTrees, references, Options);
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

                var symbol = new MetadataTypeSymbol(
                    type.GetTypeInfo(), ns, null, ns,
                    []);

                foreach (var mi in type.GetMethods())
                {
                    var symbol2 = new MetadataMethodSymbol(
                        mi, null, symbol!, symbol, ns,
                        []);

                    _symbols.Add(symbol2);
                }

                foreach (var pi in type.GetProperties())
                {
                    var symbol3 = new MetadataPropertySymbol(
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