using System.Collections.Immutable;
using System.Reflection;

using Raven.CodeAnalysis.CodeGen;
using Raven.CodeAnalysis.Symbols;
using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis;

public class Compilation
{
    private readonly string _name;
    private SyntaxTree[] _syntaxTrees;
    private MetadataReference[] _references;
    private List<ISymbol> _symbols = new List<ISymbol>();
    
    private Compilation(string name, SyntaxTree[] syntaxTrees, MetadataReference[] references, CompilationOptions? options = null)
    {
        _name = name;
        _syntaxTrees = syntaxTrees;
        _references = references;
        Options = options ?? new CompilationOptions();
    }
    
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
        return new Compilation(_name, syntaxTrees, _references, Options);
    }

    public Compilation AddReferences(MetadataReference[] references)
    {
        // TODO: Create new compilation
        return new Compilation(_name, _syntaxTrees, references, Options);
    }

    public SemanticModel GetSemanticModel(SyntaxTree syntaxTree)
    {
        if (!_syntaxTrees.Contains((syntaxTree)))
        {
            throw new ArgumentNullException(nameof(syntaxTree), "Syntax tree is not part of compilation");
        }
        return new SemanticModel(this, _symbols, syntaxTree, new DiagnosticBag(syntaxTree.GetDiagnostics()));
    }

    public MetadataReference ToMetadataReference() => new CompilationReference(this);

    public void Emit(Stream peStream, Stream? pdbStream = null)
    {
        new CodeGenerator().Generate(this, peStream, pdbStream);
    }
    
    public Compilation ProcessSymbolsTemp()
    {
        foreach (var syntaxTree in SyntaxTrees)
        {
            var root = syntaxTree.GetRoot();

            Location[] locations = [syntaxTree.GetLocation(root.Span)];

            SyntaxReference[] references = [new SyntaxReference(syntaxTree, root.Span)];

            var namespaceSymbol = new NamespaceSymbol(
                "", null!, null, null,
                locations, references);

            var globalStatements = root.Members.OfType<GlobalStatementSyntax>();
            if (globalStatements.Any())
            {
                ITypeSymbol typeSymbol = null!;
                
                var symbol2 = new SourceTypeSymbol(
                    "Program", namespaceSymbol!, null, namespaceSymbol,
                    [], []);

                _symbols.Add(symbol2);

                var symbol = new SourceMethodSymbol(
                    "Main", typeSymbol, symbol2!, symbol2, namespaceSymbol,
                    [syntaxTree.GetLocation(root.Span)], [new SyntaxReference(syntaxTree, root.Span)]);

                _symbols.Add(symbol);
            }
            else
            {
                foreach (var memberDeclaration in root.Members)
                {
                    AnalyzeMemberDeclaration(syntaxTree, namespaceSymbol, memberDeclaration);
                }
            }

            _symbols.Add(namespaceSymbol);   
        }

        return this;
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