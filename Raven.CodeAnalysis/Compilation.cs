

using System.Collections.Immutable;
using System.Reflection;

using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis;

public class Compilation
{
    private readonly string _name;
    private SyntaxTree[] _syntaxTrees;
    private MetadataReference[] _references;

    private Compilation(string name)
    {
        _name = name;
    }

    public SyntaxTree[] SyntaxTrees => _syntaxTrees;

    public static Compilation Create(string name, SyntaxTree[] syntaxTrees)
    {
        return new Compilation(name)
            .AddSyntaxTrees(syntaxTrees);
    }
    
    public static Compilation Create(string name)
    {
        return new Compilation(name);
    }
    
    public static Compilation Create(string name, SyntaxTree[] syntaxTrees, MetadataReference[] references)
    {
        return Create(name, syntaxTrees)
            .AddReferences(references);
    }

    public Compilation AddSyntaxTrees(params SyntaxTree[] syntaxTrees)
    {
        _syntaxTrees = syntaxTrees;
        return this;
    }
    
    public Compilation AddReferences(MetadataReference[] references)
    {
        _references = references;
        return this;
    }

    public SemanticModel GetSemanticModel(SyntaxTree syntaxTree)
    {
        if (!_syntaxTrees.Contains((syntaxTree)))
        {
            throw new ArgumentNullException(nameof(syntaxTree), "Syntax tree is not part of compilation");
        }
        return new SemanticModel(this, syntaxTree, new DiagnosticBag(syntaxTree.GetDiagnostics()));
    }

    public MetadataReference ToMetadataReference() => new CompilationReference(this);
}

public class CompilationOptions(OutputKind outputKind) { }

public enum OutputKind
{
    ConsoleApplication = 0,
    WindowsApplication = 1,
    DynamicallyLinkedLibrary = 2
}