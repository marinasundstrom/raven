

using System.Collections.Immutable;
using System.Reflection;

using Raven.CodeAnalysis.CodeGen;
using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis;

public class Compilation
{
    private readonly string _name;
    private SyntaxTree[] _syntaxTrees;
    private MetadataReference[] _references;

    public CompilationOptions Options { get; }

    private Compilation(string name, CompilationOptions? options = null)
    {
        _name = name;
        Options = options ?? new CompilationOptions();
    }

    public SyntaxTree[] SyntaxTrees => _syntaxTrees;

    public static Compilation Create(string name, SyntaxTree[] syntaxTrees, CompilationOptions? options = null)
    {
        return new Compilation(name, options)
            .AddSyntaxTrees(syntaxTrees);
    }

    public static Compilation Create(string name, CompilationOptions? options = null)
    {
        return new Compilation(name, options);
    }

    public static Compilation Create(string name, SyntaxTree[] syntaxTrees, MetadataReference[] references, CompilationOptions? options = null)
    {
        return Create(name, syntaxTrees, options)
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

    public void Emit(Stream peStream, Stream? pdbStream = null)
    {
        new CodeGenerator().Generate(this, peStream, pdbStream);
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