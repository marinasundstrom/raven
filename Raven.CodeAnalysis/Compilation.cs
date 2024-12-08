

using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis;

public class Compilation
{
    private string _name;
    private SyntaxTree[] _syntaxTress;

    public Compilation(string name)
    {
        _name = name;
    }

    public static Compilation Create(string name, SyntaxTree[] syntaxTrees /* references */)
    {
        return new Compilation(name)
            .AddSyntaxTrees(syntaxTrees);
    }

    public static Compilation Create(string name)
    {
        return new Compilation(name);
    }

    public Compilation AddSyntaxTrees(params SyntaxTree[] syntaxTrees)
    {
        _syntaxTress = syntaxTrees;
        return this;
    }

    public SemanticModel GetSemanticModel(SyntaxTree syntaxTree)
    {
        return new SemanticModel();
    }

    /*
     var compilation = CSharpCompilation.Create(
                "ExampleCompilation",
                syntaxTrees: new[] { syntaxTree },
                references: new[]
                {
                    MetadataReference.CreateFromFile(typeof(object).Assembly.Location),
                    MetadataReference.CreateFromFile(typeof(Console).Assembly.Location)
                }
            );
    */
}

public class SemanticModel
{
}