
using System.Reflection;
using System.Reflection.Emit;

namespace Raven.CodeAnalysis.CodeGen;

internal class TypeGenerator
{
    readonly Dictionary<IMethodSymbol, MethodGenerator> _methodGenerators = new Dictionary<IMethodSymbol, MethodGenerator>(SymbolEqualityComparer.Default);
    private Compilation _compilation;

    public CodeGenerator CodeGen { get; }
    public Compilation Compilation => _compilation ??= CodeGen.Compilation;
    public ITypeSymbol TypeSymbol { get; }
    public TypeBuilder? TypeBuilder { get; private set; }

    public IEnumerable<MethodGenerator> MethodGenerators => _methodGenerators.Values;

    public TypeGenerator(CodeGenerator codeGen, ITypeSymbol typeSymbol)
    {
        CodeGen = codeGen;
        TypeSymbol = typeSymbol;
    }

    public void DefineTypeBuilder()
    {
        var syntaxReference = TypeSymbol.DeclaringSyntaxReferences.FirstOrDefault();
        if (syntaxReference is not null)
        {
            TypeBuilder = CodeGen.ModuleBuilder.DefineType(TypeSymbol.Name, TypeAttributes.Public | TypeAttributes.Class);
        }

        DefineMemberBuilders();
    }

    public void DefineMemberBuilders()
    {
        foreach (var memberSymbol in TypeSymbol.GetMembers())
        {
            if (memberSymbol is IMethodSymbol methodSymbol)
            {
                var methodGenerator = new MethodGenerator(this, methodSymbol);
                _methodGenerators[methodSymbol] = methodGenerator;
                methodGenerator.DefineMethodBuilder();
            }
        }
    }

    public void GenerateMemberILBodies()
    {
        foreach (var methodGenerators in _methodGenerators.Values.ToList())
        {
            methodGenerators.GenerateBody();
        }
    }

    public Type CreateType()
    {
        return TypeBuilder!.CreateType();
    }

    public void Add(IMethodSymbol methodSymbol, MethodGenerator methodGenerator)
    {
        _methodGenerators[methodSymbol] = methodGenerator;
    }
}
