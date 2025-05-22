
using System.Reflection;
using System.Reflection.Emit;

namespace Raven.CodeAnalysis.CodeGen;

internal class MethodGenerator
{
    private readonly IDictionary<ISymbol, ParameterBuilder> _parameterBuilders = new Dictionary<ISymbol, ParameterBuilder>(SymbolEqualityComparer.Default);
    private Compilation _compilation;

    public MethodGenerator(TypeGenerator typeGenerator, IMethodSymbol methodSymbol)
    {
        TypeGenerator = typeGenerator;
        MethodSymbol = methodSymbol;
    }

    public Compilation Compilation => _compilation ??= TypeGenerator.Compilation;
    public TypeGenerator TypeGenerator { get; }
    public IMethodSymbol MethodSymbol { get; }
    public MethodBuilder MethodBuilder { get; private set; }
    public bool IsEntryPointCandidate { get; private set; }

    internal void DefineMethodBuilder()
    {
        var returnType = MethodSymbol.ReturnType.GetClrType(Compilation);

        var parameterTypes = MethodSymbol.Parameters.Select(p => p.Type.GetClrType(Compilation));

        MethodBuilder = TypeGenerator.TypeBuilder!
            .DefineMethod(MethodSymbol.Name,
                MethodAttributes.HideBySig | MethodAttributes.Public | MethodAttributes.Static, CallingConventions.Standard,
                returnType,
                parameterTypes.ToArray());

        int i = 1;
        foreach (var parameterSymbol in MethodSymbol.Parameters)
        {
            var methodBuilder = MethodBuilder.DefineParameter(i, ParameterAttributes.None, parameterSymbol.Name);
            _parameterBuilders[parameterSymbol] = methodBuilder;
            i++;
        }

        if (MethodSymbol.Name == "Main")
        {
            IsEntryPointCandidate = true;
        }
    }

    public IEnumerable<ParameterBuilder> GetParameterBuilders() => _parameterBuilders.Values;

    public ParameterBuilder GetParameterBuilder(IParameterSymbol parameterSymbol) => _parameterBuilders[parameterSymbol];

    public void GenerateBody()
    {
        var bodyGenerator = new MethodBodyGenerator(this);
        bodyGenerator.Generate();
    }
}