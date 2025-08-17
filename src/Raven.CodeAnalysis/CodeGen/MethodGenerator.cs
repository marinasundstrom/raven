
using System.Reflection;
using System.Reflection.Emit;

using Raven.CodeAnalysis.Symbols;

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
    public MethodBase MethodBase { get; private set; }
    public bool IsEntryPointCandidate { get; private set; }

    internal void DefineMethodBuilder()
    {
        var returnType = ResolveClrType(MethodSymbol.ReturnType);

        var parameterTypes = MethodSymbol.Parameters.Select(p => ResolveClrType(p.Type)).ToArray();

        MethodAttributes attributes = MethodAttributes.HideBySig | MethodAttributes.Public;
        if (MethodSymbol.IsStatic)
            attributes |= MethodAttributes.Static;

        if (MethodSymbol.IsConstructor && !MethodSymbol.IsNamedConstructor)
        {
            MethodBase = TypeGenerator.TypeBuilder!
                .DefineConstructor(attributes, CallingConventions.Standard, parameterTypes);
        }
        else
        {
            MethodBase = TypeGenerator.TypeBuilder!
                .DefineMethod(MethodSymbol.Name,
                    attributes, CallingConventions.Standard,
                    returnType,
                    parameterTypes);
        }

        int i = 1;
        foreach (var parameterSymbol in MethodSymbol.Parameters)
        {
            ParameterBuilder methodBuilder;
            if (MethodBase is MethodBuilder mb)
                methodBuilder = mb.DefineParameter(i, ParameterAttributes.None, parameterSymbol.Name);
            else
                methodBuilder = ((ConstructorBuilder)MethodBase).DefineParameter(i, ParameterAttributes.None, parameterSymbol.Name);

            if (parameterSymbol.Type.IsUnion)
            {
                var types = (parameterSymbol.Type as IUnionTypeSymbol).Types.Select(x => ResolveClrType(x)).ToArray();
                var construtor = TypeGenerator.CodeGen.TypeUnionAttributeType.GetConstructor(new[] { typeof(Type[]) });
                CustomAttributeBuilder customAttributeBuilder = new CustomAttributeBuilder(construtor, [types]);
                methodBuilder.SetCustomAttribute(customAttributeBuilder);
            }

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

    public void EmitBody()
    {
        var bodyGenerator = new MethodBodyGenerator(this);
        bodyGenerator.Emit();
    }

    public Type ResolveClrType(ITypeSymbol typeSymbol)
    {
        return typeSymbol.GetClrType(TypeGenerator.CodeGen);
    }
}