
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
            if (MethodSymbol.IsStatic)
                MethodBase = TypeGenerator.TypeBuilder!.DefineTypeInitializer();
            else
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

        if (MethodSymbol.ReturnType.IsUnion)
        {
            var type = MethodSymbol.ReturnType;

            if (MethodBase is MethodBuilder mb)
            {
                var returnParam = mb.DefineParameter(0, ParameterAttributes.Retval, null);

                CustomAttributeBuilder customAttributeBuilder = CreateUnionTypeAttribute(type);
                returnParam.SetCustomAttribute(customAttributeBuilder);
            }
        }

        int i = 1;
        foreach (var parameterSymbol in MethodSymbol.Parameters)
        {
            ParameterBuilder parameterBuilder;
            if (MethodBase is MethodBuilder mb)
                parameterBuilder = mb.DefineParameter(i, ParameterAttributes.None, parameterSymbol.Name);
            else
                parameterBuilder = ((ConstructorBuilder)MethodBase).DefineParameter(i, ParameterAttributes.None, parameterSymbol.Name);

            if (parameterSymbol.Type.IsUnion)
            {
                var type = parameterSymbol.Type;
                CustomAttributeBuilder customAttributeBuilder = CreateUnionTypeAttribute(type);
                parameterBuilder.SetCustomAttribute(customAttributeBuilder);
            }

            _parameterBuilders[parameterSymbol] = parameterBuilder;
            i++;
        }

        if (MethodSymbol.Name == "Main")
        {
            IsEntryPointCandidate = true;
        }
    }

    private CustomAttributeBuilder CreateUnionTypeAttribute(ITypeSymbol type)
    {
        var types = (type as IUnionTypeSymbol).Types.Select(x => ResolveClrType(x)).ToArray();
        var constructor = TypeGenerator.CodeGen.TypeUnionAttributeType!.
            GetConstructor(new[] { typeof(Type[]) });
        CustomAttributeBuilder customAttributeBuilder = new CustomAttributeBuilder(constructor!, [types]);
        return customAttributeBuilder;
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

    public override string ToString() => this.MethodSymbol.ToDisplayString();
}
