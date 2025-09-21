
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
        var returnType = MethodSymbol.ReturnType.SpecialType == SpecialType.System_Unit
            ? Compilation.GetSpecialType(SpecialType.System_Void).GetClrType(TypeGenerator.CodeGen)
            : ResolveClrType(MethodSymbol.ReturnType);

        var parameterTypes = MethodSymbol.Parameters
            .Select(p =>
            {
                var clrType = ResolveClrType(p.Type);
                if (p.RefKind is RefKind.Ref or RefKind.Out or RefKind.In)
                    return clrType.MakeByRefType();
                return clrType;
            })
            .ToArray();

        MethodAttributes attributes = MethodAttributes.HideBySig | MethodAttributes.Public;

        if (MethodSymbol.MethodKind is MethodKind.PropertyGet or MethodKind.PropertySet)
            attributes |= MethodAttributes.SpecialName;

        var isInterfaceMethod = TypeGenerator.TypeSymbol is INamedTypeSymbol named && named.TypeKind == TypeKind.Interface;
        if (isInterfaceMethod)
        {
            attributes |= MethodAttributes.Abstract | MethodAttributes.Virtual | MethodAttributes.NewSlot;
        }
        else if (TypeGenerator.ImplementsInterfaceMethod(MethodSymbol))
        {
            attributes |= MethodAttributes.Virtual | MethodAttributes.Final | MethodAttributes.NewSlot;
        }

        if (MethodSymbol.IsStatic && !isInterfaceMethod)
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

        ParameterBuilder? returnParamBuilder = MethodBase is MethodBuilder methodBuilder
            ? methodBuilder.DefineParameter(0, ParameterAttributes.Retval, null)
            : ((ConstructorBuilder)MethodBase).DefineParameter(0, ParameterAttributes.Retval, null);

        if (MethodSymbol.ReturnType.IsUnion)
        {
            var type = MethodSymbol.ReturnType;
            CustomAttributeBuilder customAttributeBuilder = CreateUnionTypeAttribute(type);
            returnParamBuilder.SetCustomAttribute(customAttributeBuilder);
        }

        var nullableReturnAttr = TypeGenerator.CodeGen.CreateNullableAttribute(MethodSymbol.ReturnType);
        if (nullableReturnAttr is not null)
            returnParamBuilder.SetCustomAttribute(nullableReturnAttr);

        int i = 1;
        foreach (var parameterSymbol in MethodSymbol.Parameters)
        {
            ParameterAttributes attrs = ParameterAttributes.None;
            if (parameterSymbol.RefKind == RefKind.Out)
                attrs |= ParameterAttributes.Out;
            else if (parameterSymbol.RefKind == RefKind.In)
                attrs |= ParameterAttributes.In;

            ParameterBuilder parameterBuilder;
            if (MethodBase is MethodBuilder mb)
                parameterBuilder = mb.DefineParameter(i, attrs, parameterSymbol.Name);
            else
                parameterBuilder = ((ConstructorBuilder)MethodBase).DefineParameter(i, attrs, parameterSymbol.Name);

            if (parameterSymbol.Type.IsUnion)
            {
                var type = parameterSymbol.Type;
                CustomAttributeBuilder customAttributeBuilder = CreateUnionTypeAttribute(type);
                parameterBuilder.SetCustomAttribute(customAttributeBuilder);
            }

            var nullableAttr = TypeGenerator.CodeGen.CreateNullableAttribute(parameterSymbol.Type);
            if (nullableAttr is not null)
                parameterBuilder.SetCustomAttribute(nullableAttr);

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
        var types = (type as IUnionTypeSymbol).Types
            .Select(x => x is LiteralTypeSymbol lit ? lit.ConstantValue : (object)ResolveClrType(x))
            .ToArray();
        var constructor = TypeGenerator.CodeGen.TypeUnionAttributeType!.
            GetConstructor(new[] { typeof(object[]) });
        CustomAttributeBuilder customAttributeBuilder = new CustomAttributeBuilder(constructor!, [types]);
        return customAttributeBuilder;
    }

    public IEnumerable<ParameterBuilder> GetParameterBuilders() => _parameterBuilders.Values;

    public ParameterBuilder GetParameterBuilder(IParameterSymbol parameterSymbol) => _parameterBuilders[parameterSymbol];

    public void EmitBody()
    {
        var isInterfaceMethod = TypeGenerator.TypeSymbol is INamedTypeSymbol named && named.TypeKind == TypeKind.Interface;

        if (isInterfaceMethod)
            return;

        var bodyGenerator = new MethodBodyGenerator(this);
        bodyGenerator.Emit();
    }

    public Type ResolveClrType(ITypeSymbol typeSymbol)
    {
        return typeSymbol.GetClrType(TypeGenerator.CodeGen);
    }

    public override string ToString() => this.MethodSymbol.ToDisplayString();
}
