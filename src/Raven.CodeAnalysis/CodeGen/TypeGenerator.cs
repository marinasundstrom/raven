using System;
using System.Collections.Immutable;
using System.Linq;
using System.Reflection;
using System.Reflection.Emit;

using Raven.CodeAnalysis.Symbols;

namespace Raven.CodeAnalysis.CodeGen;

internal class TypeGenerator
{
    readonly Dictionary<IMethodSymbol, MethodGenerator> _methodGenerators = new Dictionary<IMethodSymbol, MethodGenerator>(SymbolEqualityComparer.Default);
    readonly Dictionary<IFieldSymbol, FieldBuilder> _fieldBuilders = new Dictionary<IFieldSymbol, FieldBuilder>(SymbolEqualityComparer.Default);

    private Compilation _compilation;

    public CodeGenerator CodeGen { get; }
    public Compilation Compilation => _compilation ??= CodeGen.Compilation;
    public ITypeSymbol TypeSymbol { get; }
    public TypeBuilder? TypeBuilder { get; private set; }

    public IEnumerable<MethodGenerator> MethodGenerators => _methodGenerators.Values;

    public Type? Type { get; private set; }

    public TypeGenerator(CodeGenerator codeGen, ITypeSymbol typeSymbol)
    {
        CodeGen = codeGen;
        TypeSymbol = typeSymbol;
    }

    public void DefineTypeBuilder()
    {
        TypeAttributes typeAttributes = TypeAttributes.Public;

        if (TypeSymbol is INamedTypeSymbol named)
        {
            if (named.TypeKind == TypeKind.Interface)
            {
                typeAttributes |= TypeAttributes.Interface | TypeAttributes.Abstract;
            }
            else
            {
                if (named.IsAbstract)
                    typeAttributes |= TypeAttributes.Abstract;

                if (named.IsSealed)
                    typeAttributes |= TypeAttributes.Sealed;
            }
        }

        if (TypeSymbol.BaseType.Name == "Enum")
        {
            TypeBuilder = CodeGen.ModuleBuilder.DefineType(
                TypeSymbol.MetadataName,
                TypeAttributes.Public | TypeAttributes.Sealed | TypeAttributes.Serializable,
                ResolveClrType(TypeSymbol.BaseType) // bör vara System.Enum
            );

            // Lägg till value__ direkt här
            TypeBuilder.DefineField(
                "value__",
                Compilation.GetTypeByMetadataName("System.Int32").GetClrType(Compilation),
                FieldAttributes.Public | FieldAttributes.SpecialName | FieldAttributes.RTSpecialName
            );

            return;
        }

        var syntaxReference = TypeSymbol.DeclaringSyntaxReferences.FirstOrDefault();
        if (syntaxReference is not null)
        {
            if (TypeSymbol is INamedTypeSymbol nt && nt.TypeKind == TypeKind.Interface)
            {
                TypeBuilder = CodeGen.ModuleBuilder.DefineType(
                    TypeSymbol.MetadataName,
                    typeAttributes);

                if (!nt.Interfaces.IsDefaultOrEmpty)
                {
                    foreach (var iface in nt.Interfaces)
                        TypeBuilder.AddInterfaceImplementation(ResolveClrType(iface));
                }

                return;
            }

            TypeBuilder = CodeGen.ModuleBuilder.DefineType(
                TypeSymbol.MetadataName,
                typeAttributes,
                ResolveClrType(TypeSymbol.BaseType));

            if (TypeSymbol is INamedTypeSymbol nt2 && !nt2.Interfaces.IsDefaultOrEmpty)
            {
                foreach (var iface in nt2.Interfaces)
                    TypeBuilder.AddInterfaceImplementation(ResolveClrType(iface));
            }
        }
    }

    public void DefineMemberBuilders()
    {
        if (TypeSymbol.BaseType.ContainingNamespace.Name == "System"
            && TypeSymbol.BaseType.Name == "Enum")
        {
            foreach (var fieldSymbol in TypeSymbol.GetMembers().OfType<IFieldSymbol>())
            {
                var fieldBuilder = TypeBuilder.DefineField(
                    fieldSymbol.Name,
                    TypeBuilder,
                    FieldAttributes.Public | FieldAttributes.Static | FieldAttributes.Literal
                );

                fieldBuilder.SetConstant(fieldSymbol.GetConstantValue());

                CodeGen.AddMemberBuilder((SourceSymbol)fieldSymbol, fieldBuilder);
            }

            return;
        }

        foreach (var memberSymbol in TypeSymbol.GetMembers())
        {
            switch (memberSymbol)
            {
                case IMethodSymbol methodSymbol when methodSymbol.MethodKind is not (MethodKind.PropertyGet or MethodKind.PropertySet):
                    {
                        var methodGenerator = new MethodGenerator(this, methodSymbol);
                        _methodGenerators[methodSymbol] = methodGenerator;
                        methodGenerator.DefineMethodBuilder();

                        CodeGen.AddMemberBuilder((SourceSymbol)methodSymbol, methodGenerator.MethodBase);
                        break;
                    }
                case IFieldSymbol fieldSymbol:
                    {
                        var type = fieldSymbol.Type.Equals(TypeSymbol, SymbolEqualityComparer.Default) ? TypeBuilder : ResolveClrType(fieldSymbol.Type);

                        FieldAttributes attr = FieldAttributes.Public;

                        if (fieldSymbol.IsLiteral)
                        {
                            attr |= FieldAttributes.Literal;
                        }

                        if (fieldSymbol.IsStatic)
                        {
                            attr |= FieldAttributes.Static;
                        }

                        var fieldBuilder = TypeBuilder.DefineField(fieldSymbol.Name, type, attr);
                        if (fieldSymbol.IsLiteral)
                            fieldBuilder.SetConstant(fieldSymbol.GetConstantValue());
                        var nullableAttr = CodeGen.CreateNullableAttribute(fieldSymbol.Type);
                        if (nullableAttr is not null)
                            fieldBuilder.SetCustomAttribute(nullableAttr);
                        _fieldBuilders[fieldSymbol] = fieldBuilder;

                        CodeGen.AddMemberBuilder((SourceSymbol)fieldSymbol, fieldBuilder);
                        break;
                    }
                case IPropertySymbol propertySymbol:
                    {
                        var getterSymbol = propertySymbol.GetMethod as IMethodSymbol;
                        var setterSymbol = propertySymbol.SetMethod as IMethodSymbol;

                        MethodGenerator? getGen = null;
                        MethodGenerator? setGen = null;

                        if (getterSymbol is not null)
                        {
                            getGen = new MethodGenerator(this, getterSymbol);
                            _methodGenerators[getterSymbol] = getGen;
                            getGen.DefineMethodBuilder();
                            CodeGen.AddMemberBuilder((SourceSymbol)getterSymbol, getGen.MethodBase);
                        }

                        if (setterSymbol is not null)
                        {
                            setGen = new MethodGenerator(this, setterSymbol);
                            _methodGenerators[setterSymbol] = setGen;
                            setGen.DefineMethodBuilder();
                            CodeGen.AddMemberBuilder((SourceSymbol)setterSymbol, setGen.MethodBase);
                        }

                        var propertyType = ResolveClrType(propertySymbol.Type);

                        Type[]? paramTypes = null;
                        if (getterSymbol is not null)
                        {
                            var getterParams = getterSymbol.Parameters
                                .Select(p => ResolveClrType(p.Type))
                                .ToArray();

                            if (getterParams.Length > 0)
                                paramTypes = getterParams;
                        }
                        else if (setterSymbol is not null)
                        {
                            var setterParams = setterSymbol.Parameters;
                            var paramCount = setterSymbol.MethodKind == MethodKind.PropertySet
                                ? setterParams.Length - 1
                                : setterParams.Length;

                            if (paramCount > 0)
                            {
                                var builder = new Type[paramCount];
                                for (var i = 0; i < paramCount; i++)
                                    builder[i] = ResolveClrType(setterParams[i].Type);

                                paramTypes = builder;
                            }
                        }

                        var propBuilder = TypeBuilder.DefineProperty(propertySymbol.MetadataName, PropertyAttributes.None, propertyType, paramTypes);

                        if (getGen != null)
                            propBuilder.SetGetMethod((MethodBuilder)getGen.MethodBase);
                        if (setGen != null)
                            propBuilder.SetSetMethod((MethodBuilder)setGen.MethodBase);

                        var nullableAttr = CodeGen.CreateNullableAttribute(propertySymbol.Type);
                        if (nullableAttr is not null)
                            propBuilder.SetCustomAttribute(nullableAttr);

                        CodeGen.AddMemberBuilder((SourceSymbol)propertySymbol, propBuilder);
                        break;
                    }
            }
        }

    }

    public void EmitMemberILBodies()
    {
        foreach (var methodGenerator in _methodGenerators.Values.ToList())
        {
            methodGenerator.EmitBody();
        }
    }

    public Type CreateType() => TypeBuilder!.CreateType();

    public bool HasMethodGenerator(IMethodSymbol methodSymbol)
    {
        return _methodGenerators.ContainsKey(methodSymbol);
    }

    public void Add(IMethodSymbol methodSymbol, MethodGenerator methodGenerator)
    {
        _methodGenerators[methodSymbol] = methodGenerator;
    }

    public Type ResolveClrType(ITypeSymbol typeSymbol)
    {
        return typeSymbol.GetClrType(CodeGen);
    }

    internal bool ImplementsInterfaceMethod(IMethodSymbol methodSymbol)
    {
        if (TypeSymbol is not INamedTypeSymbol named || named.TypeKind == TypeKind.Interface)
            return false;

        if (methodSymbol.IsStatic)
            return false;

        var interfaces = GetAllInterfaces(named);
        if (interfaces.IsDefaultOrEmpty)
            return false;

        if (!methodSymbol.ExplicitInterfaceImplementations.IsDefaultOrEmpty)
        {
            foreach (var implemented in methodSymbol.ExplicitInterfaceImplementations)
            {
                if (implemented.ContainingType is INamedTypeSymbol containingInterface &&
                    interfaces.Contains(containingInterface, SymbolEqualityComparer.Default))
                    return true;
            }

            return false;
        }

        foreach (var interfaceType in interfaces)
        {
            foreach (var interfaceMethod in interfaceType.GetMembers().OfType<IMethodSymbol>())
            {
                if (SignaturesMatch(methodSymbol, interfaceMethod))
                    return true;
            }
        }

        return false;
    }

    internal void CompleteInterfaceImplementations()
    {
        if (TypeSymbol is INamedTypeSymbol named && named.TypeKind != TypeKind.Interface)
        {
            ImplementInterfaceMembers(named);
            ImplementVirtualOverrides(named);
        }
    }

    private void ImplementInterfaceMembers(INamedTypeSymbol named)
    {
        if (TypeBuilder is null)
            return;

        var interfaces = GetAllInterfaces(named);
        if (interfaces.IsDefaultOrEmpty)
            return;

        foreach (var interfaceType in interfaces)
        {
            foreach (var interfaceMethod in interfaceType.GetMembers().OfType<IMethodSymbol>())
            {
                if (interfaceMethod.IsStatic)
                    continue;

                if (!TryFindImplementation(interfaceMethod, out var implementation))
                    continue;

                if (!_methodGenerators.TryGetValue(implementation, out var implementationGenerator))
                    continue;

                if (implementationGenerator.MethodBase is not MethodBuilder methodBuilder)
                    continue;

                if (!TryGetInterfaceMethodInfo(interfaceMethod, out var interfaceMethodInfo))
                    continue;

                TypeBuilder.DefineMethodOverride(methodBuilder, interfaceMethodInfo);
            }
        }
    }

    private void ImplementVirtualOverrides(INamedTypeSymbol named)
    {
        if (TypeBuilder is null)
            return;

        foreach (var methodSymbol in named.GetMembers().OfType<IMethodSymbol>())
        {
            if (methodSymbol is not SourceMethodSymbol sourceMethod)
                continue;

            if (sourceMethod.OverriddenMethod is null)
                continue;

            if (!_methodGenerators.TryGetValue(sourceMethod, out var implementationGenerator))
                continue;

            if (implementationGenerator.MethodBase is not MethodBuilder methodBuilder)
                continue;

            if (!TryGetMethodInfo(sourceMethod.OverriddenMethod, out var baseMethodInfo))
                continue;

            TypeBuilder.DefineMethodOverride(methodBuilder, baseMethodInfo);
        }
    }

    private static ImmutableArray<INamedTypeSymbol> GetAllInterfaces(INamedTypeSymbol named)
    {
        if (!named.AllInterfaces.IsDefaultOrEmpty)
            return named.AllInterfaces;

        return named.Interfaces;
    }

    private bool TryFindImplementation(IMethodSymbol interfaceMethod, out IMethodSymbol implementation)
    {
        foreach (var candidate in TypeSymbol.GetMembers().OfType<IMethodSymbol>())
        {
            if (candidate.ExplicitInterfaceImplementations.IsDefaultOrEmpty)
                continue;

            if (candidate.ExplicitInterfaceImplementations.Contains(interfaceMethod, SymbolEqualityComparer.Default))
            {
                implementation = candidate;
                return true;
            }
        }

        foreach (var candidate in TypeSymbol.GetMembers(interfaceMethod.Name).OfType<IMethodSymbol>())
        {
            if (SignaturesMatch(candidate, interfaceMethod))
            {
                implementation = candidate;
                return true;
            }
        }

        implementation = null!;
        return false;
    }

    private bool TryGetInterfaceMethodInfo(IMethodSymbol interfaceMethod, out MethodInfo methodInfo)
    {
        if (interfaceMethod is SourceSymbol sourceSymbol)
        {
            if (CodeGen.GetMemberBuilder(sourceSymbol) is MethodInfo interfaceBuilder)
            {
                methodInfo = interfaceBuilder;
                return true;
            }
        }
        else if (interfaceMethod.ContainingType is not null)
        {
            var interfaceClrType = ResolveClrType(interfaceMethod.ContainingType);
            var parameterTypes = interfaceMethod.Parameters
                .Select(GetParameterClrType)
                .ToArray();

            var bindingFlags = BindingFlags.Public | BindingFlags.NonPublic;
            bindingFlags |= interfaceMethod.IsStatic ? BindingFlags.Static : BindingFlags.Instance;

            var candidate = interfaceClrType.GetMethod(
                interfaceMethod.Name,
                bindingFlags,
                binder: null,
                types: parameterTypes,
                modifiers: null);

            if (candidate is not null)
            {
                methodInfo = candidate;
                return true;
            }
        }

        methodInfo = null!;
        return false;
    }

    private bool TryGetMethodInfo(IMethodSymbol methodSymbol, out MethodInfo methodInfo)
    {
        switch (methodSymbol)
        {
            case SourceMethodSymbol sourceMethod:
                {
                    if (CodeGen.GetMemberBuilder(sourceMethod) is MethodInfo builder)
                    {
                        methodInfo = builder;
                        return true;
                    }

                    break;
                }
            case PEMethodSymbol peMethod:
                methodInfo = peMethod.GetMethodInfo();
                return true;
            case SubstitutedMethodSymbol substitutedMethod:
                methodInfo = substitutedMethod.GetMethodInfo(CodeGen);
                return true;
        }

        methodInfo = null!;
        return false;
    }

    private Type GetParameterClrType(IParameterSymbol parameter)
    {
        var parameterType = ResolveClrType(parameter.Type);
        return parameter.RefKind is RefKind.Ref or RefKind.Out or RefKind.In
            ? parameterType.MakeByRefType()
            : parameterType;
    }

    private static bool SignaturesMatch(IMethodSymbol candidate, IMethodSymbol interfaceMethod)
    {
        if (!string.Equals(candidate.Name, interfaceMethod.Name, StringComparison.Ordinal))
            return false;

        if (!ReturnTypesMatch(candidate.ReturnType, interfaceMethod.ReturnType))
            return false;

        if (candidate.Parameters.Length != interfaceMethod.Parameters.Length)
            return false;

        for (var i = 0; i < candidate.Parameters.Length; i++)
        {
            var candidateParameter = candidate.Parameters[i];
            var interfaceParameter = interfaceMethod.Parameters[i];

            if (candidateParameter.RefKind != interfaceParameter.RefKind)
                return false;

            if (!SymbolEqualityComparer.Default.Equals(candidateParameter.Type, interfaceParameter.Type))
                return false;
        }

        return true;
    }

    private static bool ReturnTypesMatch(ITypeSymbol candidateReturnType, ITypeSymbol interfaceReturnType)
    {
        if (SymbolEqualityComparer.Default.Equals(candidateReturnType, interfaceReturnType))
            return true;

        if (candidateReturnType.SpecialType == SpecialType.System_Unit
            && interfaceReturnType.SpecialType == SpecialType.System_Void)
            return true;

        if (candidateReturnType.SpecialType == SpecialType.System_Void
            && interfaceReturnType.SpecialType == SpecialType.System_Unit)
            return true;

        return false;
    }
}
