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
            else if (named.IsSealed)
            {
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
                    {
                        var interfaceType = ResolveClrType(iface);
                        TypeBuilder.AddInterfaceImplementation(interfaceType);
                        CodeGen.RegisterForwardedType(interfaceType);
                    }
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
                {
                    var interfaceType = ResolveClrType(iface);
                    TypeBuilder.AddInterfaceImplementation(interfaceType);
                    CodeGen.RegisterForwardedType(interfaceType);
                }
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
                        if (_methodGenerators.ContainsKey(methodSymbol))
                            break;

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
                        MethodGenerator? getGen = null;
                        MethodGenerator? setGen = null;

                        if (propertySymbol.GetMethod is IMethodSymbol getMethod)
                        {
                            getGen = new MethodGenerator(this, getMethod);
                            _methodGenerators[getMethod] = getGen;
                            getGen.DefineMethodBuilder();
                            CodeGen.AddMemberBuilder((SourceSymbol)getMethod, getGen.MethodBase);
                        }

                        if (propertySymbol.SetMethod is IMethodSymbol setMethod)
                        {
                            setGen = new MethodGenerator(this, setMethod);
                            _methodGenerators[setMethod] = setGen;
                            setGen.DefineMethodBuilder();
                            CodeGen.AddMemberBuilder((SourceSymbol)setMethod, setGen.MethodBase);
                        }

                        var propertyType = ResolveClrType(propertySymbol.Type);
                        var paramTypes = propertySymbol.GetMethod?.Parameters.Select(p => ResolveClrType(p.Type)).ToArray() ?? Type.EmptyTypes;
                        var propBuilder = TypeBuilder.DefineProperty(propertySymbol.Name, PropertyAttributes.None, propertyType, paramTypes);

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

    private static ImmutableArray<INamedTypeSymbol> GetAllInterfaces(INamedTypeSymbol named)
    {
        if (!named.AllInterfaces.IsDefaultOrEmpty)
            return named.AllInterfaces;

        return named.Interfaces;
    }

    private bool TryFindImplementation(IMethodSymbol interfaceMethod, out IMethodSymbol implementation)
    {
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
        else if (interfaceMethod.ContainingType is INamedTypeSymbol containingType)
        {
            var interfaceClrType = ResolveClrType(containingType);

            var runtimeName = containingType.ToDisplayString(SymbolDisplayFormat.FullyQualifiedFormat);
            if (runtimeName.StartsWith("global::", StringComparison.Ordinal))
                runtimeName = runtimeName["global::".Length..];

            var runtimeType = Type.GetType(runtimeName, throwOnError: false, ignoreCase: false) ?? interfaceClrType;
            var parameterTypes = interfaceMethod.Parameters
                .Select(GetParameterClrType)
                .ToArray();

            var candidate = runtimeType.GetMethod(
                interfaceMethod.Name,
                BindingFlags.Instance | BindingFlags.Public | BindingFlags.NonPublic,
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

    private Type GetParameterClrType(IParameterSymbol parameter)
    {
        var parameterType = ResolveClrType(parameter.Type);
        return parameter.RefKind is RefKind.Ref or RefKind.Out or RefKind.In
            ? parameterType.MakeByRefType()
            : parameterType;
    }

    private static bool SignaturesMatch(IMethodSymbol candidate, IMethodSymbol interfaceMethod)
    {
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
}
