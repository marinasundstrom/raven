using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;
using System.Reflection;
using System.Reflection.Emit;

using Raven.CodeAnalysis.CodeGen.Metadata;
using Raven.CodeAnalysis.Symbols;

namespace Raven.CodeAnalysis.CodeGen;

internal class TypeGenerator
{
    readonly Dictionary<IMethodSymbol, MethodGenerator> _methodGenerators = new Dictionary<IMethodSymbol, MethodGenerator>(SymbolEqualityComparer.Default);
    readonly Dictionary<IFieldSymbol, FieldBuilder> _fieldBuilders = new Dictionary<IFieldSymbol, FieldBuilder>(SymbolEqualityComparer.Default);
    readonly Dictionary<ILambdaSymbol, LambdaClosure> _lambdaClosures = new Dictionary<ILambdaSymbol, LambdaClosure>(SymbolEqualityComparer.Default);

    private int _lambdaClosureOrdinal;

    private Compilation _compilation;

    public CodeGenerator CodeGen { get; }
    public Compilation Compilation => _compilation ??= CodeGen.Compilation;
    public ITypeSymbol TypeSymbol { get; }
    public TypeBuilder? TypeBuilder { get; private set; }

    public MetadataTypeDefinition MetadataType { get; }

    public IEnumerable<MethodGenerator> MethodGenerators => _methodGenerators.Values;

    public Type? Type { get; private set; }

    public TypeGenerator(CodeGenerator codeGen, ITypeSymbol typeSymbol)
    {
        CodeGen = codeGen;
        TypeSymbol = typeSymbol;
        MetadataType = codeGen.MetadataModule.GetOrAddTypeDefinition(typeSymbol);
    }

    public void DefineTypeBuilder()
    {
        var metadataType = MetadataType;
        metadataType.SetKind(GetMetadataTypeKind(TypeSymbol));

        if (TypeSymbol is INamedTypeSymbol metadataNamed)
        {
            metadataType.SetBaseType(metadataNamed.BaseType);
            metadataType.SetInterfaces(metadataNamed.Interfaces);
        }
        else
        {
            metadataType.SetBaseType(null);
            metadataType.SetInterfaces(ImmutableArray<ITypeSymbol>.Empty);
        }

        metadataType.SetCustomAttributes(TypeSymbol.GetAttributes());

        TypeAttributes typeAttributes = TypeAttributes.Public;

        if (TypeSymbol is INamedTypeSymbol named)
        {
            if (named.TypeKind == TypeKind.Delegate)
            {
                var accessibilityAttributes = GetTypeAccessibilityAttributes(named);
                TypeBuilder = CodeGen.ModuleBuilder.DefineType(
                    named.MetadataName,
                    accessibilityAttributes | TypeAttributes.Class | TypeAttributes.Sealed | TypeAttributes.AutoClass | TypeAttributes.AnsiClass,
                    ResolveClrType(named.BaseType));
                DefineTypeGenericParameters(named);
                CodeGen.ApplyCustomAttributes(TypeSymbol.GetAttributes(), attribute => TypeBuilder!.SetCustomAttribute(attribute));
                metadataType.SetAttributes(accessibilityAttributes | TypeAttributes.Class | TypeAttributes.Sealed | TypeAttributes.AutoClass | TypeAttributes.AnsiClass);
                return;
            }

            if (named.TypeKind == TypeKind.Interface)
            {
                typeAttributes = GetTypeAccessibilityAttributes(named) | TypeAttributes.Interface | TypeAttributes.Abstract;
                metadataType.SetInterfaces(named.Interfaces);
            }
            else
            {
                typeAttributes = GetTypeAccessibilityAttributes(named);
                if (named.IsAbstract)
                    typeAttributes |= TypeAttributes.Abstract;

                if (named.IsSealed)
                    typeAttributes |= TypeAttributes.Sealed;
            }
        }

        if (TypeSymbol.BaseType.Name == "Enum")
        {
            var accessibilityAttributes = GetTypeAccessibilityAttributes((INamedTypeSymbol)TypeSymbol);
            TypeBuilder = CodeGen.ModuleBuilder.DefineType(
                TypeSymbol.MetadataName,
                accessibilityAttributes | TypeAttributes.Sealed | TypeAttributes.Serializable,
                ResolveClrType(TypeSymbol.BaseType) // bör vara System.Enum
            );

            // Lägg till value__ direkt här
            TypeBuilder.DefineField(
                "value__",
                Compilation.GetTypeByMetadataName("System.Int32").GetClrType(Compilation),
                FieldAttributes.Public | FieldAttributes.SpecialName | FieldAttributes.RTSpecialName
            );

            CodeGen.ApplyCustomAttributes(TypeSymbol.GetAttributes(), attribute => TypeBuilder!.SetCustomAttribute(attribute));
            metadataType.SetAttributes(accessibilityAttributes | TypeAttributes.Sealed | TypeAttributes.Serializable);
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
                DefineTypeGenericParameters(nt);

                if (!nt.Interfaces.IsDefaultOrEmpty)
                {
                    foreach (var iface in nt.Interfaces)
                        TypeBuilder.AddInterfaceImplementation(ResolveClrType(iface));
                }

                CodeGen.ApplyCustomAttributes(TypeSymbol.GetAttributes(), attribute => TypeBuilder!.SetCustomAttribute(attribute));
                metadataType.SetAttributes(typeAttributes);
                return;
            }

            TypeBuilder = CodeGen.ModuleBuilder.DefineType(
                TypeSymbol.MetadataName,
                typeAttributes);
            metadataType.SetAttributes(typeAttributes);

            if (TypeSymbol is INamedTypeSymbol namedType)
                DefineTypeGenericParameters(namedType);

            if (TypeSymbol.BaseType is not null)
                TypeBuilder.SetParent(ResolveClrType(TypeSymbol.BaseType));

        }
        else if (TypeSymbol is INamedTypeSymbol synthesizedType)
        {
            var synthesizedAttributes = GetTypeAccessibilityAttributes(synthesizedType);

            if (synthesizedType.TypeKind == TypeKind.Interface)
                synthesizedAttributes |= TypeAttributes.Interface | TypeAttributes.Abstract;
            else if (synthesizedType.TypeKind == TypeKind.Struct)
            {
                synthesizedAttributes |= TypeAttributes.Sealed | TypeAttributes.SequentialLayout | TypeAttributes.AnsiClass;
            }
            else
            {
                synthesizedAttributes |= TypeAttributes.Class;

                if (synthesizedType.IsAbstract)
                    synthesizedAttributes |= TypeAttributes.Abstract;

                if (synthesizedType.IsSealed)
                    synthesizedAttributes |= TypeAttributes.Sealed;
            }

            TypeBuilder? containingTypeBuilder = null;
            if (synthesizedType.ContainingType is INamedTypeSymbol containingType)
            {
                var containingGenerator = CodeGen.GetOrCreateTypeGenerator(containingType);
                if (containingGenerator.TypeBuilder is null)
                    containingGenerator.DefineTypeBuilder();

                containingTypeBuilder = containingGenerator.TypeBuilder;
            }

            var baseClrType = synthesizedType.BaseType is not null
                ? ResolveClrType(synthesizedType.BaseType)
                : null;

            if (containingTypeBuilder is not null)
            {
                TypeBuilder = containingTypeBuilder.DefineNestedType(
                    synthesizedType.MetadataName,
                    synthesizedAttributes,
                    baseClrType);
                metadataType.SetAttributes(synthesizedAttributes);
            }
            else
            {
                TypeBuilder = CodeGen.ModuleBuilder.DefineType(
                    synthesizedType.MetadataName,
                    synthesizedAttributes);
                metadataType.SetAttributes(synthesizedAttributes);

                if (baseClrType is not null)
                    TypeBuilder.SetParent(baseClrType);
            }
        }

        if (TypeSymbol is INamedTypeSymbol nt2 && !nt2.Interfaces.IsDefaultOrEmpty)
        {
            foreach (var iface in nt2.Interfaces)
                TypeBuilder.AddInterfaceImplementation(ResolveClrType(iface));
            metadataType.SetInterfaces(nt2.Interfaces);
        }

        CodeGen.ApplyCustomAttributes(TypeSymbol.GetAttributes(), attribute => TypeBuilder!.SetCustomAttribute(attribute));
    }

    private static MetadataTypeKind GetMetadataTypeKind(ITypeSymbol typeSymbol)
    {
        if (typeSymbol is not INamedTypeSymbol namedType)
            return MetadataTypeKind.Unknown;

        return namedType.TypeKind switch
        {
            TypeKind.Class => MetadataTypeKind.Class,
            TypeKind.Struct => MetadataTypeKind.Struct,
            TypeKind.Interface => MetadataTypeKind.Interface,
            TypeKind.Delegate => MetadataTypeKind.Delegate,
            TypeKind.Enum => MetadataTypeKind.Enum,
            _ => MetadataTypeKind.Unknown
        };
    }

    private void DefineTypeGenericParameters(INamedTypeSymbol namedType)
    {
        if (TypeBuilder is null)
            return;

        if (namedType.TypeParameters.IsDefaultOrEmpty)
            return;

        var parameterBuilders = TypeBuilder.DefineGenericParameters(namedType.TypeParameters.Select(tp => tp.Name).ToArray());
        CodeGen.RegisterGenericParameters(namedType.TypeParameters, parameterBuilders);
    }

    private static TypeAttributes GetTypeAccessibilityAttributes(INamedTypeSymbol typeSymbol)
    {
        if (typeSymbol.ContainingType is null)
        {
            return typeSymbol.DeclaredAccessibility switch
            {
                Accessibility.Public => TypeAttributes.Public,
                Accessibility.Internal => TypeAttributes.NotPublic,
                Accessibility.Private => TypeAttributes.NotPublic,
                Accessibility.ProtectedAndProtected => TypeAttributes.NotPublic,
                Accessibility.ProtectedOrInternal => TypeAttributes.NotPublic,
                Accessibility.ProtectedAndInternal => TypeAttributes.NotPublic,
                _ => TypeAttributes.NotPublic
            };
        }

        return typeSymbol.DeclaredAccessibility switch
        {
            Accessibility.Public => TypeAttributes.NestedPublic,
            Accessibility.Private => TypeAttributes.NestedPrivate,
            Accessibility.ProtectedAndProtected => TypeAttributes.NestedFamily,
            Accessibility.Internal => TypeAttributes.NestedAssembly,
            Accessibility.ProtectedOrInternal => TypeAttributes.NestedFamORAssem,
            Accessibility.ProtectedAndInternal => TypeAttributes.NestedFamANDAssem,
            _ => TypeAttributes.NestedPrivate
        };
    }

    private static FieldAttributes GetFieldAccessibilityAttributes(IFieldSymbol fieldSymbol)
    {
        return fieldSymbol.DeclaredAccessibility switch
        {
            Accessibility.Public => FieldAttributes.Public,
            Accessibility.Private => FieldAttributes.Private,
            Accessibility.Internal => FieldAttributes.Assembly,
            Accessibility.ProtectedAndProtected => FieldAttributes.Family,
            Accessibility.ProtectedOrInternal => FieldAttributes.FamORAssem,
            Accessibility.ProtectedAndInternal => FieldAttributes.FamANDAssem,
            _ => FieldAttributes.Private
        };
    }

    internal static ParameterAttributes GetParameterAttributes(IParameterSymbol parameterSymbol)
    {
        ParameterAttributes attrs = ParameterAttributes.None;

        if (parameterSymbol.RefKind == RefKind.Out)
            attrs |= ParameterAttributes.Out;
        else if (parameterSymbol.RefKind == RefKind.In)
            attrs |= ParameterAttributes.In;

        return attrs;
    }

    internal static ImmutableArray<MetadataParameterDefinition> BuildParameterMetadata(ImmutableArray<IParameterSymbol> parameters)
    {
        if (parameters.IsDefaultOrEmpty)
            return ImmutableArray<MetadataParameterDefinition>.Empty;

        var builder = ImmutableArray.CreateBuilder<MetadataParameterDefinition>(parameters.Length);

        foreach (var parameter in parameters)
        {
            var metadataParameter = new MetadataParameterDefinition(parameter);
            metadataParameter.SetAttributes(GetParameterAttributes(parameter));
            metadataParameter.SetRequiresNullableAttribute(MetadataNullability.RequiresNullableAttribute(parameter.Type));
            metadataParameter.SetCustomAttributes(parameter.GetAttributes());
            builder.Add(metadataParameter);
        }

        return builder.MoveToImmutable();
    }

    private static ImmutableArray<IParameterSymbol> GetPropertyIndexParameters(IPropertySymbol propertySymbol)
    {
        if (propertySymbol.GetMethod is IMethodSymbol getter)
            return getter.Parameters;

        if (propertySymbol.SetMethod is IMethodSymbol setter)
        {
            var parameters = setter.Parameters;
            if (parameters.IsDefaultOrEmpty)
                return ImmutableArray<IParameterSymbol>.Empty;

            var length = setter.MethodKind == MethodKind.PropertySet && parameters.Length > 0
                ? parameters.Length - 1
                : parameters.Length;

            if (length <= 0)
                return ImmutableArray<IParameterSymbol>.Empty;

            var builder = ImmutableArray.CreateBuilder<IParameterSymbol>(length);
            for (var i = 0; i < length; i++)
                builder.Add(parameters[i]);

            return builder.MoveToImmutable();
        }

        return ImmutableArray<IParameterSymbol>.Empty;
    }

    public void DefineMemberBuilders()
    {
        if (TypeSymbol is SynthesizedDelegateTypeSymbol synthesizedDelegate)
        {
            DefineDelegateMembers(synthesizedDelegate);
            return;
        }

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
                CodeGen.ApplyCustomAttributes(fieldSymbol.GetAttributes(), attribute => fieldBuilder.SetCustomAttribute(attribute));
            }

            return;
        }

        foreach (var memberSymbol in TypeSymbol.GetMembers())
        {
            switch (memberSymbol)
            {
                case IMethodSymbol methodSymbol when methodSymbol.MethodKind is not (MethodKind.PropertyGet or MethodKind.PropertySet):
                    {
                        if (methodSymbol.MethodKind == MethodKind.LambdaMethod)
                            break;

                        var metadataMethod = MetadataType.GetOrAddMethodDefinition(methodSymbol);
                        var methodGenerator = new MethodGenerator(this, methodSymbol, metadataMethod, CodeGen.ILBuilderFactory);

                        if (methodSymbol is SourceLambdaSymbol sourceLambda && sourceLambda.HasCaptures)
                        {
                            var closure = EnsureLambdaClosure(sourceLambda);
                            methodGenerator.SetLambdaClosure(closure);
                        }

                        _methodGenerators[methodSymbol] = methodGenerator;
                        methodGenerator.DefineMethodBuilder();

                        CodeGen.AddMemberBuilder((SourceSymbol)methodSymbol, methodGenerator.MethodBase);
                        break;
                    }
                case IFieldSymbol fieldSymbol:
                    {
                        var type = fieldSymbol.Type.Equals(TypeSymbol, SymbolEqualityComparer.Default) ? TypeBuilder : ResolveClrType(fieldSymbol.Type);

                        FieldAttributes attr = GetFieldAccessibilityAttributes(fieldSymbol);

                        if (fieldSymbol.IsLiteral)
                        {
                            attr |= FieldAttributes.Literal;
                        }

                        if (fieldSymbol.IsStatic)
                        {
                            attr |= FieldAttributes.Static;
                        }

                        var metadataField = MetadataType.GetOrAddFieldDefinition(fieldSymbol);
                        metadataField.SetAttributes(attr);
                        metadataField.SetFieldType(fieldSymbol.Type);
                        metadataField.SetRequiresNullableAttribute(MetadataNullability.RequiresNullableAttribute(fieldSymbol.Type));
                        metadataField.SetCustomAttributes(fieldSymbol.GetAttributes());

                        var fieldBuilder = TypeBuilder.DefineField(fieldSymbol.Name, type, attr);
                        if (fieldSymbol.IsLiteral)
                        {
                            fieldBuilder.SetConstant(fieldSymbol.GetConstantValue());
                            metadataField.SetConstantValue(fieldSymbol.GetConstantValue());
                        }
                        else
                        {
                            metadataField.SetConstantValue(null);
                        }
                        var nullableAttr = CodeGen.CreateNullableAttribute(fieldSymbol.Type);
                        if (nullableAttr is not null)
                            fieldBuilder.SetCustomAttribute(nullableAttr);
                        CodeGen.ApplyCustomAttributes(fieldSymbol.GetAttributes(), attribute => fieldBuilder.SetCustomAttribute(attribute));
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

                        var metadataProperty = MetadataType.GetOrAddPropertyDefinition(propertySymbol);
                        metadataProperty.SetAttributes(PropertyAttributes.None);
                        metadataProperty.SetPropertyType(propertySymbol.Type);
                        metadataProperty.SetRequiresNullableAttribute(MetadataNullability.RequiresNullableAttribute(propertySymbol.Type));
                        metadataProperty.SetCustomAttributes(propertySymbol.GetAttributes());

                        var indexParameters = GetPropertyIndexParameters(propertySymbol);
                        metadataProperty.SetParameters(BuildParameterMetadata(indexParameters));

                        if (getterSymbol is not null)
                        {
                            var getterMetadata = MetadataType.GetOrAddMethodDefinition(getterSymbol);
                            getGen = new MethodGenerator(this, getterSymbol, getterMetadata, CodeGen.ILBuilderFactory);
                            _methodGenerators[getterSymbol] = getGen;
                            getGen.DefineMethodBuilder();
                            CodeGen.AddMemberBuilder((SourceSymbol)getterSymbol, getGen.MethodBase);
                            metadataProperty.SetGetAccessor(getGen.MetadataMethod);
                        }

                        if (setterSymbol is not null)
                        {
                            var setterMetadata = MetadataType.GetOrAddMethodDefinition(setterSymbol);
                            setGen = new MethodGenerator(this, setterSymbol, setterMetadata, CodeGen.ILBuilderFactory);
                            _methodGenerators[setterSymbol] = setGen;
                            setGen.DefineMethodBuilder();
                            CodeGen.AddMemberBuilder((SourceSymbol)setterSymbol, setGen.MethodBase);
                            metadataProperty.SetSetAccessor(setGen.MetadataMethod);
                        }

                        var propertyType = ResolveClrType(propertySymbol.Type);

                        Type[]? paramTypes = null;
                        if (!indexParameters.IsDefaultOrEmpty)
                        {
                            var builder = new Type[indexParameters.Length];
                            for (var i = 0; i < indexParameters.Length; i++)
                            {
                                var parameter = indexParameters[i];
                                var clrType = ResolveClrType(parameter.Type);
                                if (parameter.RefKind is RefKind.Ref or RefKind.Out or RefKind.In)
                                    clrType = clrType.MakeByRefType();

                                builder[i] = clrType;
                            }

                            paramTypes = builder;
                        }

                        var propBuilder = TypeBuilder.DefineProperty(propertySymbol.MetadataName, PropertyAttributes.None, propertyType, paramTypes);

                        if (getGen != null)
                            propBuilder.SetGetMethod((MethodBuilder)getGen.MethodBase);
                        if (setGen != null)
                            propBuilder.SetSetMethod((MethodBuilder)setGen.MethodBase);

                        var nullableAttr = CodeGen.CreateNullableAttribute(propertySymbol.Type);
                        if (nullableAttr is not null)
                            propBuilder.SetCustomAttribute(nullableAttr);

                        CodeGen.ApplyCustomAttributes(propertySymbol.GetAttributes(), attribute => propBuilder.SetCustomAttribute(attribute));

                        CodeGen.AddMemberBuilder((SourceSymbol)propertySymbol, propBuilder);
                        break;
                    }
            }
        }

    }

    private void DefineDelegateMembers(SynthesizedDelegateTypeSymbol delegateType)
    {
        if (TypeBuilder is null)
            throw new InvalidOperationException("Type builder must be defined before creating delegate members.");

        var ctorParameters = delegateType.Constructor.Parameters
            .Select(p => ResolveClrType(p.Type))
            .ToArray();

        var ctorMetadata = MetadataType.GetOrAddMethodDefinition(delegateType.Constructor);
        ctorMetadata.SetAttributes(MethodAttributes.Public | MethodAttributes.HideBySig | MethodAttributes.RTSpecialName | MethodAttributes.SpecialName);
        ctorMetadata.SetImplementationAttributes(MethodImplAttributes.Runtime | MethodImplAttributes.Managed);
        ctorMetadata.SetReturnType(delegateType.Constructor.ReturnType);
        ctorMetadata.SetRequiresNullableAttributeOnReturn(MetadataNullability.RequiresNullableAttribute(delegateType.Constructor.ReturnType));
        ctorMetadata.SetParameters(BuildParameterMetadata(delegateType.Constructor.Parameters));
        ctorMetadata.SetCustomAttributes(delegateType.Constructor.GetAttributes());
        ctorMetadata.SetReturnAttributes(delegateType.Constructor.GetReturnTypeAttributes());

        var ctorBuilder = TypeBuilder.DefineConstructor(
            MethodAttributes.Public | MethodAttributes.HideBySig | MethodAttributes.RTSpecialName | MethodAttributes.SpecialName,
            CallingConventions.Standard,
            ctorParameters);
        ctorBuilder.SetImplementationFlags(MethodImplAttributes.Runtime | MethodImplAttributes.Managed);
        CodeGen.AddMemberBuilder(delegateType.Constructor, ctorBuilder);

        var invokeParameters = delegateType.InvokeMethod.Parameters
            .Select(p => ResolveClrType(p.Type))
            .ToArray();

        var invokeMetadata = MetadataType.GetOrAddMethodDefinition(delegateType.InvokeMethod);
        invokeMetadata.SetAttributes(MethodAttributes.Public | MethodAttributes.HideBySig | MethodAttributes.Virtual);
        invokeMetadata.SetImplementationAttributes(MethodImplAttributes.Runtime | MethodImplAttributes.Managed);
        invokeMetadata.SetReturnType(delegateType.InvokeMethod.ReturnType);
        invokeMetadata.SetRequiresNullableAttributeOnReturn(MetadataNullability.RequiresNullableAttribute(delegateType.InvokeMethod.ReturnType));
        invokeMetadata.SetParameters(BuildParameterMetadata(delegateType.InvokeMethod.Parameters));
        invokeMetadata.SetCustomAttributes(delegateType.InvokeMethod.GetAttributes());
        invokeMetadata.SetReturnAttributes(delegateType.InvokeMethod.GetReturnTypeAttributes());

        var invokeBuilder = TypeBuilder.DefineMethod(
            delegateType.InvokeMethod.Name,
            MethodAttributes.Public | MethodAttributes.HideBySig | MethodAttributes.Virtual,
            ResolveClrType(delegateType.InvokeMethod.ReturnType),
            invokeParameters);
        invokeBuilder.SetImplementationFlags(MethodImplAttributes.Runtime | MethodImplAttributes.Managed);
        CodeGen.AddMemberBuilder(delegateType.InvokeMethod, invokeBuilder);
    }

    public void EmitMemberILBodies()
    {
        while (true)
        {
            var pending = _methodGenerators.Values
                .Where(static generator => !generator.HasEmittedBody)
                .ToList();

            if (pending.Count == 0)
                break;

            foreach (var methodGenerator in pending)
            {
                methodGenerator.EmitBody();
            }
        }
    }

    public Type CreateType()
    {
        foreach (var closure in _lambdaClosures.Values)
            closure.CreateType();

        Type ??= TypeBuilder!.CreateType();
        return Type!;
    }

    public bool HasMethodGenerator(IMethodSymbol methodSymbol)
    {
        return _methodGenerators.ContainsKey(methodSymbol);
    }

    public MethodGenerator? GetMethodGenerator(IMethodSymbol methodSymbol)
    {
        return _methodGenerators.TryGetValue(methodSymbol, out var generator)
            ? generator
            : null;
    }

    public void Add(IMethodSymbol methodSymbol, MethodGenerator methodGenerator)
    {
        _methodGenerators[methodSymbol] = methodGenerator;
    }

    public bool TryGetLambdaClosure(ILambdaSymbol lambdaSymbol, out LambdaClosure closure)
    {
        return _lambdaClosures.TryGetValue(lambdaSymbol, out closure);
    }

    internal LambdaClosure EnsureLambdaClosure(SourceLambdaSymbol lambdaSymbol)
    {
        if (_lambdaClosures.TryGetValue(lambdaSymbol, out var existing))
            return existing;

        if (TypeBuilder is null)
            throw new InvalidOperationException("Type builder must be defined before creating a lambda closure.");

        var closureName = $"<>c__LambdaClosure{_lambdaClosureOrdinal++}";
        var objectType = ResolveClrType(Compilation.GetSpecialType(SpecialType.System_Object));
        var closureBuilder = TypeBuilder.DefineNestedType(
            closureName,
            TypeAttributes.NestedPrivate | TypeAttributes.Sealed | TypeAttributes.Class,
            objectType);

        var ctor = closureBuilder.DefineDefaultConstructor(MethodAttributes.Public);

        var fields = new Dictionary<ISymbol, FieldBuilder>(SymbolEqualityComparer.Default);
        var index = 0;
        foreach (var captured in lambdaSymbol.CapturedVariables)
        {
            var fieldType = ResolveCapturedSymbolType(captured);
            var fieldName = CreateClosureFieldName(captured, index++);
            var fieldBuilder = closureBuilder.DefineField(fieldName, fieldType, FieldAttributes.Public);
            fields[captured] = fieldBuilder;
        }

        var closure = new LambdaClosure(closureBuilder, ctor, fields);
        _lambdaClosures[lambdaSymbol] = closure;
        return closure;
    }

    public Type ResolveClrType(ITypeSymbol typeSymbol)
    {
        return typeSymbol.GetClrType(CodeGen);
    }

    private Type ResolveCapturedSymbolType(ISymbol symbol)
    {
        var typeSymbol = symbol switch
        {
            ILocalSymbol local when local.Type is not null => local.Type,
            IParameterSymbol parameter when parameter.Type is not null => parameter.Type,
            IFieldSymbol field => field.Type,
            IPropertySymbol property => property.Type,
            ITypeSymbol type => type,
            _ => Compilation.ErrorTypeSymbol
        };

        return ResolveClrType(typeSymbol);
    }

    private static string CreateClosureFieldName(ISymbol symbol, int ordinal)
    {
        return symbol switch
        {
            ILocalSymbol local => $"<{local.Name}>__{ordinal}",
            IParameterSymbol parameter => $"<{parameter.Name}>__{ordinal}",
            ITypeSymbol => $"<>self__{ordinal}",
            _ => $"<>capture__{ordinal}"
        };
    }

    internal sealed class LambdaClosure
    {
        private readonly Dictionary<ISymbol, FieldBuilder> _fields;
        private Type? _createdType;

        public LambdaClosure(TypeBuilder typeBuilder, ConstructorBuilder constructor, Dictionary<ISymbol, FieldBuilder> fields)
        {
            TypeBuilder = typeBuilder;
            Constructor = constructor;
            _fields = fields;
        }

        public TypeBuilder TypeBuilder { get; }

        public ConstructorBuilder Constructor { get; }

        public bool TryGetField(ISymbol symbol, out FieldBuilder fieldBuilder) => _fields.TryGetValue(symbol, out fieldBuilder);

        public FieldBuilder GetField(ISymbol symbol) => _fields[symbol];

        public void CreateType()
        {
            if (_createdType is not null)
                return;

            _createdType = TypeBuilder.CreateType();
        }
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
