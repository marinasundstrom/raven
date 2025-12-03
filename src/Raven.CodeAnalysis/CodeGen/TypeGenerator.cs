using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;
using System.Reflection;
using System.Reflection.Emit;
using System.Runtime.CompilerServices;

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

    public IEnumerable<MethodGenerator> MethodGenerators => _methodGenerators.Values;

    public Type? Type { get; private set; }

    ImmutableArray<ITypeParameterSymbol> _inheritedTypeParameters = ImmutableArray<ITypeParameterSymbol>.Empty;
    bool _releasedInheritedTypeParameters;

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
            if (named.TypeKind == TypeKind.Delegate)
            {
                var accessibilityAttributes = GetTypeAccessibilityAttributes(named);
                TypeBuilder = CodeGen.ModuleBuilder.DefineType(
                    named.MetadataName,
                    accessibilityAttributes | TypeAttributes.Class | TypeAttributes.Sealed | TypeAttributes.AutoClass | TypeAttributes.AnsiClass,
                    ResolveClrType(named.BaseType));
                DefineTypeGenericParameters(named);
                CodeGen.ApplyCustomAttributes(TypeSymbol.GetAttributes(), attribute => TypeBuilder!.SetCustomAttribute(attribute));
                return;
            }

            if (named.TypeKind == TypeKind.Interface)
            {
                typeAttributes = GetTypeAccessibilityAttributes(named) | TypeAttributes.Interface | TypeAttributes.Abstract;
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
            // Raven enums currently default to an Int32 underlying type. If the language adds support
            // for explicit enum bases we should thread that information through the symbol model and
            // resolve it here instead of hard-coding Int32.
            var enumUnderlyingType = Compilation.GetSpecialType(SpecialType.System_Int32);
            var runtimeUnderlyingType = ResolveClrType(enumUnderlyingType);

            TypeBuilder.DefineField(
                "value__",
                runtimeUnderlyingType,
                FieldAttributes.Public | FieldAttributes.SpecialName | FieldAttributes.RTSpecialName
            );

            CodeGen.ApplyCustomAttributes(TypeSymbol.GetAttributes(), attribute => TypeBuilder!.SetCustomAttribute(attribute));
            return;
        }

        TypeBuilder? containingTypeBuilder = null;
        if (TypeSymbol is INamedTypeSymbol { ContainingType: INamedTypeSymbol containingType })
        {
            var containingGenerator = CodeGen.GetOrCreateTypeGenerator(containingType);
            if (containingGenerator.TypeBuilder is null)
                containingGenerator.DefineTypeBuilder();

            containingTypeBuilder = containingGenerator.TypeBuilder;
        }

        var syntaxReference = TypeSymbol.DeclaringSyntaxReferences.FirstOrDefault();
        if (syntaxReference is not null)
        {
            if (TypeSymbol is INamedTypeSymbol nt && nt.TypeKind == TypeKind.Interface)
            {
                if (containingTypeBuilder is not null)
                {
                    var nestedName = GetNestedTypeMetadataName(nt);
                    TypeBuilder = containingTypeBuilder.DefineNestedType(
                        nestedName,
                        typeAttributes);
                }
                else
                {
                    TypeBuilder = CodeGen.ModuleBuilder.DefineType(
                        TypeSymbol.MetadataName,
                        typeAttributes);
                }

                DefineTypeGenericParameters(nt);

                if (!nt.Interfaces.IsDefaultOrEmpty)
                {
                    foreach (var iface in nt.Interfaces)
                        TypeBuilder.AddInterfaceImplementation(ResolveClrType(iface));
                }

                CodeGen.ApplyCustomAttributes(TypeSymbol.GetAttributes(), attribute => TypeBuilder!.SetCustomAttribute(attribute));
                return;
            }

            if (containingTypeBuilder is not null && TypeSymbol is INamedTypeSymbol nestedType)
            {
                var nestedName = GetNestedTypeMetadataName(nestedType);
                var baseClrType = TypeSymbol.BaseType is not null
                    ? ResolveClrType(TypeSymbol.BaseType)
                    : null;

                TypeBuilder = containingTypeBuilder.DefineNestedType(
                    nestedName,
                    typeAttributes,
                    baseClrType);
            }
            else
            {
                TypeBuilder = CodeGen.ModuleBuilder.DefineType(
                    TypeSymbol.MetadataName,
                    typeAttributes);

                if (TypeSymbol.BaseType is not null)
                    TypeBuilder.SetParent(ResolveClrType(TypeSymbol.BaseType));
            }

            if (TypeSymbol is INamedTypeSymbol namedType)
                DefineTypeGenericParameters(namedType);

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

            TypeBuilder? synthesizedContainingBuilder = null;
            if (synthesizedType.ContainingType is INamedTypeSymbol synthesizedContainingType)
            {
                var containingGenerator = CodeGen.GetOrCreateTypeGenerator(synthesizedContainingType);
                if (containingGenerator.TypeBuilder is null)
                    containingGenerator.DefineTypeBuilder();

                synthesizedContainingBuilder = containingGenerator.TypeBuilder;
            }

            var baseClrType = synthesizedType.BaseType is not null
                ? ResolveClrType(synthesizedType.BaseType)
                : null;

            if (synthesizedContainingBuilder is not null)
            {
                var nestedName = GetNestedTypeMetadataName(synthesizedType);
                TypeBuilder = synthesizedContainingBuilder.DefineNestedType(
                    nestedName,
                    synthesizedAttributes,
                    baseClrType);
            }
            else
            {
                TypeBuilder = CodeGen.ModuleBuilder.DefineType(
                    synthesizedType.MetadataName,
                    synthesizedAttributes);

                if (baseClrType is not null)
                    TypeBuilder.SetParent(baseClrType);
            }

            DefineTypeGenericParameters(synthesizedType);
        }

        if (TypeSymbol is INamedTypeSymbol nt2 && !nt2.Interfaces.IsDefaultOrEmpty)
        {
            foreach (var iface in nt2.Interfaces)
                TypeBuilder.AddInterfaceImplementation(ResolveClrType(iface));
        }

        CodeGen.ApplyCustomAttributes(TypeSymbol.GetAttributes(), attribute => TypeBuilder!.SetCustomAttribute(attribute));

        if (TypeSymbol is SourceDiscriminatedUnionSymbol)
        {
            var discriminatedUnionAttribute = CodeGen.CreateDiscriminatedUnionAttribute();
            TypeBuilder!.SetCustomAttribute(discriminatedUnionAttribute);
        }
        else if (TypeSymbol is SourceDiscriminatedUnionCaseTypeSymbol caseSymbol)
        {
            var unionType = caseSymbol.Union.GetClrType(CodeGen);
            var discriminatedUnionCaseAttribute = CodeGen.CreateDiscriminatedUnionCaseAttribute(unionType);
            TypeBuilder!.SetCustomAttribute(discriminatedUnionCaseAttribute);
        }
    }

    private static string GetNestedTypeMetadataName(INamedTypeSymbol type)
    {
        if (type is null)
            throw new ArgumentNullException(nameof(type));

        var name = type.Name;
        if (type.Arity > 0)
            name = $"{name}`{type.Arity}";

        return name;
    }

    private void DefineTypeGenericParameters(INamedTypeSymbol namedType)
    {
        if (TypeBuilder is null)
            return;

        var inScopeParameters = GetTypeParametersInScope(namedType);
        if (inScopeParameters.IsDefaultOrEmpty)
            return;

        var parameterBuilders = TypeBuilder.DefineGenericParameters(inScopeParameters.Select(tp => tp.Name).ToArray());
        CodeGen.RegisterGenericParameters(inScopeParameters, parameterBuilders);

        _inheritedTypeParameters = namedType.ContainingType is null
            ? ImmutableArray<ITypeParameterSymbol>.Empty
            : GetTypeParametersInScope(namedType.ContainingType);
    }

    private static ImmutableArray<ITypeParameterSymbol> GetTypeParametersInScope(INamedTypeSymbol? typeSymbol)
    {
        if (typeSymbol is null)
            return ImmutableArray<ITypeParameterSymbol>.Empty;

        var stack = new Stack<INamedTypeSymbol>();
        var current = typeSymbol;
        while (current is not null)
        {
            stack.Push(current);
            current = current.ContainingType;
        }

        var builder = ImmutableArray.CreateBuilder<ITypeParameterSymbol>();
        while (stack.Count > 0)
        {
            var next = stack.Pop();
            if (!next.TypeParameters.IsDefaultOrEmpty)
                builder.AddRange(next.TypeParameters);
        }

        return builder.ToImmutable();
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

    internal FieldBuilder EnsureFieldBuilder(SourceFieldSymbol fieldSymbol)
    {
        if (fieldSymbol is null)
            throw new ArgumentNullException(nameof(fieldSymbol));

        if (_fieldBuilders.TryGetValue(fieldSymbol, out var existing))
            return existing;

        if (TypeBuilder is null)
            DefineTypeBuilder();

        if (TypeBuilder is null)
            throw new InvalidOperationException("Type builder must be defined before creating field builders.");

        var fieldType = ResolveFieldClrType(fieldSymbol);
        var attributes = GetFieldAccessibilityAttributes(fieldSymbol);

        if (fieldSymbol.IsLiteral)
            attributes |= FieldAttributes.Literal;

        if (fieldSymbol.IsStatic)
            attributes |= FieldAttributes.Static;

        var fieldBuilder = TypeBuilder.DefineField(fieldSymbol.Name, fieldType, attributes);

        if (fieldSymbol.IsLiteral)
            fieldBuilder.SetConstant(fieldSymbol.GetConstantValue());

        var nullableAttr = CodeGen.CreateNullableAttribute(fieldSymbol.Type);
        if (nullableAttr is not null)
            fieldBuilder.SetCustomAttribute(nullableAttr);

        var tupleNamesAttr = CodeGen.CreateTupleElementNamesAttribute(fieldSymbol.Type);
        if (tupleNamesAttr is not null)
            fieldBuilder.SetCustomAttribute(tupleNamesAttr);

        CodeGen.ApplyCustomAttributes(fieldSymbol.GetAttributes(), attribute => fieldBuilder.SetCustomAttribute(attribute));

        _fieldBuilders[fieldSymbol] = fieldBuilder;
        CodeGen.AddMemberBuilder(fieldSymbol, fieldBuilder);

        return fieldBuilder;
    }

    private Type ResolveFieldClrType(IFieldSymbol fieldSymbol)
    {
        if (fieldSymbol is null)
            throw new ArgumentNullException(nameof(fieldSymbol));

        var fieldTypeSymbol = fieldSymbol.Type;

        if (fieldTypeSymbol.Equals(TypeSymbol, SymbolEqualityComparer.Default))
        {
            if (TypeBuilder is null)
                throw new InvalidOperationException("Type builder must be created before resolving field types.");

            return TypeBuilder;
        }

        var resolved = ResolveClrType(fieldTypeSymbol);

        if (resolved is TypeBuilder)
            return resolved;

        if (fieldTypeSymbol is INamedTypeSymbol named &&
            named.IsGenericType &&
            !named.TypeArguments.IsDefaultOrEmpty)
        {
            var definition = named.ConstructedFrom;

            try
            {
                var argumentTypes = named.TypeArguments
                    .Select(ResolveClrType)
                    .ToArray();

                Type definitionType;

                if (definition.SpecialType == SpecialType.System_Runtime_CompilerServices_AsyncTaskMethodBuilder_T)
                {
                    definitionType = typeof(AsyncTaskMethodBuilder<int>).GetGenericTypeDefinition();
                }
                else
                {
                    definitionType = ResolveClrType(definition);
                }

                if (definitionType.IsGenericTypeDefinition || definitionType.ContainsGenericParameters)
                    return definitionType.MakeGenericType(argumentTypes);

                if (resolved.IsGenericType && resolved.ContainsGenericParameters)
                    return resolved.GetGenericTypeDefinition().MakeGenericType(argumentTypes);
            }
            catch
            {
                // Fall back to the initially resolved type when the runtime types for the
                // generic arguments cannot be materialised (e.g. for unsupported type parameters).
            }
        }

        return resolved;
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
            if (memberSymbol.ContainingType is { } containingType &&
                !SymbolEqualityComparer.Default.Equals(containingType, TypeSymbol))
            {
                // Skip members that belong to nested types (e.g., discriminated union cases)
                // but are surfaced on the containing union symbol for semantic analysis.
                continue;
            }

            switch (memberSymbol)
            {
                case IMethodSymbol methodSymbol when methodSymbol.MethodKind is not (MethodKind.PropertyGet or MethodKind.PropertySet):
                    {
                        if (methodSymbol.MethodKind == MethodKind.LambdaMethod)
                            break;

                        var methodGenerator = new MethodGenerator(this, methodSymbol, CodeGen.ILBuilderFactory);

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
                        if (fieldSymbol is SourceFieldSymbol sourceField)
                        {
                            _ = EnsureFieldBuilder(sourceField);
                        }

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
                            getGen = new MethodGenerator(this, getterSymbol, CodeGen.ILBuilderFactory);
                            _methodGenerators[getterSymbol] = getGen;
                            getGen.DefineMethodBuilder();
                            CodeGen.AddMemberBuilder((SourceSymbol)getterSymbol, getGen.MethodBase);
                        }

                        if (setterSymbol is not null)
                        {
                            setGen = new MethodGenerator(this, setterSymbol, CodeGen.ILBuilderFactory);
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

        var ctorBuilder = TypeBuilder.DefineConstructor(
            MethodAttributes.Public | MethodAttributes.HideBySig | MethodAttributes.RTSpecialName | MethodAttributes.SpecialName,
            CallingConventions.Standard,
            ctorParameters);
        ctorBuilder.SetImplementationFlags(MethodImplAttributes.Runtime | MethodImplAttributes.Managed);
        CodeGen.AddMemberBuilder(delegateType.Constructor, ctorBuilder);

        var invokeParameters = delegateType.InvokeMethod.Parameters
            .Select(p => ResolveClrType(p.Type))
            .ToArray();

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
                CodeGen.CurrentEmittingMethod = methodGenerator.MethodSymbol;
                try
                {
                    methodGenerator.EmitBody();
                }
                catch (Exception ex)
                {
                    throw new InvalidOperationException($"Failed to emit method '{methodGenerator.MethodSymbol.ToDisplayString()}'", ex);
                }
            }
        }
    }

    public Type CreateType()
    {
        foreach (var closure in _lambdaClosures.Values)
            closure.CreateType();

        Type ??= TypeBuilder!.CreateType();
        ReleaseInheritedGenericParameters();
        return Type!;
    }

    private void ReleaseInheritedGenericParameters()
    {
        if (_releasedInheritedTypeParameters)
            return;

        if (!_inheritedTypeParameters.IsDefaultOrEmpty)
            CodeGen.UnregisterGenericParameters(_inheritedTypeParameters);

        _releasedInheritedTypeParameters = true;
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
            var capturedTypeSymbol = GetCapturedSymbolTypeSymbol(captured);
            var fieldType = ResolveClrType(capturedTypeSymbol);
            var fieldName = CreateClosureFieldName(captured, index++);
            var fieldBuilder = closureBuilder.DefineField(fieldName, fieldType, FieldAttributes.Public);

            var tupleAttr = CodeGen.CreateTupleElementNamesAttribute(capturedTypeSymbol);
            if (tupleAttr is not null)
                fieldBuilder.SetCustomAttribute(tupleAttr);

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
        return ResolveClrType(GetCapturedSymbolTypeSymbol(symbol));
    }

    private ITypeSymbol GetCapturedSymbolTypeSymbol(ISymbol symbol)
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

        return typeSymbol;
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

            // TypeBuilder.DefineMethodOverride should only be used for interface implementations.
            // Virtual overrides are emitted by setting the correct MethodAttributes when the
            // MethodBuilder is created, so there is no need to define an explicit override
            // mapping for base class methods. Skipping this avoids Reflection.Emit throwing
            // when the overridden method belongs to a base type rather than an interface.
            if (baseMethodInfo.DeclaringType?.IsInterface == true)
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
                methodInfo = peMethod.GetClrMethodInfo(CodeGen);
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
