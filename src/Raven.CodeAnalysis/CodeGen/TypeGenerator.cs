using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;
using System.Reflection;
using System.Reflection.Emit;
using System.Runtime.CompilerServices;
using System.Runtime.InteropServices;

using Raven.CodeAnalysis;
using Raven.CodeAnalysis.Symbols;

namespace Raven.CodeAnalysis.CodeGen;

internal class TypeGenerator
{
    readonly Dictionary<IMethodSymbol, MethodGenerator> _methodGenerators = new Dictionary<IMethodSymbol, MethodGenerator>(SymbolEqualityComparer.Default);
    readonly Dictionary<IFieldSymbol, FieldBuilder> _fieldBuilders = new Dictionary<IFieldSymbol, FieldBuilder>(SymbolEqualityComparer.Default);
    readonly Dictionary<ILambdaSymbol, LambdaClosure> _lambdaClosures = new Dictionary<ILambdaSymbol, LambdaClosure>(SymbolEqualityComparer.Default);

    private int _lambdaClosureOrdinal;

    private Compilation _compilation;
    private const string ExtensionMarkerMethodName = "<Extension>$";
    private const string ExtensionGroupingTypePrefix = "<>__RavenExtensionGrouping_For_";
    private const string ExtensionMarkerTypePrefix = "<>__RavenExtensionMarker_";
    private TypeBuilder? _extensionGroupingTypeBuilder;
    private TypeBuilder? _extensionMarkerTypeBuilder;
    private string? _extensionMarkerName;
    private GenericTypeParameterBuilder[]? _extensionGroupingTypeParameters;

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
            if (named is SourceNamedTypeSymbol sourceNamed && sourceNamed.IsExtensionDeclaration)
            {
                DefineExtensionContainerTypeBuilder(named);
                return;
            }

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

                if (named is SourceNamedTypeSymbol sn && sn.IsSealedHierarchy)
                {
                    // Sealed hierarchy: do NOT emit IL sealed — inheritance is allowed for permitted types
                }
                else if (named.IsClosed)
                {
                    typeAttributes |= TypeAttributes.Sealed;
                }
            }

            if (TypeSymbol is SourceDiscriminatedUnionSymbol unionSymbol)
            {
                var unionNamed = (INamedTypeSymbol)unionSymbol;
                typeAttributes |= unionNamed.IsGenericType
                    ? TypeAttributes.SequentialLayout
                    : TypeAttributes.ExplicitLayout;
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

            // Add value__ using the enum's bound underlying type
            // Defaults to Int32 if no explicit underlying type was specified
            var enumUnderlyingTypeSymbol =
                (TypeSymbol as INamedTypeSymbol)?.EnumUnderlyingType
               ?? Compilation.GetSpecialType(SpecialType.System_Int32);

            var runtimeUnderlyingType = ResolveClrType(enumUnderlyingTypeSymbol);

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

            if (TypeSymbol is INamedTypeSymbol namedType)
                DefineTypeGenericParameters(namedType);

            // Set base type after generic parameters are defined so type parameters (e.g. T) can be resolved.
            if (TypeSymbol.BaseType is not null)
                TypeBuilder!.SetParent(ResolveClrType(TypeSymbol.BaseType));

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

                if (synthesizedType.IsClosed)
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

            if (synthesizedContainingBuilder is not null)
            {
                var nestedName = GetNestedTypeMetadataName(synthesizedType);
                TypeBuilder = synthesizedContainingBuilder.DefineNestedType(
                    nestedName,
                    synthesizedAttributes);
            }
            else
            {
                TypeBuilder = CodeGen.ModuleBuilder.DefineType(
                    synthesizedType.MetadataName,
                    synthesizedAttributes);
            }

            DefineTypeGenericParameters(synthesizedType);

            // Set base type after generic parameters are defined so type parameters can be resolved.
            if (synthesizedType.BaseType is not null)
                TypeBuilder!.SetParent(ResolveClrType(synthesizedType.BaseType));
        }

        if (TypeSymbol is INamedTypeSymbol nt2 && !nt2.Interfaces.IsDefaultOrEmpty)
        {
            foreach (var iface in nt2.Interfaces)
                TypeBuilder.AddInterfaceImplementation(ResolveClrType(iface));
        }

        CodeGen.ApplyCustomAttributes(TypeSymbol.GetAttributes(), attribute => TypeBuilder!.SetCustomAttribute(attribute));

        if (TypeSymbol is SourceDiscriminatedUnionSymbol)
        {
            ApplyDiscriminatedUnionLayout();
            var discriminatedUnionAttribute = CodeGen.CreateDiscriminatedUnionAttribute();
            TypeBuilder!.SetCustomAttribute(discriminatedUnionAttribute);
        }
        else if (TypeSymbol is SourceDiscriminatedUnionCaseTypeSymbol caseSymbol)
        {
            var unionType = TypeSymbolExtensionsForCodeGen.GetClrType(caseSymbol.Union, CodeGen);
            var discriminatedUnionCaseAttribute = CodeGen.CreateUnionCaseAttribute(unionType);
            TypeBuilder!.SetCustomAttribute(discriminatedUnionCaseAttribute);
        }

        if (TypeSymbol is SourceNamedTypeSymbol sourceNamedType && sourceNamedType.IsSealedHierarchy)
        {
            var permittedClrTypes = sourceNamedType.PermittedDirectSubtypes
                .Select(t => TypeSymbolExtensionsForCodeGen.GetClrType(t, CodeGen))
                .ToArray();
            var closedHierarchyAttribute = CodeGen.CreateClosedHierarchyAttribute(permittedClrTypes);
            TypeBuilder!.SetCustomAttribute(closedHierarchyAttribute);
        }

        EnsureExtensionGroupingType();
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

    private void ApplyDiscriminatedUnionLayout()
    {
        if (TypeBuilder is null)
            return;

        if (TypeSymbol is not INamedTypeSymbol namedType)
            return;

        var layoutKind = LayoutKind.Sequential;

        if (TypeSymbol is SourceDiscriminatedUnionSymbol unionSymbol && ShouldUseExplicitUnionLayout(unionSymbol))
            layoutKind = LayoutKind.Explicit;
        var layoutCtor = typeof(StructLayoutAttribute).GetConstructor(new[] { typeof(LayoutKind) });
        if (layoutCtor is null)
            return;

        if (namedType.IsGenericType && layoutKind == LayoutKind.Explicit)
            throw new InvalidOperationException("Generic discriminated unions cannot use explicit layout on .NET.");

        var attribute = new CustomAttributeBuilder(layoutCtor, new object[] { layoutKind });
        TypeBuilder.SetCustomAttribute(attribute);
    }

    private bool ShouldUseExplicitUnionLayout(SourceDiscriminatedUnionSymbol unionSymbol)
    {
        var named = (INamedTypeSymbol)unionSymbol;
        if (named.IsGenericType)
            return false;

        return !UnionHasManagedReferences(unionSymbol);
    }

    private bool UnionHasManagedReferences(SourceDiscriminatedUnionSymbol unionSymbol)
    {
        var visited = new HashSet<ITypeSymbol>(SymbolEqualityComparer.Default);

        foreach (var caseSymbol in unionSymbol.Cases)
        {
            foreach (var parameter in caseSymbol.ConstructorParameters)
            {
                if (parameter.RefKind != RefKind.None || parameter.Type is null)
                    continue;

                if (ContainsManagedReference(parameter.Type, visited))
                    return true;
            }
        }

        return false;
    }

    private bool ContainsManagedReference(ITypeSymbol typeSymbol, HashSet<ITypeSymbol> visited)
    {
        if (typeSymbol is null)
            return false;

        var definition = typeSymbol.OriginalDefinition ?? typeSymbol;
        if (!visited.Add(definition))
            return false;

        if (typeSymbol.IsReferenceType)
            return true;

        if (typeSymbol is ITypeParameterSymbol)
            return true;

        switch (typeSymbol)
        {
            case IArrayTypeSymbol:
                return true;
            case RefTypeSymbol:
                return true;
            case IAddressTypeSymbol:
                return true;
            case IPointerTypeSymbol:
                return true;
            case NullableTypeSymbol nullableType:
                return ContainsManagedReference(nullableType.UnderlyingType, visited);
            case ITupleTypeSymbol tupleType:
                foreach (var element in tupleType.TupleElements)
                {
                    if (ContainsManagedReference(element.Type, visited))
                        return true;
                }

                return false;
            case INamedTypeSymbol named when named.IsValueType:
                foreach (var field in named.GetMembers().OfType<IFieldSymbol>())
                {
                    if (field.IsStatic)
                        continue;

                    if (ContainsManagedReference(field.Type, visited))
                        return true;
                }

                return false;
            default:
                return false;
        }
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

        if (fieldSymbol.IsConst)
            attributes |= FieldAttributes.Static | FieldAttributes.Literal | FieldAttributes.HasDefault;

        if (!fieldSymbol.IsMutable && !fieldSymbol.IsConst)
            attributes |= FieldAttributes.InitOnly;

        if (fieldSymbol.IsStatic)
            attributes |= FieldAttributes.Static;

        var fieldBuilder = TypeBuilder.DefineField(fieldSymbol.Name, fieldType, attributes);

        if (TypeSymbol is SourceDiscriminatedUnionSymbol unionSymbol)
        {
            if (ShouldUseExplicitUnionLayout(unionSymbol))
            {
                if (DiscriminatedUnionFieldUtilities.IsTagFieldName(fieldSymbol.Name))
                    fieldBuilder.SetOffset(DiscriminatedUnionFieldUtilities.TagFieldOffset);
                else if (DiscriminatedUnionFieldUtilities.IsPayloadFieldName(fieldSymbol.Name))
                    fieldBuilder.SetOffset(DiscriminatedUnionFieldUtilities.PayloadFieldOffset);
            }
        }

        if (fieldSymbol.IsConst)
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
        if (TypeSymbol is INamedTypeSymbol { TypeKind: TypeKind.Delegate } delegateType)
        {
            DefineDelegateMembers(delegateType);
            return;
        }

        if (TypeSymbol.BaseType.ContainingNamespace.Name == "System"
            && TypeSymbol.BaseType.Name == "Enum")
        {
            // Enum types only emit fields (value__ is created in DefineTypeBuilder).
            // Use the normal field path so const values, accessibility, and attributes are consistent.
            foreach (var fieldSymbol in TypeSymbol.GetMembers().OfType<IFieldSymbol>())
            {
                // The runtime backing field is injected directly when defining the enum type.
                if (string.Equals(fieldSymbol.Name, "value__", StringComparison.Ordinal))
                    continue;

                if (fieldSymbol is SourceFieldSymbol sourceField)
                {
                    _ = EnsureFieldBuilder(sourceField);
                }
                else if (fieldSymbol is SourceSymbol src)
                {
                    // Best-effort fallback for any non-SourceFieldSymbol fields that still
                    // participate in codegen (should be rare for enums).
                    var fieldType = ResolveFieldClrType(fieldSymbol);
                    var attributes = GetFieldAccessibilityAttributes(fieldSymbol);

                    if (fieldSymbol.IsConst)
                        attributes |= FieldAttributes.Static | FieldAttributes.Literal | FieldAttributes.HasDefault;

                    if (!fieldSymbol.IsMutable && !fieldSymbol.IsConst)
                        attributes |= FieldAttributes.InitOnly;

                    if (fieldSymbol.IsStatic)
                        attributes |= FieldAttributes.Static;

                    var builder = TypeBuilder.DefineField(fieldSymbol.Name, fieldType, attributes);

                    if (fieldSymbol.IsConst)
                        builder.SetConstant(fieldSymbol.GetConstantValue());

                    var nullableAttr = CodeGen.CreateNullableAttribute(fieldSymbol.Type);
                    if (nullableAttr is not null)
                        builder.SetCustomAttribute(nullableAttr);

                    var tupleNamesAttr = CodeGen.CreateTupleElementNamesAttribute(fieldSymbol.Type);
                    if (tupleNamesAttr is not null)
                        builder.SetCustomAttribute(tupleNamesAttr);

                    CodeGen.ApplyCustomAttributes(fieldSymbol.GetAttributes(), attribute => builder.SetCustomAttribute(attribute));

                    _fieldBuilders[fieldSymbol] = builder;
                    CodeGen.AddMemberBuilder(src, builder);
                }
            }

            return;
        }

        if (TypeSymbol is SourceNamedTypeSymbol sourceType && sourceType.IsExtensionDeclaration)
            DefineExtensionGroupingMembers();

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
                case IMethodSymbol methodSymbol when methodSymbol.MethodKind is not (MethodKind.PropertyGet or MethodKind.PropertySet or MethodKind.InitOnly or MethodKind.EventAdd or MethodKind.EventRemove):
                    {
                        if (methodSymbol is SynthesizedMainMethodSymbol { ContainsExecutableCode: false })
                            break;

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
                        if (methodGenerator.MethodBase is MethodBuilder methodBuilder)
                            ApplyExtensionMarkerNameAttribute(methodSymbol, methodBuilder.SetCustomAttribute);

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

                        DebugUtils.PrintDebug($"Defining propertySymbol: {propertySymbol.Name} from {TypeBuilder.Name}");

                        if (propertySymbol.IsExtensionProperty)
                        {
                            // Extension properties are not emitted as real CLR properties on the extension container.
                            // But their accessor methods *must* be emitted as real methods so invocation works.

                            if (getterSymbol is not null)
                            {
                                if (_methodGenerators.ContainsKey(getterSymbol))
                                    continue;

                                var getGen2 = new MethodGenerator(this, getterSymbol, CodeGen.ILBuilderFactory);
                                _methodGenerators[getterSymbol] = getGen2;
                                getGen2.DefineMethodBuilder();
                                if (getGen2.MethodBase is MethodBuilder getterBuilder)
                                    ApplyExtensionMarkerNameAttribute(getterSymbol, getterBuilder.SetCustomAttribute);
                                CodeGen.AddMemberBuilder((SourceSymbol)getterSymbol, getGen2.MethodBase);
                            }

                            if (setterSymbol is not null)
                            {
                                if (_methodGenerators.ContainsKey(setterSymbol))
                                    continue;

                                var setGen2 = new MethodGenerator(this, setterSymbol, CodeGen.ILBuilderFactory);
                                _methodGenerators[setterSymbol] = setGen2;
                                setGen2.DefineMethodBuilder();
                                if (setGen2.MethodBase is MethodBuilder setterBuilder)
                                    ApplyExtensionMarkerNameAttribute(setterSymbol, setterBuilder.SetCustomAttribute);
                                CodeGen.AddMemberBuilder((SourceSymbol)setterSymbol, setGen2.MethodBase);
                            }

                            // No CLR PropertyBuilder for extension properties on the container.
                            break;
                        }

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
                        if (propertySymbol.IsIndexer)
                        {
                            var parameters = propertySymbol.Parameters;
                            if (!parameters.IsDefaultOrEmpty)
                            {
                                paramTypes = parameters
                                    .Select(p => ResolveClrType(p.Type))
                                    .ToArray();
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
                        ApplyExtensionMarkerNameAttribute(propertySymbol, propBuilder.SetCustomAttribute);

                        CodeGen.AddMemberBuilder((SourceSymbol)propertySymbol, propBuilder);
                        break;
                    }
                case IEventSymbol eventSymbol:
                    {
                        var addSymbol = eventSymbol.AddMethod as IMethodSymbol;
                        var removeSymbol = eventSymbol.RemoveMethod as IMethodSymbol;

                        MethodGenerator? addGen = null;
                        MethodGenerator? removeGen = null;

                        if (addSymbol is not null)
                        {
                            addGen = new MethodGenerator(this, addSymbol, CodeGen.ILBuilderFactory);
                            _methodGenerators[addSymbol] = addGen;
                            addGen.DefineMethodBuilder();
                            CodeGen.AddMemberBuilder((SourceSymbol)addSymbol, addGen.MethodBase);
                        }

                        if (removeSymbol is not null)
                        {
                            removeGen = new MethodGenerator(this, removeSymbol, CodeGen.ILBuilderFactory);
                            _methodGenerators[removeSymbol] = removeGen;
                            removeGen.DefineMethodBuilder();
                            CodeGen.AddMemberBuilder((SourceSymbol)removeSymbol, removeGen.MethodBase);
                        }

                        var eventType = ResolveClrType(eventSymbol.Type);
                        var eventBuilder = TypeBuilder.DefineEvent(eventSymbol.MetadataName, EventAttributes.None, eventType);

                        if (addGen != null)
                            eventBuilder.SetAddOnMethod((MethodBuilder)addGen.MethodBase);
                        if (removeGen != null)
                            eventBuilder.SetRemoveOnMethod((MethodBuilder)removeGen.MethodBase);

                        var nullableAttr = CodeGen.CreateNullableAttribute(eventSymbol.Type);
                        if (nullableAttr is not null)
                            eventBuilder.SetCustomAttribute(nullableAttr);

                        CodeGen.ApplyCustomAttributes(eventSymbol.GetAttributes(), attribute => eventBuilder.SetCustomAttribute(attribute));
                        break;
                    }
            }
        }

    }

    private void DefineDelegateMembers(INamedTypeSymbol delegateType)
    {
        if (TypeBuilder is null)
            throw new InvalidOperationException("Type builder must be defined before creating delegate members.");

        var ctorSymbol = delegateType.Constructors.FirstOrDefault();
        if (ctorSymbol is not null)
        {
            var ctorParameters = ctorSymbol.Parameters
                .Select(p => ResolveClrType(p.Type))
                .ToArray();

            var ctorBuilder = TypeBuilder.DefineConstructor(
                MethodAttributes.Public | MethodAttributes.HideBySig | MethodAttributes.RTSpecialName | MethodAttributes.SpecialName,
                CallingConventions.Standard,
                ctorParameters);
            ctorBuilder.SetImplementationFlags(MethodImplAttributes.Runtime | MethodImplAttributes.Managed);
            CodeGen.ApplyCustomAttributes(ctorSymbol.GetAttributes(), attribute => ctorBuilder.SetCustomAttribute(attribute));
            if (ctorSymbol is SourceSymbol ctorSource)
                CodeGen.AddMemberBuilder(ctorSource, ctorBuilder);
        }

        var invokeSymbol = delegateType.GetDelegateInvokeMethod();
        if (invokeSymbol is not null)
        {
            var invokeParameters = invokeSymbol.Parameters
                .Select(p => ResolveClrType(p.Type))
                .ToArray();

            var invokeBuilder = TypeBuilder.DefineMethod(
                invokeSymbol.Name,
                MethodAttributes.Public | MethodAttributes.HideBySig | MethodAttributes.Virtual,
                ResolveClrType(invokeSymbol.ReturnType),
                invokeParameters);
            invokeBuilder.SetImplementationFlags(MethodImplAttributes.Runtime | MethodImplAttributes.Managed);
            CodeGen.ApplyCustomAttributes(invokeSymbol.GetAttributes(), attribute => invokeBuilder.SetCustomAttribute(attribute));
            if (invokeSymbol is SourceSymbol invokeSource)
                CodeGen.AddMemberBuilder(invokeSource, invokeBuilder);
        }
    }

    public void EmitMemberILBodies()
    {
        DebugUtils.PrintDebug($"Emitting IL bodies for type: {TypeSymbol.ToDisplayString()}");

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
                    DebugUtils.PrintDebug($"Emitting IL body for method: {methodGenerator.MethodSymbol.ToDisplayString()}");

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

        _extensionMarkerTypeBuilder?.CreateType();
        _extensionGroupingTypeBuilder?.CreateType();
        Type ??= TypeBuilder!.CreateType();
        ReleaseInheritedGenericParameters();
        return Type!;
    }

    internal void ApplyExtensionMarkerNameAttribute(IMethodSymbol methodSymbol, Action<CustomAttributeBuilder> apply)
    {
        if (!ShouldApplyExtensionMarkerName(methodSymbol))
            return;

        if (_extensionMarkerName is null)
            return;

        var builder = CodeGen.CreateExtensionMarkerNameAttribute(_extensionMarkerName);
        if (builder is not null)
            apply(builder);
    }

    internal void ApplyExtensionMarkerNameAttribute(IPropertySymbol propertySymbol, Action<CustomAttributeBuilder> apply)
    {
        if (!ShouldApplyExtensionMarkerName(propertySymbol))
            return;

        if (_extensionMarkerName is null)
            return;

        var builder = CodeGen.CreateExtensionMarkerNameAttribute(_extensionMarkerName);
        if (builder is not null)
            apply(builder);
    }

    private bool ShouldApplyExtensionMarkerName(IMethodSymbol methodSymbol)
    {
        if (TypeSymbol is not SourceNamedTypeSymbol sourceType || !sourceType.IsExtensionDeclaration)
            return false;

        if (_extensionMarkerName is null)
            return false;

        return methodSymbol.MethodKind is not (MethodKind.Constructor or MethodKind.StaticConstructor);
    }

    private bool ShouldApplyExtensionMarkerName(IPropertySymbol propertySymbol)
    {
        if (TypeSymbol is not SourceNamedTypeSymbol sourceType || !sourceType.IsExtensionDeclaration)
            return false;

        return _extensionMarkerName is not null;
    }

    private void DefineExtensionContainerTypeBuilder(INamedTypeSymbol named)
    {
        TypeAttributes typeAttributes = GetTypeAccessibilityAttributes(named);

        if (named.IsAbstract)
            typeAttributes |= TypeAttributes.Abstract;

        if (named.IsClosed)
            typeAttributes |= TypeAttributes.Sealed;

        TypeBuilder? containingTypeBuilder = null;
        if (named.ContainingType is INamedTypeSymbol containingType)
        {
            var containingGenerator = CodeGen.GetOrCreateTypeGenerator(containingType);
            if (containingGenerator.TypeBuilder is null)
                containingGenerator.DefineTypeBuilder();

            containingTypeBuilder = containingGenerator.TypeBuilder;
        }

        var baseClrType = named.BaseType is not null
            ? ResolveClrType(named.BaseType)
            : null;

        if (containingTypeBuilder is not null)
        {
            var nestedName = named.Name;
            TypeBuilder = containingTypeBuilder.DefineNestedType(
                nestedName,
                typeAttributes,
                baseClrType);
        }
        else
        {
            var metadataName = named.ContainingNamespace?.QualifyName(named.Name) ?? named.Name;
            TypeBuilder = CodeGen.ModuleBuilder.DefineType(
                metadataName,
                typeAttributes,
                baseClrType);
        }

        CodeGen.ApplyCustomAttributes(TypeSymbol.GetAttributes(), attribute => TypeBuilder!.SetCustomAttribute(attribute));

        var extensionAttribute = CodeGen.CreateExtensionAttributeBuilder();
        if (extensionAttribute is not null)
            TypeBuilder!.SetCustomAttribute(extensionAttribute);

        EnsureExtensionGroupingType();
    }

    private void EnsureExtensionGroupingType()
    {
        if (_extensionGroupingTypeBuilder is not null)
            return;

        if (TypeBuilder is null)
            return;

        if (TypeSymbol is not SourceNamedTypeSymbol sourceType || !sourceType.IsExtensionDeclaration)
            return;

        var receiverType = sourceType.ExtensionReceiverType;
        if (receiverType is null || receiverType.TypeKind == TypeKind.Error)
            return;

        var groupingTypeName = GetExtensionGroupingTypeName(receiverType);
        _extensionGroupingTypeBuilder = TypeBuilder.DefineNestedType(
            groupingTypeName,
            TypeAttributes.NestedPublic | TypeAttributes.Class | TypeAttributes.Sealed | TypeAttributes.SpecialName);

        DefineExtensionGroupingTypeParameters();

        var extensionAttribute = CodeGen.CreateExtensionAttributeBuilder();
        if (extensionAttribute is not null)
            _extensionGroupingTypeBuilder.SetCustomAttribute(extensionAttribute);

        DefineExtensionMarkerType(receiverType);
    }

    private void DefineExtensionGroupingTypeParameters()
    {
        if (_extensionGroupingTypeBuilder is null)
            return;

        var extensionTypeParameters = GetExtensionTypeParameters();
        if (extensionTypeParameters.IsDefaultOrEmpty)
            return;

        var names = new string[extensionTypeParameters.Length];
        for (int i = 0; i < names.Length; i++)
            names[i] = extensionTypeParameters[i].Name;

        _extensionGroupingTypeParameters = _extensionGroupingTypeBuilder.DefineGenericParameters(names);
    }

    private void DefineExtensionMarkerType(ITypeSymbol receiverType)
    {
        if (_extensionGroupingTypeBuilder is null)
            return;

        _extensionMarkerName = GetExtensionMarkerTypeName(receiverType);
        _extensionMarkerTypeBuilder = _extensionGroupingTypeBuilder.DefineNestedType(
            _extensionMarkerName,
            TypeAttributes.NestedPublic | TypeAttributes.Class | TypeAttributes.Abstract | TypeAttributes.Sealed | TypeAttributes.SpecialName);

        var receiverClrType = ResolveExtensionGroupingType(receiverType);

        var markerMethod = _extensionMarkerTypeBuilder.DefineMethod(
            ExtensionMarkerMethodName,
            MethodAttributes.Public | MethodAttributes.Static | MethodAttributes.HideBySig | MethodAttributes.SpecialName,
            typeof(void),
            new[] { receiverClrType });

        markerMethod.DefineParameter(1, ParameterAttributes.None, "self");

        var il = markerMethod.GetILGenerator();
        il.Emit(OpCodes.Ret);
    }

    private string GetExtensionGroupingTypeName(ITypeSymbol receiverType)
        => ExtensionGroupingTypePrefix + GetExtensionReceiverSuffix(receiverType);

    private string GetExtensionMarkerTypeName(ITypeSymbol receiverType)
        => ExtensionMarkerTypePrefix + $"{TypeSymbol.Name}_for_{GetExtensionReceiverSuffix(receiverType)}";

    private string GetExtensionReceiverSuffix(ITypeSymbol receiverType)
        => GetExtensionReceiverSuffix(receiverType, new HashSet<ITypeSymbol>(SymbolEqualityComparer.Default));

    private string GetExtensionReceiverSuffix(ITypeSymbol receiverType, HashSet<ITypeSymbol> visiting)
    {
        if (!visiting.Add(receiverType))
            return receiverType.Name;

        try
        {
            switch (receiverType)
            {
                case INamedTypeSymbol named:
                    {
                        var baseName = named.Name;
                        var typeArguments = named is ConstructedNamedTypeSymbol constructed
                            ? constructed.GetExplicitTypeArgumentsForInference()
                            : named.TypeArguments;

                        if (typeArguments.IsDefaultOrEmpty)
                            return baseName;

                        var args = new string[typeArguments.Length];
                        for (int i = 0; i < typeArguments.Length; i++)
                            args[i] = GetExtensionReceiverSuffix(typeArguments[i], visiting);

                        return $"{baseName}_{string.Join("_", args)}";
                    }
                case IArrayTypeSymbol arrayType:
                    return $"{GetExtensionReceiverSuffix(arrayType.ElementType, visiting)}_Array";
                case ITypeParameterSymbol typeParameter:
                    return typeParameter.Name;
                default:
                    return receiverType.Name;
            }
        }
        finally
        {
            visiting.Remove(receiverType);
        }
    }

    private Type ResolveExtensionGroupingType(ITypeSymbol typeSymbol)
    {
        var typeParameters = GetExtensionTypeParameters();
        var registered = false;
        if (!typeParameters.IsDefaultOrEmpty && _extensionGroupingTypeParameters is not null)
        {
            CodeGen.RegisterGenericParameters(typeParameters, _extensionGroupingTypeParameters);
            registered = true;
        }

        try
        {
            return ResolveClrType(typeSymbol);
        }
        finally
        {
            if (registered)
                CodeGen.UnregisterGenericParameters(typeParameters);
        }
    }

    internal ImmutableArray<ITypeParameterSymbol> GetExtensionTypeParameters()
        => TypeSymbol is SourceNamedTypeSymbol sourceType && sourceType.IsExtensionDeclaration
            ? sourceType.TypeParameters
            : ImmutableArray<ITypeParameterSymbol>.Empty;

    private void DefineExtensionGroupingMembers()
    {
        if (_extensionGroupingTypeBuilder is null || _extensionMarkerName is null)
            return;

        var markerAttribute = CodeGen.CreateExtensionMarkerNameAttribute(_extensionMarkerName);
        if (markerAttribute is null)
            return;

        var extensionTypeParameters = GetExtensionTypeParameters();
        var registered = false;
        if (!extensionTypeParameters.IsDefaultOrEmpty && _extensionGroupingTypeParameters is not null)
        {
            CodeGen.RegisterGenericParameters(extensionTypeParameters, _extensionGroupingTypeParameters);
            registered = true;
        }

        try
        {
            var emitted = new HashSet<IMethodSymbol>(SymbolEqualityComparer.Default);
            foreach (var memberSymbol in TypeSymbol.GetMembers())
            {
                if (memberSymbol.ContainingType is { } containingType &&
                    !SymbolEqualityComparer.Default.Equals(containingType, TypeSymbol))
                {
                    continue;
                }

                switch (memberSymbol)
                {
                    case IMethodSymbol methodSymbol when methodSymbol.MethodKind is not (MethodKind.PropertyGet or MethodKind.PropertySet or MethodKind.InitOnly or MethodKind.EventAdd or MethodKind.EventRemove):
                        {
                            // Only emit extension methods into the grouping type (as skeleton methods).
                            if (methodSymbol.IsExtensionMethod && emitted.Add(methodSymbol))
                            {
                                DefineExtensionSkeletonMethod(methodSymbol, markerAttribute);
                            }
                            break;
                        }
                    case IPropertySymbol propertySymbol:
                        {
                            // C#-compatible: do not emit a CLR Property for extension properties.
                            // Only emit their accessor methods into the grouping type.
                            if (propertySymbol.IsExtensionProperty)
                            {
                                if (propertySymbol.GetMethod is IMethodSymbol getMethod && emitted.Add(getMethod))
                                    DefineExtensionSkeletonMethod(getMethod, markerAttribute);

                                if (propertySymbol.SetMethod is IMethodSymbol setMethod && emitted.Add(setMethod))
                                    DefineExtensionSkeletonMethod(setMethod, markerAttribute);

                                // Only emit accessor skeletons, not a CLR property.
                                break;
                            }

                            // Non-extension properties (if any appear here) can keep existing behavior,
                            // but extension grouping is generally only for extension members.
                            break;
                        }
                        // Other member kinds ignored for extension grouping.
                }
            }
        }
        finally
        {
            if (registered)
                CodeGen.UnregisterGenericParameters(extensionTypeParameters);
        }
    }

    private void DefineExtensionSkeletonMethod(IMethodSymbol methodSymbol, CustomAttributeBuilder markerAttribute)
    {
        if (_extensionGroupingTypeBuilder is null)
            return;

        if (methodSymbol.MethodKind is MethodKind.Constructor or MethodKind.NamedConstructor or MethodKind.LambdaMethod)
            return;
        var emittedMethodName = GetEmittedMethodName(methodSymbol);
        if (string.IsNullOrWhiteSpace(emittedMethodName))
            return;

        var isExtensionInstance = methodSymbol.IsExtensionMethod;
        var isStatic = methodSymbol.IsStatic && !isExtensionInstance;
        var parameters = methodSymbol.Parameters;

        var methodTypeParameters = methodSymbol.TypeParameters;
        var extensionTypeParameterCount = GetExtensionTypeParameters().Length;
        var extensionMethodTypeParameters = extensionTypeParameterCount > 0 && methodTypeParameters.Length >= extensionTypeParameterCount
            ? methodTypeParameters.Take(extensionTypeParameterCount).ToImmutableArray()
            : ImmutableArray<ITypeParameterSymbol>.Empty;
        var methodSpecificTypeParameters = methodTypeParameters.Length > extensionTypeParameterCount
            ? methodTypeParameters.Skip(extensionTypeParameterCount).ToImmutableArray()
            : ImmutableArray<ITypeParameterSymbol>.Empty;

        var parameterSymbols = isExtensionInstance && parameters.Length > 0
            ? parameters.Skip(1).ToArray()
            : parameters.ToArray();

        var attributes = MethodAttributes.Public | MethodAttributes.HideBySig;

        if (isStatic)
            attributes |= MethodAttributes.Static;

        if (methodSymbol.MethodKind is MethodKind.Conversion or MethodKind.UserDefinedOperator or MethodKind.PropertyGet or MethodKind.PropertySet)
            attributes |= MethodAttributes.SpecialName;

        var methodBuilder = _extensionGroupingTypeBuilder.DefineMethod(
            emittedMethodName,
            attributes,
            CallingConventions.Standard);

        GenericTypeParameterBuilder[]? methodGenericBuilders = null;
        if (!methodSpecificTypeParameters.IsDefaultOrEmpty)
            methodGenericBuilders = methodBuilder.DefineGenericParameters(methodSpecificTypeParameters.Select(tp => tp.Name).ToArray());

        var registeredExtensionParameters = false;
        var registeredMethodParameters = false;

        try
        {
            if (!extensionMethodTypeParameters.IsDefaultOrEmpty &&
                _extensionGroupingTypeParameters is not null &&
                extensionMethodTypeParameters.Length == _extensionGroupingTypeParameters.Length)
            {
                CodeGen.RegisterGenericParameters(extensionMethodTypeParameters, _extensionGroupingTypeParameters);
                registeredExtensionParameters = true;
            }

            if (!methodSpecificTypeParameters.IsDefaultOrEmpty && methodGenericBuilders is not null)
            {
                CodeGen.RegisterGenericParameters(methodSpecificTypeParameters, methodGenericBuilders);
                registeredMethodParameters = true;
            }

            var parameterTypes = parameterSymbols
                .Select(p => ResolveClrType(p.Type))
                .Select((type, index) =>
                {
                    var parameter = parameterSymbols[index];
                    return parameter.RefKind is RefKind.Ref or RefKind.Out or RefKind.In or RefKind.RefReadOnly or RefKind.RefReadOnlyParameter
                        ? type.MakeByRefType()
                        : type;
                })
                .ToArray();

            var returnType = methodSymbol.ReturnType.SpecialType == SpecialType.System_Unit
                ? typeof(void)
                : ResolveClrType(methodSymbol.ReturnType);

            methodBuilder.SetSignature(returnType, null, null, parameterTypes, null, null);

            for (int i = 0; i < parameterSymbols.Length; i++)
            {
                var parameter = parameterSymbols[i];
                methodBuilder.DefineParameter(i + 1, ParameterAttributes.None, parameter.Name);
            }

            methodBuilder.SetCustomAttribute(markerAttribute);
            EmitSkeletonMethodBody(methodBuilder);
        }
        finally
        {
            if (registeredMethodParameters)
                CodeGen.UnregisterGenericParameters(methodSpecificTypeParameters);
            if (registeredExtensionParameters)
                CodeGen.UnregisterGenericParameters(extensionMethodTypeParameters);
        }
    }

    private static string GetEmittedMethodName(IMethodSymbol methodSymbol)
    {
        if (!string.IsNullOrWhiteSpace(methodSymbol.Name))
            return methodSymbol.Name;

        var declaration = methodSymbol.DeclaringSyntaxReferences
            .Select(r => r.GetSyntax())
            .FirstOrDefault();

        if (methodSymbol.MethodKind == MethodKind.Conversion &&
            declaration is ConversionOperatorDeclarationSyntax conversionDecl &&
            OperatorFacts.TryGetConversionOperatorMetadataName(conversionDecl.ConversionKindKeyword.Kind, out var conversionMetadataName))
        {
            return conversionMetadataName;
        }

        if (methodSymbol.MethodKind == MethodKind.UserDefinedOperator &&
            declaration is OperatorDeclarationSyntax operatorDecl &&
            OperatorFacts.TryGetUserDefinedOperatorInfo(
                operatorDecl.OperatorToken.Kind,
                operatorDecl.ParameterList.Parameters.Count,
                out var operatorInfo))
        {
            return operatorInfo.MetadataName;
        }

        return methodSymbol.MetadataName;
    }

    private MethodBuilder DefineExtensionSkeletonAccessor(
        IMethodSymbol accessorSymbol,
        Type returnType,
        CustomAttributeBuilder markerAttribute)
    {
        if (_extensionGroupingTypeBuilder is null)
            throw new InvalidOperationException("Grouping type builder is not available.");

        var isExtensionInstance = accessorSymbol.IsExtensionMethod;
        var isStatic = accessorSymbol.IsStatic && !isExtensionInstance;

        var parameters = accessorSymbol.Parameters;
        var parameterSymbols = isExtensionInstance && parameters.Length > 0
            ? parameters.Skip(1).ToArray()
            : parameters.ToArray();

        var parameterTypes = parameterSymbols
            .Select(p => ResolveClrType(p.Type))
            .Select((type, index) =>
            {
                var parameter = parameterSymbols[index];
                return parameter.RefKind is RefKind.Ref or RefKind.Out or RefKind.In or RefKind.RefReadOnly or RefKind.RefReadOnlyParameter
                    ? type.MakeByRefType()
                    : type;
            })
            .ToArray();

        var attributes = MethodAttributes.Public | MethodAttributes.HideBySig | MethodAttributes.SpecialName;

        if (isStatic)
            attributes |= MethodAttributes.Static;

        // Add required return-type modifier for init-only setters
        var isInitOnly = accessorSymbol.MethodKind == MethodKind.InitOnly;
        var requiredReturnMods = isInitOnly
            ? new[] { typeof(System.Runtime.CompilerServices.IsExternalInit) }
            : null;

        var accessorBuilder = _extensionGroupingTypeBuilder.DefineMethod(
            accessorSymbol.Name,
            attributes,
            CallingConventions.Standard,
            returnType,
            requiredReturnMods,
            null,
            parameterTypes,
            null,
            null);

        for (int i = 0; i < parameterSymbols.Length; i++)
        {
            var parameter = parameterSymbols[i];
            accessorBuilder.DefineParameter(i + 1, ParameterAttributes.None, parameter.Name);
        }

        accessorBuilder.SetCustomAttribute(markerAttribute);
        EmitSkeletonMethodBody(accessorBuilder);
        return accessorBuilder;
    }

    private static void EmitSkeletonMethodBody(MethodBuilder methodBuilder)
    {
        var ctor = typeof(NotImplementedException).GetConstructor(Type.EmptyTypes);
        var il = methodBuilder.GetILGenerator();
        if (ctor is not null)
            il.Emit(OpCodes.Newobj, ctor);
        il.Emit(OpCodes.Throw);
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
        if (TypeSymbol is SynthesizedAsyncStateMachineTypeSymbol asyncStateMachine &&
            asyncStateMachine.AsyncMethod.ContainingType is { } containingType &&
            !SymbolEqualityComparer.Default.Equals(containingType, TypeSymbol))
        {
            var hostGenerator = CodeGen.GetOrCreateTypeGenerator(containingType);
            if (!ReferenceEquals(hostGenerator, this))
                return hostGenerator.EnsureLambdaClosure(lambdaSymbol);
        }

        if (TypeSymbol is SynthesizedAsyncStateMachineTypeSymbol { AsyncMethod.ContainingSymbol: INamedTypeSymbol hostType } &&
            !SymbolEqualityComparer.Default.Equals(hostType, TypeSymbol))
        {
            var hostGenerator = CodeGen.GetOrCreateTypeGenerator(hostType);
            if (!ReferenceEquals(hostGenerator, this))
                return hostGenerator.EnsureLambdaClosure(lambdaSymbol);
        }

        if (_lambdaClosures.TryGetValue(lambdaSymbol, out var existing))
            return existing;

        if (TypeBuilder is null)
            throw new InvalidOperationException("Type builder must be defined before creating a lambda closure.");

        var closureName = $"<>c__LambdaClosure{_lambdaClosureOrdinal++}";
        var objectType = ResolveClrType(Compilation.GetSpecialType(SpecialType.System_Object));
        var baseType = Compilation.GetSpecialType(SpecialType.System_Object);
        var closureSymbol = new SourceNamedTypeSymbol(
            closureName,
            baseType,
            TypeKind.Class,
            lambdaSymbol.ContainingSymbol,
            containingType: TypeSymbol as INamedTypeSymbol,
            containingNamespace: TypeSymbol.ContainingNamespace,
            locations: lambdaSymbol.Locations.ToArray(),
            declaringSyntaxReferences: lambdaSymbol.DeclaringSyntaxReferences.ToArray(),
            isSealed: true,
            declaredAccessibility: Accessibility.Private);

        var closureGenerator = CodeGen.GetOrCreateTypeGenerator(closureSymbol);
        if (closureGenerator.TypeBuilder is null)
            closureGenerator.DefineTypeBuilder();

        var closureBuilder = closureGenerator.TypeBuilder
            ?? throw new InvalidOperationException("Failed to define closure type builder.");

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

        var closure = new LambdaClosure(closureSymbol, closureBuilder, ctor, fields);
        _lambdaClosures[lambdaSymbol] = closure;
        return closure;
    }

    public Type ResolveClrType(ITypeSymbol typeSymbol)
    {
        return TypeSymbolExtensionsForCodeGen.GetClrType(typeSymbol, CodeGen);
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

        public LambdaClosure(
            SourceNamedTypeSymbol symbol,
            TypeBuilder typeBuilder,
            ConstructorBuilder constructor,
            Dictionary<ISymbol, FieldBuilder> fields)
        {
            Symbol = symbol ?? throw new ArgumentNullException(nameof(symbol));
            TypeBuilder = typeBuilder;
            Constructor = constructor;
            _fields = fields;
        }

        public SourceNamedTypeSymbol Symbol { get; }
        public TypeBuilder TypeBuilder { get; }

        public ConstructorBuilder Constructor { get; }

        public bool TryGetField(ISymbol symbol, out FieldBuilder fieldBuilder) => _fields.TryGetValue(symbol, out fieldBuilder);

        public FieldBuilder GetField(ISymbol symbol) => _fields[symbol];

        public Type EnsureCreatedType()
        {
            CreateType();
            return _createdType ?? throw new InvalidOperationException("Lambda closure type was not created.");
        }

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
        var definitionMethod = interfaceMethod;
        if (interfaceMethod is SubstitutedMethodSymbol sub)
            definitionMethod = sub.OriginalDefinition;

        if (!TryGetInterfaceDefinitionMethodInfo(definitionMethod, out var definitionMethodInfo))
        {
            methodInfo = null!;
            return false;
        }

        // If the interface method is constructed, we MUST target the constructed method.
        if (!SymbolEqualityComparer.Default.Equals(interfaceMethod, definitionMethod) &&
            interfaceMethod.ContainingType is not null)
        {
            var constructedIfaceClr = ResolveClrType(interfaceMethod.ContainingType);

            // TypeBuilderInstantiation (emitted generic types)
            if (constructedIfaceClr is TypeBuilder || constructedIfaceClr.GetType().Name.Contains("TypeBuilderInstantiation"))
            {
                methodInfo = TypeBuilder.GetMethod(constructedIfaceClr, definitionMethodInfo);
                return true;
            }

            // Normal runtime constructed generic types (e.g. typeof(IEnumerable<int>))
            if (constructedIfaceClr.IsGenericType)
            {
                var parms = definitionMethod.Parameters.Select(p => GetParameterClrType(p)).ToArray();
                var flags = BindingFlags.Public | BindingFlags.NonPublic |
                            (definitionMethod.IsStatic ? BindingFlags.Static : BindingFlags.Instance);

                var m = constructedIfaceClr.GetMethod(definitionMethodInfo.Name, flags, null, parms, null);
                if (m is null)
                {
                    methodInfo = null!;
                    return false;
                }

                methodInfo = m;
                return true;
            }
        }

        // Non-constructed case: definition MethodInfo is fine.
        methodInfo = definitionMethodInfo;
        return true;
    }

    private bool TryGetInterfaceDefinitionMethodInfo(IMethodSymbol definitionMethod, out MethodInfo methodInfo)
    {
        // Source-defined interfaces: we should already have MethodBuilder registered.
        if (definitionMethod is SourceSymbol src)
        {
            if (CodeGen.GetMemberBuilder(src) is MethodInfo mb)
            {
                methodInfo = mb;
                return true;
            }
        }

        // Metadata-defined interfaces: resolve through reflection.
        if (definitionMethod.ContainingType is not null)
        {
            var ifaceClr = ResolveClrType(definitionMethod.ContainingType);

            // Ensure we reflect on the *generic type definition* for the definition method.
            if (ifaceClr.IsGenericType && !ifaceClr.IsGenericTypeDefinition)
                ifaceClr = ifaceClr.GetGenericTypeDefinition();

            // If ifaceClr is still a TypeBuilder/TypeBuilderInstantiation, avoid GetMethod here too.
            // But for metadata this will be a real runtime Type.
            var parameterTypes = definitionMethod.Parameters.Select(GetParameterClrType).ToArray();
            var flags = BindingFlags.Public | BindingFlags.NonPublic |
                        (definitionMethod.IsStatic ? BindingFlags.Static : BindingFlags.Instance);

            var m = ifaceClr.GetMethod(definitionMethod.Name, flags, null, parameterTypes, null);
            if (m is not null)
            {
                methodInfo = m;
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
                methodInfo = CodeGen.RuntimeSymbolResolver.GetMethodInfo(peMethod);
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
