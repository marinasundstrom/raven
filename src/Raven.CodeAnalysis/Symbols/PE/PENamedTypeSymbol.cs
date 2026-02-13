using System;
using System.Collections.Immutable;
using System.Diagnostics;
using System.Linq;
using System.Net.NetworkInformation;
using System.Reflection;

using Raven.CodeAnalysis;

namespace Raven.CodeAnalysis.Symbols;

internal partial class PENamedTypeSymbol : PESymbol, INamedTypeSymbol
{
    private static readonly Dictionary<string, SpecialType> s_specialTypeByFullName = new(StringComparer.Ordinal)
    {
        ["System.Object"] = SpecialType.System_Object,
        ["System.Enum"] = SpecialType.System_Enum,
        ["System.MulticastDelegate"] = SpecialType.System_MulticastDelegate,
        ["System.Delegate"] = SpecialType.System_Delegate,
        ["System.ValueType"] = SpecialType.System_ValueType,
        ["System.Void"] = SpecialType.System_Void,
        ["System.Boolean"] = SpecialType.System_Boolean,
        ["System.Char"] = SpecialType.System_Char,
        ["System.SByte"] = SpecialType.System_SByte,
        ["System.Byte"] = SpecialType.System_Byte,
        ["System.Int16"] = SpecialType.System_Int16,
        ["System.UInt16"] = SpecialType.System_UInt16,
        ["System.Int32"] = SpecialType.System_Int32,
        ["System.UInt32"] = SpecialType.System_UInt32,
        ["System.Int64"] = SpecialType.System_Int64,
        ["System.UInt64"] = SpecialType.System_UInt64,
        ["System.Decimal"] = SpecialType.System_Decimal,
        ["System.Single"] = SpecialType.System_Single,
        ["System.Double"] = SpecialType.System_Double,
        ["System.String"] = SpecialType.System_String,
        ["System.IntPtr"] = SpecialType.System_IntPtr,
        ["System.UIntPtr"] = SpecialType.System_UIntPtr,
        ["System.Array"] = SpecialType.System_Array,
        ["System.Collections.IEnumerable"] = SpecialType.System_Collections_IEnumerable,
        ["System.Collections.Generic.IEnumerable`1"] = SpecialType.System_Collections_Generic_IEnumerable_T,
        ["System.Collections.Generic.IList`1"] = SpecialType.System_Collections_Generic_IList_T,
        ["System.Collections.Generic.ICollection`1"] = SpecialType.System_Collections_Generic_ICollection_T,
        ["System.Collections.IEnumerator"] = SpecialType.System_Collections_IEnumerator,
        ["System.Collections.Generic.IEnumerator`1"] = SpecialType.System_Collections_Generic_IEnumerator_T,
        ["System.Nullable`1"] = SpecialType.System_Nullable_T,
        ["System.DateTime"] = SpecialType.System_DateTime,
        ["System.Runtime.CompilerServices.IsVolatile"] = SpecialType.System_Runtime_CompilerServices_IsVolatile,
        ["System.IDisposable"] = SpecialType.System_IDisposable,
        ["System.TypedReference"] = SpecialType.System_TypedReference,
        ["System.ArgIterator"] = SpecialType.System_ArgIterator,
        ["System.RuntimeArgumentHandle"] = SpecialType.System_RuntimeArgumentHandle,
        ["System.RuntimeFieldHandle"] = SpecialType.System_RuntimeFieldHandle,
        ["System.RuntimeMethodHandle"] = SpecialType.System_RuntimeMethodHandle,
        ["System.RuntimeTypeHandle"] = SpecialType.System_RuntimeTypeHandle,
        ["System.IAsyncResult"] = SpecialType.System_IAsyncResult,
        ["System.AsyncCallback"] = SpecialType.System_AsyncCallback,
        ["System.Runtime.CompilerServices.AsyncVoidMethodBuilder"] = SpecialType.System_Runtime_CompilerServices_AsyncVoidMethodBuilder,
        ["System.Runtime.CompilerServices.AsyncTaskMethodBuilder"] = SpecialType.System_Runtime_CompilerServices_AsyncTaskMethodBuilder,
        ["System.Runtime.CompilerServices.AsyncTaskMethodBuilder`1"] = SpecialType.System_Runtime_CompilerServices_AsyncTaskMethodBuilder_T,
        ["System.Runtime.CompilerServices.AsyncStateMachineAttribute"] = SpecialType.System_Runtime_CompilerServices_AsyncStateMachineAttribute,
        ["System.Runtime.CompilerServices.IteratorStateMachineAttribute"] = SpecialType.System_Runtime_CompilerServices_IteratorStateMachineAttribute,
        ["System.Threading.Tasks.Task"] = SpecialType.System_Threading_Tasks_Task,
        ["System.Threading.Tasks.Task`1"] = SpecialType.System_Threading_Tasks_Task_T,
        ["System.Runtime.InteropServices.WindowsRuntime.EventRegistrationToken"] = SpecialType.System_Runtime_InteropServices_WindowsRuntime_EventRegistrationToken,
        ["System.Runtime.InteropServices.WindowsRuntime.EventRegistrationTokenTable`1"] = SpecialType.System_Runtime_InteropServices_WindowsRuntime_EventRegistrationTokenTable_T,
        ["System.ValueTuple`1"] = SpecialType.System_ValueTuple_T1,
        ["System.ValueTuple`2"] = SpecialType.System_ValueTuple_T2,
        ["System.ValueTuple`3"] = SpecialType.System_ValueTuple_T3,
        ["System.ValueTuple`4"] = SpecialType.System_ValueTuple_T4,
        ["System.ValueTuple`5"] = SpecialType.System_ValueTuple_T5,
        ["System.ValueTuple`6"] = SpecialType.System_ValueTuple_T6,
        ["System.ValueTuple`7"] = SpecialType.System_ValueTuple_T7,
        ["System.ValueTuple`8"] = SpecialType.System_ValueTuple_TRest,
        ["System.Type"] = SpecialType.System_Type,
        ["System.Exception"] = SpecialType.System_Exception,
        ["System.Runtime.CompilerServices.IAsyncStateMachine"] = SpecialType.System_Runtime_CompilerServices_IAsyncStateMachine,
    };

    protected readonly ReflectionTypeLoader _reflectionTypeLoader;
    protected readonly System.Reflection.TypeInfo _typeInfo;
    private readonly bool _isValueType;
    private readonly List<ISymbol> _members = new(); //new(SymbolEqualityComparer.Default);
    private INamedTypeSymbol? _baseType;
    private bool _membersLoaded;
    private ImmutableArray<ITypeParameterSymbol>? _typeParameters;
    private string _name;
    private ImmutableArray<INamedTypeSymbol>? _interfaces;
    private ImmutableArray<INamedTypeSymbol>? _allInterfaces;
    private readonly ITypeSymbol? _constructedFrom;
    private readonly ITypeSymbol? _originalDefinition;
    private bool _extensionReceiverTypeComputed;
    private ITypeSymbol? _extensionReceiverType;
    private bool _extensionMarkerMembersComputed;
    private bool _hasExtensionMarkerMembers;

    internal static PENamedTypeSymbol Create(
        ReflectionTypeLoader reflectionTypeLoader,
        System.Reflection.TypeInfo typeInfo,
        ISymbol containingSymbol,
        INamedTypeSymbol? containingType,
        INamespaceSymbol? containingNamespace,
        Location[] locations)
    {
        var name = typeInfo.Name;

        foreach (var attribute in GetCustomAttributesSafe(typeInfo))
        {
            var attributeName = GetAttributeTypeName(attribute);
            if (attributeName is null)
                continue;

            if (attributeName == "System.Runtime.CompilerServices.DiscriminatedUnionAttribute")
            {
                // Do not eagerly load members during construction; keep loading lazy to avoid re-entrancy/duplication.
                return new PEDiscriminatedUnionSymbol(reflectionTypeLoader, typeInfo, containingSymbol, containingType, containingNamespace, locations).AddAsMember();
            }

            if (attributeName == "System.Runtime.CompilerServices.DiscriminatedUnionCaseAttribute")
            {
                IDiscriminatedUnionSymbol? unionSymbol = null;

                if (containingType is IDiscriminatedUnionSymbol containingUnion)
                {
                    unionSymbol = containingUnion;
                }

                if (TryGetAttributeConstructorTypeArgument(attribute, out var unionType))
                {
                    unionSymbol = reflectionTypeLoader.ResolveType(unionType) as IDiscriminatedUnionSymbol;
                }

                return new PEDiscriminatedUnionCaseSymbol(
                    reflectionTypeLoader,
                    typeInfo,
                    containingSymbol,
                    containingType,
                    containingNamespace,
                    locations,
                    unionSymbol).AddAsMember();

            }
        }

        if (LooksLikeDiscriminatedUnion(typeInfo))
        {
            // Do not eagerly load members during construction; keep loading lazy to avoid re-entrancy/duplication.
            return new PEDiscriminatedUnionSymbol(reflectionTypeLoader, typeInfo, containingSymbol, containingType, containingNamespace, locations).AddAsMember();
        }

        if (containingType is IDiscriminatedUnionSymbol parentUnion)
        {
            return new PEDiscriminatedUnionCaseSymbol(
                reflectionTypeLoader,
                typeInfo,
                containingSymbol,
                containingType,
                containingNamespace,
                locations,
                parentUnion).AddAsMember();
        }

        return new PENamedTypeSymbol(reflectionTypeLoader, typeInfo, containingSymbol, containingType, containingNamespace, locations, addAsMember: false)
            .AddAsMember();
    }

    private static bool LooksLikeDiscriminatedUnion(System.Reflection.TypeInfo typeInfo)
    {
        try
        {
            var fields = typeInfo.DeclaredFields;

            return fields.Any(f => DiscriminatedUnionFieldUtilities.IsTagFieldName(f.Name))
                && fields.Any(f => DiscriminatedUnionFieldUtilities.IsPayloadFieldName(f.Name));
        }
        catch (ArgumentException)
        {
            return false;
        }
    }

    private static bool IsValueTypeLike(System.Reflection.TypeInfo typeInfo)
    {
        var runtimeType = typeInfo.AsType();

        try
        {
            if (runtimeType.IsValueType || runtimeType.IsPrimitive || runtimeType.IsEnum)
                return true;

            var baseTypeName = typeInfo.BaseType?.FullName;
            return baseTypeName == "System.ValueType" || baseTypeName == "System.Enum";
        }
        catch (ArgumentException)
        {
            // MetadataLoadContext can throw while resolving incomplete type graphs.
            // Fall back to stable runtime shape checks for known value-type families.
            var fullName = runtimeType.FullName ?? typeInfo.FullName ?? string.Empty;
            return fullName is "System.Decimal" or "System.DateTime" or "System.TimeSpan" or "System.Guid";
        }
    }

    internal static IEnumerable<CustomAttributeData> GetCustomAttributesSafe(System.Reflection.TypeInfo typeInfo)
    {
        IList<CustomAttributeData> attributes;
        try
        {
            attributes = typeInfo.GetCustomAttributesData();
        }
        catch (ArgumentException)
        {
            yield break;
        }

        foreach (var attribute in attributes)
        {
            CustomAttributeData? safeAttribute;
            try
            {
                safeAttribute = attribute;
            }
            catch (ArgumentException)
            {
                continue;
            }

            yield return safeAttribute;
        }
    }

    private static string? GetAttributeTypeName(CustomAttributeData attribute)
    {
        try
        {
            return attribute.AttributeType.FullName;
        }
        catch (ArgumentException)
        {
            return null;
        }
    }

    private static bool TryGetAttributeConstructorTypeArgument(CustomAttributeData attribute, out Type unionType)
    {
        unionType = null!;

        try
        {
            if (attribute.ConstructorArguments is [{ Value: Type unionTypeValue }])
            {
                unionType = unionTypeValue;
                return true;
            }
        }
        catch (ArgumentException)
        {
            return false;
        }

        return false;
    }

    public PENamedTypeSymbol(ReflectionTypeLoader reflectionTypeLoader, System.Reflection.TypeInfo typeInfo, ISymbol containingSymbol, INamedTypeSymbol? containingType, INamespaceSymbol? containingNamespace, Location[] locations, bool addAsMember = true)
        : base(containingSymbol, containingType, containingNamespace, locations, addAsMember: addAsMember)
    {
        _reflectionTypeLoader = reflectionTypeLoader;
        _typeInfo = typeInfo;

        _isValueType = IsValueTypeLike(typeInfo);

        if (typeInfo?.Name == "Object" || typeInfo.BaseType?.Name == "Object")
        {
            TypeKind = TypeKind.Class;
            (_constructedFrom, _originalDefinition) = ResolveGenericOrigins();
            return;
        }

        if (typeInfo.IsEnum)
            TypeKind = TypeKind.Enum;
        else if (IsDelegateType(typeInfo))
            TypeKind = TypeKind.Delegate;
        else if (_isValueType)
            TypeKind = TypeKind.Struct;
        else if (typeInfo.IsInterface)
            TypeKind = TypeKind.Interface;
        else if (typeInfo.IsPointer)
            TypeKind = TypeKind.Pointer;
        else if (typeInfo.IsArray)
            TypeKind = TypeKind.Array;
        else
            TypeKind = TypeKind.Class;

        (_constructedFrom, _originalDefinition) = ResolveGenericOrigins();
    }

    internal ITypeSymbol? GetExtensionReceiverType()
    {
        if (_extensionReceiverTypeComputed)
            return _extensionReceiverType;

        _extensionReceiverTypeComputed = true;

        foreach (var method in GetMembers().OfType<IMethodSymbol>())
        {
            if (!method.IsStatic || !method.IsExtensionMethod)
                continue;

            if (method.Parameters.IsDefaultOrEmpty || method.Parameters.Length == 0)
                continue;

            _extensionReceiverType = method.Parameters[0].Type;
            break;
        }

        if (_extensionReceiverType is null)
        {
            foreach (var nestedType in GetMembers().OfType<PENamedTypeSymbol>())
            {
                foreach (var member in nestedType.GetMembers())
                {
                    if (member is PEMethodSymbol peMethod && peMethod.TryGetExtensionMarkerName(out _))
                    {
                        _extensionReceiverType = nestedType.GetExtensionMarkerReceiverType(peMethod);
                        break;
                    }

                    if (member is PEPropertySymbol peProperty && peProperty.TryGetExtensionMarkerName(out _))
                    {
                        _extensionReceiverType = nestedType.GetExtensionMarkerReceiverType(peProperty);
                        break;
                    }
                }

                if (_extensionReceiverType is not null)
                    break;
            }
        }

        return _extensionReceiverType;
    }

    internal bool HasExtensionMarkerMembers()
    {
        if (_extensionMarkerMembersComputed)
            return _hasExtensionMarkerMembers;

        _extensionMarkerMembersComputed = true;

        foreach (var member in GetMembers())
        {
            if (member is PEMethodSymbol peMethod && peMethod.TryGetExtensionMarkerName(out _))
            {
                _hasExtensionMarkerMembers = true;
                return true;
            }

            if (member is PEPropertySymbol peProperty && peProperty.TryGetExtensionMarkerName(out _))
            {
                _hasExtensionMarkerMembers = true;
                return true;
            }
        }

        _hasExtensionMarkerMembers = false;
        return false;
    }

    internal ITypeSymbol? GetExtensionMarkerReceiverType(ISymbol member)
    {
        if (member.ContainingType is not PENamedTypeSymbol)
            return null;

        if (!TryGetExtensionMarkerName(member, out var markerName))
            return null;

        if (string.IsNullOrWhiteSpace(markerName))
            return null;

        var markerType = GetMembers(markerName).OfType<INamedTypeSymbol>().FirstOrDefault();
        if (markerType is null)
            return null;

        var markerMethod = markerType.GetMembers("<Extension>$").OfType<IMethodSymbol>().FirstOrDefault()
            ?? markerType.GetMembers().OfType<IMethodSymbol>().FirstOrDefault(m => m.Name == "<Extension>$");

        if (markerMethod is null || markerMethod.Parameters.IsDefaultOrEmpty)
            return null;

        var receiverType = markerMethod.Parameters[0].Type;
        return receiverType;
    }

    private static bool TryGetExtensionMarkerName(ISymbol member, out string markerName)
    {
        markerName = string.Empty;

        return member switch
        {
            PEMethodSymbol peMethod => peMethod.TryGetExtensionMarkerName(out markerName),
            PEPropertySymbol peProperty => peProperty.TryGetExtensionMarkerName(out markerName),
            _ => false
        };
    }

    public override SymbolKind Kind => SymbolKind.Type;
    public override string Name => _name ??= _typeInfo.IsGenericType ? StripArity(_typeInfo.Name) : _typeInfo.Name;

    string StripArity(string name)
    {
        var index = name.IndexOf('`');
        return index >= 0 ? name.Substring(0, index) : name;
    }

    public override string MetadataName => _typeInfo.Name;

    public bool IsNamespace { get; } = false;
    public bool IsType { get; } = true;
    public bool IsValueType => _isValueType;
    public bool IsReferenceType => !_isValueType;
    public bool IsInterface => _typeInfo.IsInterface;

    public ImmutableArray<IMethodSymbol> Constructors => GetMembers(".ctor").OfType<IMethodSymbol>().ToImmutableArray();
    public ImmutableArray<IMethodSymbol> InstanceConstructors => Constructors;
    public IMethodSymbol? StaticConstructor { get; }
    public ImmutableArray<ITypeSymbol> TypeArguments { get; } = [];

    public ImmutableArray<ITypeParameterSymbol> TypeParameters =>
        _typeParameters ??= ComputeTypeParameters();

    public int Arity => GetMetadataArity();

    private int GetMetadataArity()
    {
        var name = _typeInfo.Name;
        var index = name.IndexOf('`');
        if (index < 0)
            return 0;

        if (int.TryParse(name.AsSpan(index + 1), out var arity))
            return arity;

        return 0;
    }

    private ImmutableArray<ITypeParameterSymbol> ComputeTypeParameters()
    {
        var declaredArity = GetMetadataArity();

        // Non-generic or no params: trivial
        if (declaredArity == 0)
            return ImmutableArray<ITypeParameterSymbol>.Empty;

        var allParams = _typeInfo.GenericTypeParameters;
        if (allParams.Length == 0)
            return ImmutableArray<ITypeParameterSymbol>.Empty;

        // For nested generic type definitions, reflection gives you:
        //   [outer generic params..., inner generic params...]
        //
        // So: inner *declared* parameters are the LAST `declaredArity` ones.
        //
        // Example:
        //   class Outer<TOuter>
        //   {
        //       class Inner<TInner> { }
        //   }
        //
        // typeof(Outer<,>.Inner<>).GenericTypeParameters:
        //   [TOuter, TInner]
        // Name "Inner`1" -> declaredArity = 1 -> take last 1 -> [TInner]

        int start = Math.Max(0, allParams.Length - declaredArity);
        var slice = allParams
            .AsSpan(start, declaredArity)
            .ToArray();

        var builder = ImmutableArray.CreateBuilder<ITypeParameterSymbol>(slice.Length);
        foreach (var tp in slice)
        {
            var paramSymbol = (ITypeParameterSymbol)_reflectionTypeLoader.ResolveType(tp)!;
            builder.Add(paramSymbol);
        }

        return builder.ToImmutable();
    }

    public ITypeSymbol? ConstructedFrom => _constructedFrom;

    private (ITypeSymbol constructedFrom, ITypeSymbol originalDefinition) ResolveGenericOrigins()
    {
        var self = (ITypeSymbol)this;
        var constructedFrom = self;
        var originalDefinition = self;

        if (!_typeInfo.IsGenericType)
            return (constructedFrom, originalDefinition);

        if (_typeInfo.IsGenericTypeDefinition)
            return (constructedFrom, originalDefinition);

        var type = _typeInfo.AsType();
        var definitionType = type.GetGenericTypeDefinition();

        if (ReferenceEquals(definitionType, type))
            return (constructedFrom, originalDefinition);

        var resolved = _reflectionTypeLoader.ResolveType(definitionType);
        if (resolved is INamedTypeSymbol definitionSymbol)
        {
            constructedFrom = definitionSymbol;

            originalDefinition = definitionSymbol.OriginalDefinition ?? definitionSymbol;
        }

        return (constructedFrom, originalDefinition);
    }
    public bool IsAbstract => _typeInfo.IsAbstract;
    public bool IsSealed => _typeInfo.IsSealed;
    public override bool IsStatic => TypeKind == TypeKind.Class && IsAbstract && IsSealed;
    public bool IsGenericType => _typeInfo.IsGenericType;
    public bool IsUnboundGenericType => _typeInfo.IsGenericTypeDefinition;
    public ImmutableArray<INamedTypeSymbol> Interfaces
    {
        get
        {
            if (_interfaces is null)
            {
                var all = AllInterfaces;
                _interfaces = all
                    .Where(i => (BaseType is null || !BaseType.AllInterfaces.Contains(i, SymbolEqualityComparer.Default)) &&
                                !all.Any(other => !SymbolEqualityComparer.Default.Equals(i, other) &&
                                                 other.AllInterfaces.Contains(i, SymbolEqualityComparer.Default)))
                    .ToImmutableArray();
            }

            return _interfaces.Value;
        }
    }

    public ImmutableArray<INamedTypeSymbol> AllInterfaces =>
        _allInterfaces ??= _typeInfo.GetInterfaces()
            .Select(i => (INamedTypeSymbol)_reflectionTypeLoader.ResolveType(i)!)
            .ToImmutableArray();

    private static bool IsDelegateType(System.Reflection.TypeInfo typeInfo)
    {
        if (typeInfo.FullName == typeof(MulticastDelegate).FullName ||
            typeInfo.FullName == typeof(Delegate).FullName)
        {
            return true;
        }

        if (typeof(MulticastDelegate).IsAssignableFrom(typeInfo))
            return true;

        var baseType = typeInfo.BaseType;
        while (baseType is not null)
        {
            if (baseType.FullName == typeof(MulticastDelegate).FullName)
                return true;

            baseType = baseType.BaseType;
        }

        return false;
    }

    public SpecialType SpecialType
    {
        get
        {
            var type = _typeInfo.AsType();
            var fullName = type.FullName;
            if (fullName is not null &&
                s_specialTypeByFullName.TryGetValue(fullName, out var specialType))
            {
                return specialType;
            }

            if (type.IsArray)
                return SpecialType.System_Array;

            return SpecialType.None;
        }
    }

    public INamedTypeSymbol? BaseType => _baseType ??= (_typeInfo.BaseType is not null ? (INamedTypeSymbol?)PEContainingModule.GetType(_typeInfo.BaseType) : null);

    public TypeKind TypeKind { get; }

    public ITypeSymbol? OriginalDefinition => _originalDefinition;

    private ImmutableArray<IFieldSymbol>? _tupleElements;

    public INamedTypeSymbol UnderlyingTupleType => this;

    public ImmutableArray<IFieldSymbol> TupleElements
    {
        get
        {
            if (!IsValueTupleSpecialType())
                return ImmutableArray<IFieldSymbol>.Empty;

            if (_tupleElements is not null)
                return _tupleElements.Value;

            EnsureMembersLoaded();

            _tupleElements = _members
                .OfType<IFieldSymbol>()
                .Where(@field => @field.Name.StartsWith("Item", StringComparison.Ordinal))
                .OrderBy(@field => @field.Name, StringComparer.Ordinal)
                .ToImmutableArray();

            return _tupleElements.Value;
        }
    }

    public ImmutableArray<ISymbol> GetMembers()
    {
        EnsureMembersLoaded();
        return _members.ToImmutableArray();
    }

    public ImmutableArray<ISymbol> GetMembers(string name)
    {
        EnsureMembersLoaded();
        return _members.Where(x => x.Name == name).ToImmutableArray();
    }

    public bool IsMemberDefined(string name, out ISymbol? symbol)
    {
        symbol = _members.FirstOrDefault(m => m.Name == name);
        return symbol is not null;
    }

    public ITypeSymbol? LookupType(string name)
    {
        EnsureMembersLoaded();
        return _members
            .OfType<INamedTypeSymbol>()
            .FirstOrDefault(type => type.Name == name);
    }

    private bool IsValueTupleSpecialType()
    {
        return SpecialType is SpecialType.System_ValueTuple_T1
            or SpecialType.System_ValueTuple_T2
            or SpecialType.System_ValueTuple_T3
            or SpecialType.System_ValueTuple_T4
            or SpecialType.System_ValueTuple_T5
            or SpecialType.System_ValueTuple_T6
            or SpecialType.System_ValueTuple_T7;
    }

    internal void AddMember(ISymbol member)
    {
        _members.Add(member);

        /*
        if (!_members.Add(member))
        {
            throw new InvalidOperationException($"Member '{member.ToDisplayString()}' has already been added to type '{this.ToDisplayString()}'");
        }
        */
    }

    private void EnsureMembersLoaded()
    {
        if (_membersLoaded)
            return;

        _membersLoaded = true;

        foreach (var methodInfo in _typeInfo.DeclaredMethods)
        {
            if (methodInfo.IsSpecialName)
                continue;

            var name = methodInfo.Name;

            if (name.StartsWith("get_")
                || name.StartsWith("set_")
                || name.StartsWith("add_")
                || name.StartsWith("remove_"))
                continue;

            new PEMethodSymbol(
                _reflectionTypeLoader,
                methodInfo,
                this,
                [new MetadataLocation(ContainingModule!)]);
        }

        foreach (var propertyInfo in _typeInfo.DeclaredProperties)
        {
            var property = new PEPropertySymbol(
                _reflectionTypeLoader,
                propertyInfo,
                this,
                [new MetadataLocation(ContainingModule!)]);

            if (propertyInfo.GetMethod is not null)
            {
                property.GetMethod = new PEMethodSymbol(
                    _reflectionTypeLoader,
                    propertyInfo.GetMethod,
                    this,
                    this,
                    [new MetadataLocation(ContainingModule!)],
                    associatedSymbol: property);
            }

            if (propertyInfo.SetMethod is not null)
            {
                property.SetMethod = new PEMethodSymbol(
                    _reflectionTypeLoader,
                    propertyInfo.SetMethod,
                    this,
                    this,
                    [new MetadataLocation(ContainingModule!)],
                    associatedSymbol: property);
            }
        }

        if (this.HasStaticExtensionMembers())
        {
            var props = ExtensionPropertyReflection.GroupByAccessorConvention(_typeInfo);

            foreach (var prop in props)
            {
                var property = new SynthesizedExtensionPropertySymbol(
                        this,
                        [new MetadataLocation(ContainingModule!)],
                        [], name: prop.Name);

                if (prop.GetMethod is not null)
                {
                    property.GetMethod = new PEMethodSymbol(
                        _reflectionTypeLoader,
                        prop.GetMethod,
                        this,
                        this,
                        [new MetadataLocation(ContainingModule!)],
                        associatedSymbol: property);
                }

                if (prop.SetMethod is not null)
                {
                    property.SetMethod = new PEMethodSymbol(
                        _reflectionTypeLoader,
                        prop.SetMethod,
                        this,
                        this,
                        [new MetadataLocation(ContainingModule!)],
                        associatedSymbol: property);
                }
            }
        }

        foreach (var eventInfo in _typeInfo.DeclaredEvents)
        {
            var @event = new PEEventSymbol(
                _reflectionTypeLoader,
                eventInfo,
                this,
                [new MetadataLocation(ContainingModule!)]);

            if (eventInfo.AddMethod is not null)
            {
                @event.AddMethod = new PEMethodSymbol(
                    _reflectionTypeLoader,
                    eventInfo.AddMethod,
                    this,
                    this,
                    [new MetadataLocation(ContainingModule!)],
                    associatedSymbol: @event);
            }

            if (eventInfo.RemoveMethod is not null)
            {
                @event.RemoveMethod = new PEMethodSymbol(
                    _reflectionTypeLoader,
                    eventInfo.RemoveMethod,
                    this,
                    this,
                    [new MetadataLocation(ContainingModule!)],
                    associatedSymbol: @event);
            }
        }

        foreach (var fieldInfo in _typeInfo.DeclaredFields)
        {
            if (fieldInfo.IsSpecialName)
                continue;

            new PEFieldSymbol(
                _reflectionTypeLoader,
                fieldInfo,
                this,
                [new MetadataLocation(ContainingModule!)]);
        }

        foreach (var constructorInfo in _typeInfo.DeclaredConstructors)
        {
            new PEMethodSymbol(
                _reflectionTypeLoader,
                constructorInfo,
                this,
                [new MetadataLocation(ContainingModule!)]);
        }

        foreach (var nestedTypeInfo in _typeInfo.DeclaredNestedTypes)
        {
            // Always intern nested types via the module's Type-based cache to avoid creating duplicate symbols.
            // The module is responsible for placing nested types under the correct containing type.
            var module = (PEModuleSymbol)ContainingModule;
            _ = module.GetType(nestedTypeInfo.AsType());
        }
    }

    public System.Reflection.TypeInfo GetTypeInfo() => _typeInfo;

    public ITypeSymbol Construct(params ITypeSymbol[] typeArguments)
    {
        if (typeArguments.Length != Arity)
            throw new ArgumentException($"Type '{Name}' expects {Arity} type arguments, but got {typeArguments.Length}.");

        return new ConstructedNamedTypeSymbol(this, typeArguments.ToImmutableArray());
    }

    public override void Complete()
    {
        IsCompleted = true;
    }
}
