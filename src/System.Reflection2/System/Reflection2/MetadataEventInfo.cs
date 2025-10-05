namespace System.Reflection2;

using System;
using System.Collections.Generic;
using System.Linq;
using System.Reflection;
using System.Reflection.Metadata;
using System.Reflection.Metadata.Ecma335;

/// <summary>
/// Reflection-only <see cref="EventInfo"/> implementation backed by metadata.
/// </summary>
public sealed class MetadataEventInfo : EventInfo
{
    private readonly MetadataType _declaringType;
    private readonly EventDefinitionHandle _handle;
    private readonly EventDefinition _definition;
    private readonly Lazy<Type> _eventHandlerType;
    private readonly Lazy<MethodInfo?> _addMethod;
    private readonly Lazy<MethodInfo?> _removeMethod;
    private readonly Lazy<MethodInfo?> _raiseMethod;
    private readonly Lazy<MethodInfo[]> _otherMethods;
    private readonly Lazy<IList<CustomAttributeData>> _customAttributes;

    internal MetadataEventInfo(MetadataType declaringType, EventDefinitionHandle handle)
    {
        _declaringType = declaringType ?? throw new ArgumentNullException(nameof(declaringType));
        _handle = handle;
        _definition = declaringType.Reader.GetEventDefinition(handle);
        _eventHandlerType = new Lazy<Type>(() => declaringType.MetadataModule.ResolveType(_definition.Type, _declaringType, null));
        _addMethod = new Lazy<MethodInfo?>(() => ResolveAccessor(_definition.GetAccessors().Adder));
        _removeMethod = new Lazy<MethodInfo?>(() => ResolveAccessor(_definition.GetAccessors().Remover));
        _raiseMethod = new Lazy<MethodInfo?>(() => ResolveAccessor(_definition.GetAccessors().Raiser));
        _otherMethods = new Lazy<MethodInfo[]>(ResolveOtherMethods);
        _customAttributes = new Lazy<IList<CustomAttributeData>>(() => MetadataCustomAttributeDecoder.Decode(_declaringType.MetadataModule, _definition.GetCustomAttributes(), _declaringType, null));
    }

    public override EventAttributes Attributes => _definition.Attributes;

    public override Type? DeclaringType => _declaringType;

    public override string Name => _declaringType.Reader.GetString(_definition.Name);

    public override Type? ReflectedType => DeclaringType;

    public override MethodInfo? AddMethod => _addMethod.Value;

    public override MethodInfo? RemoveMethod => _removeMethod.Value;

    public override MethodInfo? RaiseMethod => _raiseMethod.Value;

    public override Type EventHandlerType => _eventHandlerType.Value;

    public override int MetadataToken => MetadataTokens.GetToken(_handle);

    public override bool IsMulticast => typeof(MulticastDelegate).IsAssignableFrom(EventHandlerType);

    public override IList<CustomAttributeData> GetCustomAttributesData()
        => _customAttributes.Value;

    public override object[] GetCustomAttributes(bool inherit)
        => throw new NotSupportedException("Materializing attribute instances is not supported in metadata-only context.");

    public override object[] GetCustomAttributes(Type attributeType, bool inherit)
        => throw new NotSupportedException("Materializing attribute instances is not supported in metadata-only context.");

    public override bool IsDefined(Type attributeType, bool inherit)
        => _customAttributes.Value.Any(a => attributeType.IsAssignableFrom(a.AttributeType));

    public override void AddEventHandler(object? target, Delegate? handler)
    {
        var bridge = _declaringType.MetadataModule.RuntimeBridge;
        if (bridge is null)
        {
            throw new NotSupportedException("Invocation is not supported in metadata-only context. Configure MetadataLoadContext.RuntimeBridge to enable invocation.");
        }

        bridge.AddEventHandler(this, target, handler);
    }

    public override void RemoveEventHandler(object? target, Delegate? handler)
    {
        var bridge = _declaringType.MetadataModule.RuntimeBridge;
        if (bridge is null)
        {
            throw new NotSupportedException("Invocation is not supported in metadata-only context. Configure MetadataLoadContext.RuntimeBridge to enable invocation.");
        }

        bridge.RemoveEventHandler(this, target, handler);
    }

    public override MethodInfo? GetAddMethod(bool nonPublic)
    {
        var method = _addMethod.Value;
        if (method is null)
        {
            return null;
        }

        return method.IsPublic || nonPublic ? method : null;
    }

    public override MethodInfo? GetRemoveMethod(bool nonPublic)
    {
        var method = _removeMethod.Value;
        if (method is null)
        {
            return null;
        }

        return method.IsPublic || nonPublic ? method : null;
    }

    public override MethodInfo? GetRaiseMethod(bool nonPublic)
    {
        var method = _raiseMethod.Value;
        if (method is null)
        {
            return null;
        }

        return method.IsPublic || nonPublic ? method : null;
    }

    public override MethodInfo[] GetOtherMethods(bool nonPublic)
    {
        if (nonPublic)
        {
            return (MethodInfo[])_otherMethods.Value.Clone();
        }

        return _otherMethods.Value.Where(method => method.IsPublic).ToArray();
    }

    private MethodInfo? ResolveAccessor(MethodDefinitionHandle handle)
    {
        if (handle.IsNil)
        {
            return null;
        }

        return _declaringType.ResolveMethod(handle);
    }

    private MethodInfo[] ResolveOtherMethods()
    {
        var others = _definition.GetAccessors().Others;
        if (others.Length == 0)
        {
            return Array.Empty<MethodInfo>();
        }

        var list = new List<MethodInfo>(others.Length);
        foreach (var handle in others)
        {
            var method = ResolveAccessor(handle);
            if (method is not null)
            {
                list.Add(method);
            }
        }

        return list.ToArray();
    }
}
