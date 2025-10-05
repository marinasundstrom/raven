namespace System.Reflection2;

using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;
using System.Reflection;
using System.Reflection.Metadata;
using System.Reflection.Metadata.Ecma335;

internal sealed class MetadataMethodBody : MethodBody
{
    private readonly MetadataModule _module;
    private readonly MethodBodyBlock _body;
    private readonly MetadataReader _reader;
    private readonly MetadataType _declaringType;
    private readonly IReadOnlyList<Type>? _declaringTypeArguments;
    private readonly IReadOnlyList<Type>? _methodTypeArguments;
    private readonly Lazy<IList<LocalVariableInfo>> _locals;
    private readonly Lazy<IList<ExceptionHandlingClause>> _exceptionClauses;
    private readonly int _localSignatureToken;

    public MetadataMethodBody(
        MetadataModule module,
        MethodBodyBlock body,
        MetadataType declaringType,
        IReadOnlyList<Type>? declaringTypeArguments,
        IReadOnlyList<Type>? methodTypeArguments)
    {
        _module = module ?? throw new ArgumentNullException(nameof(module));
        _body = body ?? throw new ArgumentNullException(nameof(body));
        _reader = module.Reader;
        _declaringType = declaringType ?? throw new ArgumentNullException(nameof(declaringType));
        _declaringTypeArguments = declaringTypeArguments;
        _methodTypeArguments = methodTypeArguments;
        _localSignatureToken = body.LocalSignature.IsNil ? 0 : MetadataTokens.GetToken(body.LocalSignature);
        _locals = new Lazy<IList<LocalVariableInfo>>(DecodeLocals);
        _exceptionClauses = new Lazy<IList<ExceptionHandlingClause>>(DecodeExceptionClauses);
    }

    public override int LocalSignatureMetadataToken => _localSignatureToken;

    public override bool InitLocals => _body.LocalVariablesInitialized;

    public override int MaxStackSize => _body.MaxStack;

    public override IList<LocalVariableInfo> LocalVariables => _locals.Value;

    public override IList<ExceptionHandlingClause> ExceptionHandlingClauses => _exceptionClauses.Value;

    public override byte[] GetILAsByteArray()
        => _body.GetILBytes() ?? Array.Empty<byte>();

    private IList<LocalVariableInfo> DecodeLocals()
    {
        if (_body.LocalSignature.IsNil)
        {
            return Array.Empty<LocalVariableInfo>();
        }

        var signature = _reader.GetStandaloneSignature(_body.LocalSignature);
        var blobReader = _reader.GetBlobReader(signature.Signature);
        if (blobReader.ReadSignatureHeader().Kind != SignatureKind.LocalVariables)
        {
            return Array.Empty<LocalVariableInfo>();
        }

        var localCount = blobReader.ReadCompressedInteger();
        if (localCount == 0)
        {
            return Array.Empty<LocalVariableInfo>();
        }

        var provider = new MetadataPinnedTrackingTypeProvider(
            _module,
            _declaringTypeArguments,
            _methodTypeArguments,
            _declaringType,
            localCount);
        var decoder = new SignatureDecoder<Type, MetadataType?>(provider, _reader, _declaringType);
        var locals = new LocalVariableInfo[localCount];

        for (var i = 0; i < localCount; i++)
        {
            provider.SetCurrentLocalIndex(i);
            var localType = decoder.DecodeType(ref blobReader);
            locals[i] = new MetadataLocalVariableInfo(localType, provider.IsPinned(i), i);
        }

        return locals;
    }

    private IList<ExceptionHandlingClause> DecodeExceptionClauses()
    {
        if (_body.ExceptionRegions.Length == 0)
        {
            return Array.Empty<ExceptionHandlingClause>();
        }

        var regions = _body.ExceptionRegions;
        var clauses = new ExceptionHandlingClause[regions.Length];
        var declaringArguments = _declaringTypeArguments ?? _declaringType.GetGenericArguments();

        for (var i = 0; i < regions.Length; i++)
        {
            var region = regions[i];
            var options = region.Kind switch
            {
                ExceptionRegionKind.Catch => ExceptionHandlingClauseOptions.Clause,
                ExceptionRegionKind.Filter => ExceptionHandlingClauseOptions.Filter,
                ExceptionRegionKind.Finally => ExceptionHandlingClauseOptions.Finally,
                ExceptionRegionKind.Fault => ExceptionHandlingClauseOptions.Fault,
                _ => throw new NotSupportedException($"Unsupported exception region kind '{region.Kind}'."),
            };

            Type? catchType = null;
            if (!region.CatchType.IsNil)
            {
                catchType = _module.ResolveType(
                    region.CatchType,
                    _declaringType,
                    _methodTypeArguments,
                    declaringArguments);
            }

            clauses[i] = new MetadataExceptionHandlingClause(
                options,
                region.TryOffset,
                region.TryLength,
                region.HandlerOffset,
                region.HandlerLength,
                catchType,
                region.FilterOffset);
        }

        return clauses;
    }
}

internal sealed class MetadataLocalVariableInfo : LocalVariableInfo
{
    public MetadataLocalVariableInfo(Type type, bool isPinned, int index)
    {
        LocalType = type ?? throw new ArgumentNullException(nameof(type));
        IsPinned = isPinned;
        LocalIndex = index;
    }

    public override Type LocalType { get; }

    public override bool IsPinned { get; }

    public override int LocalIndex { get; }
}

internal sealed class MetadataExceptionHandlingClause : ExceptionHandlingClause
{
    public MetadataExceptionHandlingClause(
        ExceptionHandlingClauseOptions options,
        int tryOffset,
        int tryLength,
        int handlerOffset,
        int handlerLength,
        Type? catchType,
        int filterOffset)
    {
        Flags = options;
        TryOffset = tryOffset;
        TryLength = tryLength;
        HandlerOffset = handlerOffset;
        HandlerLength = handlerLength;
        CatchType = catchType;
        FilterOffset = filterOffset;
    }

    public override ExceptionHandlingClauseOptions Flags { get; }

    public override int TryOffset { get; }

    public override int TryLength { get; }

    public override int HandlerOffset { get; }

    public override int HandlerLength { get; }

    public override Type? CatchType { get; }

    public override int FilterOffset { get; }
}

internal sealed class MetadataPinnedTrackingTypeProvider : MetadataSignatureTypeProvider
{
    private readonly bool[] _pinnedLocals;
    private int _currentLocalIndex = -1;

    public MetadataPinnedTrackingTypeProvider(
        MetadataModule module,
        IReadOnlyList<Type>? genericTypeParameters,
        IReadOnlyList<Type>? genericMethodParameters,
        MetadataType declaringType,
        int localCount)
        : base(module, genericTypeParameters, genericMethodParameters, declaringType)
    {
        _pinnedLocals = new bool[localCount];
    }

    public void SetCurrentLocalIndex(int index)
    {
        _currentLocalIndex = index;
    }

    public bool IsPinned(int index)
        => index >= 0 && index < _pinnedLocals.Length && _pinnedLocals[index];

    public override Type GetPinnedType(Type elementType)
    {
        if (_currentLocalIndex >= 0 && _currentLocalIndex < _pinnedLocals.Length)
        {
            _pinnedLocals[_currentLocalIndex] = true;
        }

        return base.GetPinnedType(elementType);
    }
}
