using System.Collections.Immutable;
using System.Linq;

using Raven.CodeAnalysis;

namespace Raven.CodeAnalysis.Symbols;

internal partial class SourceFieldSymbol : SourceSymbol, IFieldSymbol
{
    private readonly object _constantValue;
    private readonly bool _isMutable;
    private readonly bool _isStatic;
    private SourcePropertySymbol? _associatedProperty;
    private SourceEventSymbol? _associatedEvent;
    private ImmutableArray<AttributeData> _lazyAttributesWithSynthesized;

    public SourceFieldSymbol(string name, ITypeSymbol fieldType, bool isStatic, bool isMutable, bool isConst, object constantValue, ISymbol containingSymbol, INamedTypeSymbol? containingType, INamespaceSymbol? containingNamespace, Location[] locations, SyntaxReference[] declaringSyntaxReferences, BoundExpression? initializer = null, Accessibility declaredAccessibility = Accessibility.NotApplicable)
        : base(SymbolKind.Field, name, containingSymbol, containingType, containingNamespace, locations, declaringSyntaxReferences, declaredAccessibility)
    {
        Type = fieldType;
        _isMutable = isMutable;
        _isStatic = isStatic;
        IsConst = isConst;
        _constantValue = constantValue;
        Initializer = initializer;
    }

    public ITypeSymbol Type { get; }

    public bool IsConst { get; }

    public bool IsMutable => _isMutable;

    public BoundExpression? Initializer { get; }

    internal bool IsAutoPropertyBackingField => _associatedProperty is not null || _associatedEvent is not null;

    public object? GetConstantValue()
    {
        return _constantValue;
    }

    public override bool IsStatic => _isStatic;

    public override ImmutableArray<AttributeData> GetAttributes()
    {
        if (!IsAutoPropertyBackingField)
            return base.GetAttributes();

        if (_lazyAttributesWithSynthesized.IsDefault)
        {
            var attributes = base.GetAttributes();
            var builder = ImmutableArray.CreateBuilder<AttributeData>();

            var compilerGenerated = CreateCompilerGeneratedAttribute();
            if (compilerGenerated is not null)
                builder.Add(compilerGenerated);

            var debuggerBrowsable = CreateDebuggerBrowsableAttribute();
            if (debuggerBrowsable is not null)
                builder.Add(debuggerBrowsable);

            _lazyAttributesWithSynthesized = builder.Count == 0
                ? attributes
                : attributes.AddRange(builder.ToImmutable());
        }

        return _lazyAttributesWithSynthesized;
    }

    internal void SetAssociatedProperty(SourcePropertySymbol property)
    {
        _associatedProperty = property;
    }

    internal void SetAssociatedEvent(SourceEventSymbol @event)
    {
        _associatedEvent = @event;
    }

    private AttributeData? CreateDebuggerBrowsableAttribute()
    {
        var compilation = GetDeclaringCompilation();
        if (compilation is null)
            return null;

        var syntaxReference = DeclaringSyntaxReferences.FirstOrDefault();
        if (syntaxReference is null)
            return null;

        var attributeType = compilation.GetTypeByMetadataName("System.Diagnostics.DebuggerBrowsableAttribute");
        var stateType = compilation.GetTypeByMetadataName("System.Diagnostics.DebuggerBrowsableState");

        if (attributeType is not INamedTypeSymbol namedAttributeType || stateType is null)
            return null;

        var constructor = namedAttributeType.Constructors.FirstOrDefault(c => !c.IsStatic && c.Parameters.Length == 1);
        if (constructor is null)
            return null;

        var stateNever = TypedConstant.CreatePrimitive(stateType, 0);

        return new AttributeData(
            namedAttributeType,
            constructor,
            ImmutableArray.Create(stateNever),
            ImmutableArray<KeyValuePair<string, TypedConstant>>.Empty,
            syntaxReference);
    }
}
