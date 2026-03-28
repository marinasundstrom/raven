using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;

using Raven.CodeAnalysis.Symbols;
using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis;

internal static partial class SynthesizedMethodBodyFactory
{
    private static BoundBlockStatement CreateRecordToStringBody(
        Compilation compilation,
        IMethodSymbol method,
        SourceNamedTypeSymbol recordType)
    {
        var statements = new List<BoundStatement>();
        var parts = new List<BoundExpression>();

        if (recordType.Arity > 0)
            parts.Add(CreateInlineUnionDisplayName(compilation, method, recordType.Name, statements));
        else
            parts.Add(CreateStringLiteral(compilation, recordType.Name));

        parts.Add(CreateStringLiteral(compilation, " { "));
        AppendFormattedMemberList(
            compilation,
            parts,
            CreateNominalDisplayMembers(compilation, method, recordType));
        parts.Add(CreateStringLiteral(compilation, " }"));
        statements.Add(new BoundReturnStatement(ConcatSequence(compilation, parts)));
        return new BoundBlockStatement(statements);
    }

    private static BoundBlockStatement CreateRecordGetHashCodeBody(
        Compilation compilation,
        IMethodSymbol method,
        SourceNamedTypeSymbol recordType)
    {
        var hashCodeType = compilation.GetTypeByMetadataName("System.HashCode")
            ?? throw new InvalidOperationException("Failed to resolve System.HashCode.");
        var addMethod = hashCodeType.GetMembers(nameof(HashCode.Add))
            .OfType<IMethodSymbol>()
            .First(methodSymbol => methodSymbol.IsGenericMethod && methodSymbol.Parameters.Length == 1);
        var toHashCodeMethod = hashCodeType.GetMembers(nameof(HashCode.ToHashCode))
            .OfType<IMethodSymbol>()
            .First(methodSymbol => methodSymbol.Parameters.Length == 0 &&
                                   methodSymbol.ReturnType.SpecialType == SpecialType.System_Int32);

        var statements = new List<BoundStatement>();
        var hashLocal = CreateSynthesizedLocal(method, hashCodeType, "hash");

        statements.Add(new BoundLocalDeclarationStatement([
            new BoundVariableDeclarator(hashLocal, new BoundDefaultValueExpression(hashCodeType))
        ]));

        foreach (var (property, _, propertyValue) in CreateSelfNominalValueAccesses(method, recordType))
        {
            statements.Add(CreateExpressionStatement(
                new BoundInvocationExpression(
                    addMethod.Construct(property.Type),
                    [propertyValue],
                    new BoundLocalAccess(hashLocal),
                    requiresReceiverAddress: true)));
        }

        statements.Add(new BoundReturnStatement(
            new BoundInvocationExpression(
                toHashCodeMethod,
                Array.Empty<BoundExpression>(),
                new BoundLocalAccess(hashLocal),
                requiresReceiverAddress: true)));
        return new BoundBlockStatement(statements);
    }

    private static BoundBlockStatement CreateRecordTypedEqualsBody(
        Compilation compilation,
        IMethodSymbol method,
        SourceNamedTypeSymbol recordType)
    {
        var otherAccess = new BoundParameterAccess(method.Parameters[0]);
        var selfAccess = new BoundSelfExpression(method.ContainingType!);
        var statements = CreateRecordReferenceEqualityPrelude(compilation, selfAccess, otherAccess, whenReferenceEqual: true);
        statements.Add(new BoundReturnStatement(CreateNominalValueEqualityExpression(compilation, method, recordType, selfAccess, otherAccess)));

        return new BoundBlockStatement(statements);
    }

    private static BoundBlockStatement CreateRecordObjectEqualsBody(
        Compilation compilation,
        IMethodSymbol method,
        SourceNamedTypeSymbol recordType)
    {
        var factory = new BoundNodeFactory(compilation);
        var otherLocal = CreateSynthesizedLocal(method, method.ContainingType!, "other");
        var objectParameter = new BoundParameterAccess(method.Parameters[0]);
        var selfAccess = new BoundSelfExpression(method.ContainingType!);
        var asConversion = compilation.ClassifyConversion(method.Parameters[0].Type, method.ContainingType!, includeUserDefined: false);
        var otherAccess = new BoundLocalAccess(otherLocal);

        var statements = new List<BoundStatement>
        {
            new BoundLocalDeclarationStatement([
                new BoundVariableDeclarator(otherLocal, factory.CreateAsExpression(objectParameter, method.ContainingType!, asConversion))
            ])
        };
        statements.AddRange(CreateRecordReferenceEqualityPrelude(compilation, selfAccess, otherAccess, whenReferenceEqual: true));
        statements.Add(new BoundReturnStatement(CreateNominalValueEqualityExpression(compilation, method, recordType, selfAccess, otherAccess)));

        return new BoundBlockStatement(statements);
    }

    private static BoundBlockStatement CreateRecordEqualityOperatorBody(
        Compilation compilation,
        IMethodSymbol method,
        SourceNamedTypeSymbol recordType)
    {
        var left = new BoundParameterAccess(method.Parameters[0]);
        var right = new BoundParameterAccess(method.Parameters[1]);
        var statements = new List<BoundStatement>();

        if (!recordType.IsValueType)
            statements.AddRange(CreateRecordReferenceEqualityPrelude(compilation, left, right, whenReferenceEqual: true));

        statements.Add(new BoundReturnStatement(CreateNominalValueEqualityExpression(compilation, method, recordType, left, right)));
        return new BoundBlockStatement(statements);
    }

    private static BoundBlockStatement CreateRecordInequalityOperatorBody(
        Compilation compilation,
        IMethodSymbol method,
        SourceNamedTypeSymbol recordType)
    {
        var left = new BoundParameterAccess(method.Parameters[0]);
        var right = new BoundParameterAccess(method.Parameters[1]);
        var equality = CreateNominalValueEqualityExpression(compilation, method, recordType, left, right);
        var statements = new List<BoundStatement>();

        if (!recordType.IsValueType)
            statements.AddRange(CreateRecordReferenceEqualityPrelude(compilation, left, right, whenReferenceEqual: false, whenNull: true));

        statements.Add(new BoundReturnStatement(CreateLogicalNotExpression(compilation, equality)));
        return new BoundBlockStatement(statements);
    }

    private static BoundExpression FormatNonGenericNominalValue(Compilation compilation, BoundExpression value)
    {
        var valueType = value.Type.GetPlainType();

        if (valueType.SpecialType == SpecialType.System_String)
            return CreateQuotedStringValue(compilation, value, quote: "\"");

        if (valueType.SpecialType == SpecialType.System_Char)
            return CreateQuotedStringValue(compilation, CreateObjectToStringInvocation(compilation, value), quote: "'");

        return CreateObjectToStringInvocation(compilation, value);
    }

    private static List<(string? Name, BoundExpression Value)> CreateNominalDisplayMembers(
        Compilation compilation,
        IMethodSymbol method,
        SourceNamedTypeSymbol recordType)
    {
        var members = new List<(string? Name, BoundExpression Value)>();
        foreach (var (_, property, propertyValue) in CreateSelfNominalValueAccesses(method, recordType))
            members.Add((property.Name, FormatNonGenericNominalValue(compilation, propertyValue)));

        return members;
    }

    private static BoundExpression CreateNominalValueEqualityExpression(
        Compilation compilation,
        IMethodSymbol method,
        SourceNamedTypeSymbol recordType,
        BoundExpression leftReceiver,
        BoundExpression rightReceiver)
    {
        BoundExpression? current = null;

        foreach (var (property, left, right) in CreateResolvedNominalEqualityOperands(method.ContainingType, recordType, leftReceiver, rightReceiver))
        {
            var equals = CreateEqualityComparerEqualsInvocation(compilation, property.Type, left, right);

            current = current is null
                ? equals
                : CreateBinaryExpression(compilation, SyntaxKind.AmpersandAmpersandToken, current, equals);
        }

        return current ?? CreateBoolLiteral(compilation, true);
    }

    private static List<(SourcePropertySymbol Source, IPropertySymbol Resolved, BoundExpression Value)> CreateResolvedNominalValueAccesses(
        INamedTypeSymbol? containingType,
        SourceNamedTypeSymbol recordType,
        BoundExpression receiver)
    {
        return CreateResolvedNominalValueProjection(
            containingType,
            recordType,
            (source, resolved) => (source, resolved, CreatePropertyGetterAccess(receiver, resolved)));
    }

    private static List<(SourcePropertySymbol Source, IPropertySymbol Resolved, BoundExpression Value)> CreateSelfNominalValueAccesses(
        IMethodSymbol method,
        SourceNamedTypeSymbol recordType)
    {
        return CreateResolvedNominalValueAccesses(
            method.ContainingType,
            recordType,
            new BoundSelfExpression(method.ContainingType!));
    }

    private static List<(SourcePropertySymbol Source, BoundExpression Left, BoundExpression Right)> CreateResolvedNominalEqualityOperands(
        INamedTypeSymbol? containingType,
        SourceNamedTypeSymbol recordType,
        BoundExpression leftReceiver,
        BoundExpression rightReceiver)
    {
        return CreateResolvedNominalValueProjection(
            containingType,
            recordType,
            (source, resolved) => (
                source,
                CreatePropertyGetterAccess(leftReceiver, resolved),
                CreatePropertyGetterAccess(rightReceiver, resolved)));
    }

    private static List<(SourcePropertySymbol Source, IPropertySymbol Resolved)> GetResolvedNominalValueProperties(
        INamedTypeSymbol? containingType,
        SourceNamedTypeSymbol recordType)
    {
        return CreateResolvedNominalValueProjection(
            containingType,
            recordType,
            static (source, resolved) => (source, resolved));
    }

    private static List<TResult> CreateResolvedNominalValueProjection<TResult>(
        INamedTypeSymbol? containingType,
        SourceNamedTypeSymbol recordType,
        Func<SourcePropertySymbol, IPropertySymbol, TResult> projection)
    {
        var results = new List<TResult>();

        foreach (var property in GetNominalValueProperties(recordType))
        {
            var resolvedProperty = ResolveNominalDeconstructProperty(containingType, property);
            if (resolvedProperty?.GetMethod is null)
                continue;

            results.Add(projection(property, resolvedProperty));
        }

        return results;
    }

    private static BoundExpression CreateEqualityComparerEqualsInvocation(
        Compilation compilation,
        ITypeSymbol valueType,
        BoundExpression left,
        BoundExpression right)
    {
        var comparerDefinition = compilation.GetTypeByMetadataName("System.Collections.Generic.EqualityComparer`1")
            ?? throw new InvalidOperationException("Failed to resolve EqualityComparer<T>.");
        var comparerType = (INamedTypeSymbol)comparerDefinition.Construct(valueType);
        var defaultGetter = ResolvePropertyGetter(comparerType, "Default");
        var equalsMethod = ResolveMethod(comparerType, nameof(Equals), [valueType, valueType]);
        var comparerInstance = new BoundInvocationExpression(defaultGetter, Array.Empty<BoundExpression>());
        return new BoundInvocationExpression(equalsMethod, [left, right], comparerInstance);
    }

    private static BoundExpression CreateReferenceEqualsInvocation(
        Compilation compilation,
        BoundExpression left,
        BoundExpression right)
    {
        var objectType = compilation.GetSpecialType(SpecialType.System_Object)
            ?? throw new InvalidOperationException("Failed to resolve System.Object.");
        var referenceEquals = objectType.GetMembers(nameof(object.ReferenceEquals)).OfType<IMethodSymbol>()
            .SingleOrDefault(method => method.IsStatic && method.Parameters.Length == 2)
            ?? throw new InvalidOperationException("Failed to resolve System.Object.ReferenceEquals(object, object).");
        return new BoundInvocationExpression(referenceEquals, [left, right]);
    }

    private static BoundIfStatement CreateNullGuardReturn(
        Compilation compilation,
        BoundExpression value,
        bool whenNull)
    {
        return new BoundIfStatement(
            CreateBinaryExpression(compilation, SyntaxKind.EqualsEqualsToken, value, CreateNullLiteral(compilation)),
            new BoundReturnStatement(CreateBoolLiteral(compilation, whenNull)));
    }

    private static BoundIfStatement CreateReferenceEqualsGuardReturn(
        Compilation compilation,
        BoundExpression left,
        BoundExpression right,
        bool whenEqual)
    {
        return new BoundIfStatement(
            CreateReferenceEqualsInvocation(compilation, left, right),
            new BoundReturnStatement(CreateBoolLiteral(compilation, whenEqual)));
    }

    private static List<BoundStatement> CreateRecordReferenceEqualityPrelude(
        Compilation compilation,
        BoundExpression left,
        BoundExpression right,
        bool whenReferenceEqual,
        bool whenNull = false)
    {
        return
        [
            CreateReferenceEqualsGuardReturn(compilation, left, right, whenReferenceEqual),
            CreateNullGuardReturn(compilation, left, whenNull),
            CreateNullGuardReturn(compilation, right, whenNull)
        ];
    }

    private static BoundBlockStatement CreateRecordCopyConstructorBody(
        Compilation compilation,
        IMethodSymbol method,
        SourceNamedTypeSymbol recordType)
    {
        var statements = new List<BoundStatement>();
        var unitType = compilation.GetSpecialType(SpecialType.System_Unit)
            ?? throw new InvalidOperationException("Failed to resolve System.Unit.");
        var source = new BoundParameterAccess(method.Parameters[0]);

        foreach (var (property, resolvedProperty) in GetResolvedNominalValueProperties(method.ContainingType, recordType)
                     .Where(entry => SymbolEqualityComparer.Default.Equals(entry.Source.ContainingType, recordType)))
        {
            if (property.BackingField is not SourceFieldSymbol backingField)
                continue;

            var assignment = new BoundFieldAssignmentExpression(
                new BoundSelfExpression(method.ContainingType!),
                backingField,
                CreatePropertyGetterAccess(source, resolvedProperty),
                unitType);
            statements.Add(new BoundAssignmentStatement(assignment));
        }

        return WithImplicitUnitReturn(statements);
    }

    private static BoundBlockStatement CreateNominalDeconstructBody(
        Compilation compilation,
        IMethodSymbol method,
        ImmutableArray<SourcePropertySymbol> deconstructProperties)
    {
        var parameterCount = Math.Min(deconstructProperties.Length, method.Parameters.Length);
        var sourceValues = new List<BoundExpression>(parameterCount);

        for (var index = 0; index < parameterCount; index++)
        {
            var property = ResolveNominalDeconstructProperty(method.ContainingType, deconstructProperties[index]);
            if (property?.GetMethod is null)
                continue;

            sourceValues.Add(CreateSelfPropertyGetterAccess(method, property));
        }

        return CreateDeconstructBody(compilation, method, sourceValues);
    }

    private static SourceNamedTypeSymbol? TryGetSourceNamedTypeDefinition(INamedTypeSymbol? typeSymbol)
    {
        return typeSymbol switch
        {
            SourceNamedTypeSymbol sourceType => sourceType,
            ConstructedNamedTypeSymbol { OriginalDefinition: SourceNamedTypeSymbol sourceType } => sourceType,
            _ => null
        };
    }

    private static IPropertySymbol? ResolveNominalDeconstructProperty(INamedTypeSymbol? containingType, SourcePropertySymbol property)
    {
        if (containingType is null)
            return property;

        return containingType.GetMembers(property.Name)
            .OfType<IPropertySymbol>()
            .FirstOrDefault(candidate =>
                SymbolEqualityComparer.Default.Equals(candidate.OriginalDefinition ?? candidate, property))
            ?? property;
    }

    private static ImmutableArray<SourcePropertySymbol> GetNominalDeconstructProperties(SourceNamedTypeSymbol typeSymbol)
    {
        if (!typeSymbol.DeconstructProperties.IsDefaultOrEmpty)
            return typeSymbol.DeconstructProperties;

        if (!typeSymbol.IsRecord)
            return ImmutableArray<SourcePropertySymbol>.Empty;

        return GetNominalValueProperties(typeSymbol).ToImmutableArray();
    }

    private static SourcePropertySymbol[] GetNominalValueProperties(SourceNamedTypeSymbol typeSymbol)
    {
        if (!typeSymbol.IsRecord)
            return [];

        return typeSymbol.RecordProperties
            .Where(static property => property.DeclaredAccessibility == Accessibility.Public)
            .ToArray();
    }
}
