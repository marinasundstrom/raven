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
        MethodSymbol method,
        SourceNamedTypeSymbol recordType)
    {
        var builderType = compilation.GetTypeByMetadataName("System.Text.StringBuilder")
            ?? throw new MissingSynthesizedRuntimeMemberException("System.Text.StringBuilder", 0);
        var builder = CreateSynthesizedLocal(method, builderType, "builder");
        var builderAccess = new BoundLocalAccess(builder);
        var constructor = builderType.Constructors.Single(candidate => !candidate.IsStatic && candidate.Parameters.IsEmpty);
        var printMembers = method.ContainingType!.GetMembers("PrintMembers").OfType<MethodSymbol>()
            .Single(candidate => !candidate.IsStatic && candidate.Parameters.Length == 1 &&
                SymbolEqualityComparer.Default.Equals(candidate.Parameters[0].Type, builderType));
        var toString = builderType.GetMembers(nameof(object.ToString)).OfType<MethodSymbol>()
            .Single(candidate => !candidate.IsStatic && candidate.Parameters.IsEmpty);
        return new BoundBlockStatement([
            new BoundLocalDeclarationStatement([new BoundVariableDeclarator(builder, new BoundObjectCreationExpression(constructor, []))]),
            CreateRecordBuilderAppend(compilation, builderAccess, CreateStringLiteral(compilation, recordType.Name + " { ")),
            new BoundIfStatement(new BoundInvocationExpression(printMembers, [builderAccess], new BoundSelfExpression(method.ContainingType)),
                CreateRecordBuilderAppend(compilation, builderAccess, CreateStringLiteral(compilation, " "))),
            CreateRecordBuilderAppend(compilation, builderAccess, CreateStringLiteral(compilation, "}")),
            new BoundReturnStatement(new BoundInvocationExpression(toString, [], builderAccess))
        ]);
    }

    private static BoundStatement CreateRecordBuilderAppend(Compilation compilation, BoundExpression builder, BoundExpression value)
    {
        var objectType = compilation.GetSpecialType(SpecialType.System_Object);
        var append = builder.Type!.GetMembers("Append").OfType<MethodSymbol>()
            .Single(candidate => !candidate.IsStatic && candidate.Parameters.Length == 1 &&
                candidate.Parameters[0].Type.GetNonNullableType().SpecialType == SpecialType.System_Object);
        return CreateExpressionStatement(new BoundInvocationExpression(append,
            [CreateConversion(compilation, value, objectType)], builder));
    }

    private static BoundBlockStatement CreateRecordPrintMembersBody(
        Compilation compilation, MethodSymbol method, SourceNamedTypeSymbol recordType)
    {
        var statements = new List<BoundStatement>();
        var builder = new BoundParameterAccess(method.Parameters[0]);
        var hasMembers = CreateSynthesizedLocal(method, compilation.GetSpecialType(SpecialType.System_Boolean), "hasMembers");
        var hasMembersAccess = new BoundLocalAccess(hasMembers);
        var basePrint = recordType.IsValueType ? null : method.ContainingType!.BaseType?.GetMembers("PrintMembers")
            .OfType<MethodSymbol>().FirstOrDefault(candidate => !candidate.IsStatic && candidate.Parameters.Length == 1 &&
                SymbolEqualityComparer.Default.Equals(candidate.Parameters[0].Type, method.Parameters[0].Type));
        if (basePrint is null &&
            compilation.GetTypeByMetadataName("System.Runtime.CompilerServices.RuntimeHelpers")?.GetMembers("EnsureSufficientExecutionStack")
                .OfType<MethodSymbol>().FirstOrDefault(candidate => candidate.IsStatic && candidate.Parameters.IsEmpty) is { } ensureStack)
        {
            statements.Add(CreateExpressionStatement(new BoundInvocationExpression(ensureStack, [])));
        }
        BoundExpression initial = basePrint is null ? CreateBoolLiteral(compilation, false)
            : new BoundInvocationExpression(basePrint, [builder], new BoundBaseExpression(method.ContainingType!.BaseType!));
        statements.Add(new BoundLocalDeclarationStatement([new BoundVariableDeclarator(hasMembers, initial)]));
        foreach (var member in recordType.GetDeclaredMembersWithoutEnsuring())
        {
            if (member.IsStatic || member.DeclaredAccessibility != Accessibility.Public)
                continue;
            BoundExpression value;
            switch (member)
            {
                case PropertySymbol property when property.GetMethod?.DeclaredAccessibility == Accessibility.Public && property.Parameters.IsEmpty:
                    var resolved = method.ContainingType!.GetMembers(property.Name).OfType<PropertySymbol>().First();
                    value = CreatePropertyGetterAccess(new BoundSelfExpression(method.ContainingType!), resolved);
                    break;
                case FieldSymbol { IsConst: false } field:
                    var resolvedField = method.ContainingType!.GetMembers(field.Name).OfType<FieldSymbol>().First();
                    value = new BoundFieldAccess(new BoundSelfExpression(method.ContainingType!), resolvedField);
                    break;
                default:
                    continue;
            }
            statements.Add(new BoundIfStatement(hasMembersAccess,
                CreateRecordBuilderAppend(compilation, builder, CreateStringLiteral(compilation, ", "))));
            statements.Add(CreateRecordBuilderAppend(compilation, builder, CreateStringLiteral(compilation, member.Name + " = ")));
            statements.Add(CreateRecordBuilderAppend(compilation, builder, value));
            statements.Add(new BoundAssignmentStatement(new BoundLocalAssignmentExpression(hasMembers, hasMembersAccess, CreateBoolLiteral(compilation, true), compilation.GetSpecialType(SpecialType.System_Unit))));
        }
        statements.Add(new BoundReturnStatement(hasMembersAccess));
        return new BoundBlockStatement(statements);
    }

    private static BoundBlockStatement CreateRecordGetHashCodeBody(
        Compilation compilation,
        MethodSymbol method,
        SourceNamedTypeSymbol recordType)
    {
        var hashCodeType = compilation.GetTypeByMetadataName("System.HashCode")
            ?? throw new InvalidOperationException("Failed to resolve System.HashCode.");
        var addMethod = hashCodeType.GetMembers(nameof(HashCode.Add))
            .OfType<MethodSymbol>()
            .First(methodSymbol => methodSymbol.IsGenericMethod && methodSymbol.Parameters.Length == 1);
        var toHashCodeMethod = hashCodeType.GetMembers(nameof(HashCode.ToHashCode))
            .OfType<MethodSymbol>()
            .First(methodSymbol => methodSymbol.Parameters.Length == 0 &&
                                   methodSymbol.ReturnType.SpecialType == SpecialType.System_Int32);

        var statements = new List<BoundStatement>();
        var hashLocal = CreateSynthesizedLocal(method, hashCodeType, "hash");

        statements.Add(new BoundLocalDeclarationStatement([
            new BoundVariableDeclarator(hashLocal, new BoundDefaultValueExpression(hashCodeType))
        ]));

        if (GetRecordBaseMethod(method, recordType, nameof(object.GetHashCode), 0) is { } baseHash)
        {
            statements.Add(CreateExpressionStatement(new BoundInvocationExpression(
                addMethod.Construct(compilation.GetSpecialType(SpecialType.System_Int32)),
                [new BoundInvocationExpression(baseHash, [], new BoundBaseExpression(method.ContainingType!.BaseType!))],
                new BoundLocalAccess(hashLocal), requiresReceiverAddress: true)));
        }

        foreach (var field in GetRecordStorageFields(method.ContainingType, recordType))
        {
            statements.Add(CreateExpressionStatement(new BoundInvocationExpression(
                addMethod.Construct(field.Type),
                [new BoundFieldAccess(new BoundSelfExpression(method.ContainingType!), field)],
                new BoundLocalAccess(hashLocal), requiresReceiverAddress: true)));
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
        MethodSymbol method,
        SourceNamedTypeSymbol recordType)
    {
        var otherAccess = new BoundParameterAccess(method.Parameters[0]);
        var selfAccess = new BoundSelfExpression(method.ContainingType!);
        var statements = recordType.IsValueType
            ? new List<BoundStatement>()
            : CreateRecordReferenceEqualityPrelude(compilation, selfAccess, otherAccess, whenReferenceEqual: true);
        statements.Add(new BoundReturnStatement(recordType.IsValueType
            ? CreateNominalValueEqualityExpression(compilation, method, recordType, selfAccess, otherAccess)
            : CreateRecordVirtualEqualsInvocation(compilation, selfAccess, otherAccess)));

        return new BoundBlockStatement(statements);
    }

    private static BoundBlockStatement CreateRecordObjectEqualsBody(
        Compilation compilation,
        MethodSymbol method,
        SourceNamedTypeSymbol recordType)
    {
        var factory = new BoundNodeFactory(compilation);
        var invocationContainingType = GetInvocationContainingType(method);
        var otherLocal = CreateSynthesizedLocal(method, invocationContainingType, "other");
        var objectParameter = new BoundParameterAccess(method.Parameters[0]);
        var selfAccess = new BoundSelfExpression(invocationContainingType);
        if (recordType.IsValueType)
        {
            var typedPattern = new BoundDeclarationPattern(invocationContainingType, new BoundSingleVariableDesignator(otherLocal));
            var matches = new BoundIsPatternExpression(objectParameter, typedPattern,
                compilation.GetSpecialType(SpecialType.System_Boolean));
            return new BoundBlockStatement([
                new BoundIfStatement(matches,
                    new BoundReturnStatement(CreateNominalValueEqualityExpression(compilation, method, recordType,
                        selfAccess, new BoundLocalAccess(otherLocal)))),
                new BoundReturnStatement(CreateBoolLiteral(compilation, false))
            ]);
        }
        var asConversion = compilation.ClassifyConversion(method.Parameters[0].Type, invocationContainingType, includeUserDefined: false);
        var otherAccess = new BoundLocalAccess(otherLocal);

        var statements = new List<BoundStatement>
        {
            new BoundLocalDeclarationStatement([
                new BoundVariableDeclarator(otherLocal, factory.CreateAsExpression(objectParameter, invocationContainingType, asConversion))
            ])
        };
        statements.AddRange(CreateRecordReferenceEqualityPrelude(compilation, selfAccess, otherAccess, whenReferenceEqual: true));
        var getType = compilation.GetSpecialType(SpecialType.System_Object).GetMembers(nameof(object.GetType))
            .OfType<MethodSymbol>().Single(m => !m.IsStatic && m.Parameters.IsEmpty);
        var sameType = CreateRecordVirtualEqualsInvocation(compilation,
            new BoundInvocationExpression(getType, [], selfAccess),
            new BoundInvocationExpression(getType, [], otherAccess));
        statements.Add(new BoundIfStatement(CreateLogicalNotExpression(compilation, sameType),
            new BoundReturnStatement(CreateBoolLiteral(compilation, false))));
        statements.Add(new BoundReturnStatement(CreateNominalValueEqualityExpression(compilation, method, recordType, selfAccess, otherAccess)));

        return new BoundBlockStatement(statements);
    }

    private static BoundBlockStatement CreateRecordEqualityOperatorBody(
        Compilation compilation,
        MethodSymbol method,
        SourceNamedTypeSymbol recordType)
    {
        var left = new BoundParameterAccess(method.Parameters[0]);
        var right = new BoundParameterAccess(method.Parameters[1]);
        var statements = new List<BoundStatement>();

        if (!recordType.IsValueType)
            statements.AddRange(CreateRecordReferenceEqualityPrelude(compilation, left, right, whenReferenceEqual: true));

        statements.Add(new BoundReturnStatement(recordType.IsValueType
            ? CreateNominalValueEqualityExpression(compilation, method, recordType, left, right)
            : CreateRecordVirtualEqualsInvocation(compilation, left, right)));
        return new BoundBlockStatement(statements);
    }

    private static BoundBlockStatement CreateRecordInequalityOperatorBody(
        Compilation compilation,
        MethodSymbol method,
        SourceNamedTypeSymbol recordType)
    {
        var left = new BoundParameterAccess(method.Parameters[0]);
        var right = new BoundParameterAccess(method.Parameters[1]);
        var equality = recordType.IsValueType
            ? CreateNominalValueEqualityExpression(compilation, method, recordType, left, right)
            : CreateRecordVirtualEqualsInvocation(compilation, left, right);
        var statements = new List<BoundStatement>();

        if (!recordType.IsValueType)
            statements.AddRange(CreateRecordReferenceEqualityPrelude(compilation, left, right, whenReferenceEqual: false, whenNull: true));

        statements.Add(new BoundReturnStatement(CreateLogicalNotExpression(compilation, equality)));
        return new BoundBlockStatement(statements);
    }

    private static BoundExpression CreateRecordVirtualEqualsInvocation(
        Compilation compilation, BoundExpression left, BoundExpression right)
    {
        var objectType = compilation.GetSpecialType(SpecialType.System_Object);
        var equals = objectType.GetMembers(nameof(object.Equals)).OfType<MethodSymbol>()
            .Single(m => !m.IsStatic && m.Parameters.Length == 1);
        return new BoundInvocationExpression(equals,
            [CreateConversion(compilation, right, equals.Parameters[0].Type)],
            CreateConversion(compilation, left, objectType));
    }

    private static BoundExpression CreateNominalValueEqualityExpression(
        Compilation compilation,
        MethodSymbol method,
        SourceNamedTypeSymbol recordType,
        BoundExpression leftReceiver,
        BoundExpression rightReceiver)
    {
        BoundExpression? current = GetRecordBaseMethod(method, recordType, nameof(object.Equals), 1) is { } baseEquals
            ? new BoundInvocationExpression(baseEquals, [rightReceiver], new BoundBaseExpression(method.ContainingType!.BaseType!))
            : null;

        foreach (var field in GetRecordStorageFields(method.ContainingType, recordType))
        {
            var equals = CreateEqualityComparerEqualsInvocation(compilation, field.Type,
                new BoundFieldAccess(leftReceiver, field), new BoundFieldAccess(rightReceiver, field));
            current = current is null
                ? equals
                : CreateBinaryExpression(compilation, SyntaxKind.AmpersandAmpersandToken, current, equals);
        }

        return current ?? CreateBoolLiteral(compilation, true);
    }

    private static BoundExpression CreateEqualityComparerEqualsInvocation(
        Compilation compilation,
        TypeSymbol valueType,
        BoundExpression left,
        BoundExpression right)
    {
        var comparerDefinition = compilation.GetTypeByMetadataName("System.Collections.Generic.EqualityComparer`1")
            ?? throw new InvalidOperationException("Failed to resolve EqualityComparer<T>.");
        var comparerType = (NamedTypeSymbol)comparerDefinition.Construct(valueType);
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
        var referenceEquals = objectType.GetMembers(nameof(object.ReferenceEquals)).OfType<MethodSymbol>()
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
            CreateReferenceEqualsInvocation(compilation, value, CreateNullLiteral(compilation)),
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
        MethodSymbol method,
        SourceNamedTypeSymbol recordType)
    {
        var statements = new List<BoundStatement>();
        var unitType = compilation.GetSpecialType(SpecialType.System_Unit)
            ?? throw new InvalidOperationException("Failed to resolve System.Unit.");
        var source = new BoundParameterAccess(method.Parameters[0]);

        foreach (var field in GetRecordStorageFields(method.ContainingType, recordType))
        {
            var assignment = new BoundFieldAssignmentExpression(
                new BoundSelfExpression(method.ContainingType!), field,
                new BoundFieldAccess(source, field), unitType);
            statements.Add(new BoundAssignmentStatement(assignment));
        }

        return WithImplicitUnitReturn(statements);
    }

    private static IEnumerable<FieldSymbol> GetRecordStorageFields(
        NamedTypeSymbol? containingType, SourceNamedTypeSymbol recordType)
    {
        foreach (var field in recordType.GetDeclaredMembersWithoutEnsuring().OfType<FieldSymbol>())
        {
            if (field.IsStatic || field.IsConst)
                continue;
            yield return containingType?.GetMembers(field.Name).OfType<FieldSymbol>().FirstOrDefault() ?? field;
        }
    }

    private static MethodSymbol? GetRecordBaseMethod(
        MethodSymbol method, SourceNamedTypeSymbol recordType, string name, int parameterCount)
    {
        if (recordType.IsValueType || recordType.BaseType?.OriginalDefinition is not SourceNamedTypeSymbol { IsRecord: true })
            return null;
        return method.ContainingType?.BaseType?.GetMembers(name).OfType<MethodSymbol>()
            .FirstOrDefault(candidate => !candidate.IsStatic && candidate.Parameters.Length == parameterCount &&
                (parameterCount == 0 || candidate.Parameters[0].Type.GetNonNullableType().SpecialType == SpecialType.System_Object));
    }

    private static BoundBlockStatement CreateNominalDeconstructBody(
        Compilation compilation,
        MethodSymbol method,
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

    private static SourceNamedTypeSymbol? TryGetSourceNamedTypeDefinition(NamedTypeSymbol? typeSymbol)
    {
        return typeSymbol switch
        {
            SourceNamedTypeSymbol sourceType => sourceType,
            ConstructedNamedTypeSymbol { OriginalDefinition: SourceNamedTypeSymbol sourceType } => sourceType,
            _ => null
        };
    }

    private static PropertySymbol? ResolveNominalDeconstructProperty(NamedTypeSymbol? containingType, SourcePropertySymbol property)
    {
        if (containingType is null)
            return property;

        return containingType.GetMembers(property.Name)
            .OfType<PropertySymbol>()
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

        var properties = new List<SourcePropertySymbol>();
        if (typeSymbol.BaseType?.OriginalDefinition is SourceNamedTypeSymbol { IsRecord: true } baseRecord)
            properties.AddRange(GetNominalValueProperties(baseRecord));
        foreach (var property in typeSymbol.RecordProperties)
        {
            if (!properties.Any(p => SymbolEqualityComparer.Default.Equals(p, property)))
                properties.Add(property);
        }
        return properties.ToArray();
    }
}
