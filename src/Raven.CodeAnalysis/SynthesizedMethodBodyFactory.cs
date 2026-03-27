using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;
using System.Reflection;

using Raven.CodeAnalysis.Symbols;
using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis;

internal static class SynthesizedMethodBodyFactory
{
    public static bool TryCreate(Compilation compilation, IMethodSymbol method, out BoundBlockStatement body)
    {
        if (method.Name == SynthesizedUnionMethodNames.DisplayNameHelper &&
            method.Parameters.Length == 0 &&
            method.ReturnType.SpecialType == SpecialType.System_String)
        {
            switch (method.ContainingType)
            {
                case SourceDiscriminatedUnionSymbol unionSymbol:
                    body = CreateUnionDisplayNameHelperBody(compilation, unionSymbol);
                    return true;

                case SourceDiscriminatedUnionCaseTypeSymbol caseSymbol:
                    body = CreateUnionDisplayNameHelperBody(compilation, method, caseSymbol);
                    return true;
            }
        }

        if (method.Name == nameof(object.ToString) &&
            method.Parameters.Length == 0 &&
            method.ReturnType.SpecialType == SpecialType.System_String)
        {
            switch (method.ContainingType)
            {
                case SourceDiscriminatedUnionSymbol unionSymbol:
                    body = CreateUnionToStringBody(compilation, method, unionSymbol);
                    return true;

                case SourceDiscriminatedUnionCaseTypeSymbol caseSymbol:
                    body = CreateUnionCaseToStringBody(compilation, method, caseSymbol);
                    return true;
            }
        }

        body = null!;
        return false;
    }

    private static BoundBlockStatement CreateUnionDisplayNameHelperBody(
        Compilation compilation,
        SourceDiscriminatedUnionSymbol unionSymbol)
    {
        var parts = new List<BoundExpression>
        {
            CreateStringLiteral(compilation, unionSymbol.Name)
        };

        if (unionSymbol is INamedTypeSymbol namedUnion &&
            !namedUnion.TypeParameters.IsDefaultOrEmpty &&
            namedUnion.TypeParameters.Length > 0)
        {
            parts.Add(CreateStringLiteral(compilation, "<"));

            for (var index = 0; index < namedUnion.TypeParameters.Length; index++)
            {
                if (index > 0)
                    parts.Add(CreateStringLiteral(compilation, ", "));

                parts.Add(CreateFriendlyTypeNameInvocation(compilation, namedUnion, namedUnion.TypeParameters[index]));
            }

            parts.Add(CreateStringLiteral(compilation, ">"));
        }

        return new([new BoundReturnStatement(ConcatSequence(compilation, parts))]);
    }

    private static BoundBlockStatement CreateUnionDisplayNameHelperBody(
        Compilation compilation,
        IMethodSymbol method,
        SourceDiscriminatedUnionCaseTypeSymbol caseSymbol)
    {
        if (caseSymbol.Arity == 0 && caseSymbol.Union.Arity == 0)
        {
            return new([new BoundReturnStatement(ConcatSequence(compilation, [
                CreateStringLiteral(compilation, caseSymbol.Union.Name)
            ]))]);
        }

        var statements = new List<BoundStatement>();
        var displayName = CreateInlineUnionDisplayName(compilation, method, caseSymbol.Union.Name, statements);
        statements.Add(new BoundReturnStatement(displayName));
        return new BoundBlockStatement(statements);
    }

    private static BoundBlockStatement CreateUnionToStringBody(
        Compilation compilation,
        IMethodSymbol method,
        SourceDiscriminatedUnionSymbol unionSymbol)
    {
        var statements = new List<BoundStatement>();

        if (!unionSymbol.CaseTypes.IsDefaultOrEmpty && unionSymbol.CaseTypes.Length > 0)
        {
            foreach (var caseType in unionSymbol.CaseTypes.OfType<SourceDiscriminatedUnionCaseTypeSymbol>())
            {
                var payloadField = (SourceFieldSymbol)UnionFieldUtilities.GetRequiredPayloadField(unionSymbol, caseType);
                var payloadAccess = new BoundFieldAccess(new BoundSelfExpression(method.ContainingType!), payloadField);
                var caseToString = CreateObjectToStringInvocation(compilation, payloadAccess);
                statements.Add(new BoundIfStatement(
                    CreateUnionTagEquality(compilation, method, unionSymbol, caseType.Ordinal),
                    new BoundReturnStatement(caseToString)));
            }

            statements.Add(new BoundReturnStatement(CreateStringLiteral(compilation, "<Uninitialized>")));
            return new BoundBlockStatement(statements);
        }

        var payloadFields = unionSymbol.PayloadFields.OfType<SourceFieldSymbol>().ToArray();
        for (var index = 0; index < payloadFields.Length; index++)
        {
            var payloadField = payloadFields[index];
            var payloadAccess = new BoundFieldAccess(new BoundSelfExpression(method.ContainingType!), payloadField);
            var formatted = FormatUnionValue(compilation, payloadAccess);
            var text = ConcatSequence(
                compilation,
                [
                    InvokeUnionDisplayNameHelper(method),
                    CreateStringLiteral(compilation, "("),
                    formatted,
                    CreateStringLiteral(compilation, ")")
                ]);

            statements.Add(new BoundIfStatement(
                CreateUnionTagEquality(compilation, method, unionSymbol, index),
                new BoundReturnStatement(text)));
        }

        statements.Add(new BoundReturnStatement(CreateStringLiteral(compilation, "<Uninitialized>")));
        return new BoundBlockStatement(statements);
    }

    private static BoundBlockStatement CreateUnionCaseToStringBody(
        Compilation compilation,
        IMethodSymbol method,
        SourceDiscriminatedUnionCaseTypeSymbol caseSymbol)
    {
        var statements = new List<BoundStatement>();
        BoundExpression unionDisplayName;

        if (caseSymbol.Arity > 0 || caseSymbol.Union.Arity > 0)
        {
            unionDisplayName = CreateInlineUnionDisplayName(
                compilation,
                method,
                caseSymbol.Union.Name,
                statements);
        }
        else
        {
            unionDisplayName = InvokeUnionDisplayNameHelper(method);
        }

        var parts = new List<BoundExpression>
        {
            unionDisplayName,
            CreateStringLiteral(compilation, "."),
            CreateStringLiteral(compilation, caseSymbol.Name)
        };

        var parameterInfos = CollectUnionCaseParameters(caseSymbol);
        if (parameterInfos.Count > 0)
        {
            parts.Add(CreateStringLiteral(compilation, "("));

            var includeParameterNames = parameterInfos.Count > 1;
            for (var index = 0; index < parameterInfos.Count; index++)
            {
                var (name, field) = parameterInfos[index];
                if (index > 0)
                    parts.Add(CreateStringLiteral(compilation, ", "));

                if (includeParameterNames)
                {
                    parts.Add(CreateStringLiteral(compilation, name));
                    parts.Add(CreateStringLiteral(compilation, "="));
                }

                var payloadAccess = new BoundFieldAccess(new BoundSelfExpression(method.ContainingType!), field);
                parts.Add(FormatUnionValue(compilation, payloadAccess));
            }

            parts.Add(CreateStringLiteral(compilation, ")"));
        }

        statements.Add(new BoundReturnStatement(ConcatSequence(compilation, parts)));
        return new BoundBlockStatement(statements);
    }

    private static BoundExpression CreateInlineUnionDisplayName(
        Compilation compilation,
        IMethodSymbol method,
        string unionName,
        List<BoundStatement> statements)
    {
        var typeType = compilation.GetSpecialType(SpecialType.System_Type)
            ?? throw new InvalidOperationException("Failed to resolve System.Type.");
        var stringType = compilation.GetSpecialType(SpecialType.System_String)
            ?? throw new InvalidOperationException("Failed to resolve System.String.");
        var intType = compilation.GetSpecialType(SpecialType.System_Int32)
            ?? throw new InvalidOperationException("Failed to resolve System.Int32.");
        var arrayType = compilation.GetTypeByMetadataName("System.Array")
            ?? throw new InvalidOperationException("Failed to resolve System.Array.");
        var memberInfoType = compilation.GetTypeByMetadataName("System.Reflection.MemberInfo")
            ?? throw new InvalidOperationException("Failed to resolve System.Reflection.MemberInfo.");
        var objectType = compilation.GetSpecialType(SpecialType.System_Object)
            ?? throw new InvalidOperationException("Failed to resolve System.Object.");
        var unitType = compilation.GetSpecialType(SpecialType.System_Unit)
            ?? throw new InvalidOperationException("Failed to resolve System.Unit.");
        var typeArrayType = compilation.CreateArrayTypeSymbol(typeType);

        var objectGetType = ResolveMethod(objectType, nameof(object.GetType), []);
        var typeNameGetter = ResolvePropertyGetter(memberInfoType, nameof(MemberInfo.Name));
        var typeGenericArgsGetter = ResolveMethod(typeType, nameof(Type.GetGenericArguments), []);
        var arrayLengthGetter = ResolvePropertyGetter(arrayType, nameof(Array.Length));
        var stringIndexOf = ResolveMethod(stringType, nameof(string.IndexOf), [stringType]);
        var stringSubstring = ResolveMethod(stringType, nameof(string.Substring), [intType, intType]);

        var runtimeTypeLocal = CreateSynthesizedLocal(method, typeType, "runtimeType");
        var typeArgsLocal = CreateSynthesizedLocal(method, typeArrayType, "typeArgs");
        var typeArgsLengthLocal = CreateSynthesizedLocal(method, intType, "typeArgsLength");
        var indexLocal = CreateSynthesizedLocal(method, intType, "typeArgIndex");
        var resultLocal = CreateSynthesizedLocal(method, stringType, "displayName");
        var displayNameLocal = resultLocal;

        statements.Add(new BoundLocalDeclarationStatement([
            new BoundVariableDeclarator(displayNameLocal, CreateStringLiteral(compilation, unionName))
        ]));

        statements.Add(new BoundLocalDeclarationStatement([
            new BoundVariableDeclarator(
                runtimeTypeLocal,
                new BoundTypeOfExpression(method.ContainingType!, typeType))
        ]));

        statements.Add(new BoundLocalDeclarationStatement([
            new BoundVariableDeclarator(
                typeArgsLocal,
                new BoundInvocationExpression(typeGenericArgsGetter, Array.Empty<BoundExpression>(), new BoundLocalAccess(runtimeTypeLocal)))
        ]));

        statements.Add(new BoundLocalDeclarationStatement([
            new BoundVariableDeclarator(
                typeArgsLengthLocal,
                new BoundInvocationExpression(arrayLengthGetter, Array.Empty<BoundExpression>(), new BoundLocalAccess(typeArgsLocal)))
        ]));

        var hasTypeArguments = CreateBinaryExpression(
            compilation,
            SyntaxKind.GreaterThanToken,
            new BoundLocalAccess(typeArgsLengthLocal),
            CreateIntLiteral(compilation, 0));

        var loopStatements = new List<BoundStatement>
        {
            CreateAppendAssignment(compilation, displayNameLocal, CreateStringLiteral(compilation, "<"), unitType),
            new BoundLocalDeclarationStatement([
                new BoundVariableDeclarator(indexLocal, CreateIntLiteral(compilation, 0))
            ])
        };

        var currentTypeLocal = CreateSynthesizedLocal(method, typeType, "currentType");
        var currentNameLocal = CreateSynthesizedLocal(method, stringType, "currentName");
        var tickIndexLocal = CreateSynthesizedLocal(method, intType, "tickIndex");

        var whileBodyStatements = new List<BoundStatement>();

        var needsSeparator = CreateBinaryExpression(
            compilation,
            SyntaxKind.GreaterThanToken,
            new BoundLocalAccess(indexLocal),
            CreateIntLiteral(compilation, 0));
        whileBodyStatements.Add(new BoundIfStatement(
            needsSeparator,
            CreateAppendAssignment(compilation, displayNameLocal, CreateStringLiteral(compilation, ", "), unitType)));

        whileBodyStatements.Add(new BoundLocalDeclarationStatement([
            new BoundVariableDeclarator(
                currentTypeLocal,
                new BoundArrayAccessExpression(
                    new BoundLocalAccess(typeArgsLocal),
                    [new BoundLocalAccess(indexLocal)],
                    typeType))
        ]));

        AppendFriendlyTypeNameStatements(
            compilation,
            method,
            new BoundLocalAccess(currentTypeLocal),
            displayNameLocal,
            currentNameLocal,
            tickIndexLocal,
            typeNameGetter,
            typeGenericArgsGetter,
            arrayLengthGetter,
            stringIndexOf,
            stringSubstring,
            unitType,
            intType,
            typeType,
            statements: whileBodyStatements,
            remainingDepth: 4);

        whileBodyStatements.Add(CreateAssignmentStatement(
            compilation,
            indexLocal,
            CreateBinaryExpression(
                compilation,
                SyntaxKind.PlusToken,
                new BoundLocalAccess(indexLocal),
                CreateIntLiteral(compilation, 1)),
            unitType));

        loopStatements.Add(new BoundWhileStatement(
            CreateBinaryExpression(
                compilation,
                SyntaxKind.LessThanToken,
                new BoundLocalAccess(indexLocal),
                new BoundLocalAccess(typeArgsLengthLocal)),
            new BoundBlockStatement(whileBodyStatements)));

        loopStatements.Add(CreateAppendAssignment(compilation, displayNameLocal, CreateStringLiteral(compilation, ">"), unitType));

        statements.Add(new BoundIfStatement(
            hasTypeArguments,
            new BoundBlockStatement(loopStatements)));

        return new BoundLocalAccess(displayNameLocal);
    }

    private static void AppendFriendlyTypeNameStatements(
        Compilation compilation,
        IMethodSymbol method,
        BoundExpression typeExpression,
        ILocalSymbol displayNameLocal,
        ILocalSymbol nameLocal,
        ILocalSymbol tickIndexLocal,
        IMethodSymbol typeNameGetter,
        IMethodSymbol typeGenericArgsGetter,
        IMethodSymbol arrayLengthGetter,
        IMethodSymbol stringIndexOf,
        IMethodSymbol stringSubstring,
        ITypeSymbol unitType,
        ITypeSymbol intType,
        ITypeSymbol typeType,
        List<BoundStatement> statements,
        int remainingDepth)
    {
        statements.Add(new BoundLocalDeclarationStatement([
            new BoundVariableDeclarator(
                nameLocal,
                new BoundInvocationExpression(typeNameGetter, Array.Empty<BoundExpression>(), typeExpression))
        ]));

        statements.Add(new BoundLocalDeclarationStatement([
            new BoundVariableDeclarator(
                tickIndexLocal,
                new BoundInvocationExpression(
                    stringIndexOf,
                    [CreateStringLiteral(compilation, "`")],
                    new BoundLocalAccess(nameLocal)))
        ]));

        var hasTick = CreateBinaryExpression(
            compilation,
            SyntaxKind.GreaterThanOrEqualsToken,
            new BoundLocalAccess(tickIndexLocal),
            CreateIntLiteral(compilation, 0));

        statements.Add(new BoundIfStatement(
            hasTick,
            CreateAssignmentStatement(
                compilation,
                nameLocal,
                new BoundInvocationExpression(
                    stringSubstring,
                    [CreateIntLiteral(compilation, 0), new BoundLocalAccess(tickIndexLocal)],
                    new BoundLocalAccess(nameLocal)),
                unitType)));

        statements.Add(CreateAppendAssignment(compilation, displayNameLocal, new BoundLocalAccess(nameLocal), unitType));

        if (remainingDepth <= 0)
            return;

        var typeArrayType = compilation.CreateArrayTypeSymbol(typeType);
        var nestedArgsLocal = CreateSynthesizedLocal(method, typeArrayType, $"nestedArgs{remainingDepth}");
        var nestedLengthLocal = CreateSynthesizedLocal(method, intType, $"nestedLength{remainingDepth}");
        var nestedIndexLocal = CreateSynthesizedLocal(method, intType, $"nestedIndex{remainingDepth}");
        var nestedTypeLocal = CreateSynthesizedLocal(method, typeType, $"nestedType{remainingDepth}");
        var nestedNameLocal = CreateSynthesizedLocal(method, compilation.GetSpecialType(SpecialType.System_String)!, $"nestedName{remainingDepth}");
        var nestedTickIndexLocal = CreateSynthesizedLocal(method, intType, $"nestedTick{remainingDepth}");

        statements.Add(new BoundLocalDeclarationStatement([
            new BoundVariableDeclarator(
                nestedArgsLocal,
                new BoundInvocationExpression(typeGenericArgsGetter, Array.Empty<BoundExpression>(), typeExpression))
        ]));

        statements.Add(new BoundLocalDeclarationStatement([
            new BoundVariableDeclarator(
                nestedLengthLocal,
                new BoundInvocationExpression(arrayLengthGetter, Array.Empty<BoundExpression>(), new BoundLocalAccess(nestedArgsLocal)))
        ]));

        var hasNestedArgs = CreateBinaryExpression(
            compilation,
            SyntaxKind.GreaterThanToken,
            new BoundLocalAccess(nestedLengthLocal),
            CreateIntLiteral(compilation, 0));

        var nestedBlockStatements = new List<BoundStatement>
        {
            CreateAppendAssignment(compilation, displayNameLocal, CreateStringLiteral(compilation, "<"), unitType),
            new BoundLocalDeclarationStatement([
                new BoundVariableDeclarator(nestedIndexLocal, CreateIntLiteral(compilation, 0))
            ])
        };

        var nestedLoopStatements = new List<BoundStatement>();
        nestedLoopStatements.Add(new BoundIfStatement(
            CreateBinaryExpression(
                compilation,
                SyntaxKind.GreaterThanToken,
                new BoundLocalAccess(nestedIndexLocal),
                CreateIntLiteral(compilation, 0)),
            CreateAppendAssignment(compilation, displayNameLocal, CreateStringLiteral(compilation, ", "), unitType)));

        nestedLoopStatements.Add(new BoundLocalDeclarationStatement([
            new BoundVariableDeclarator(
                nestedTypeLocal,
                new BoundArrayAccessExpression(
                    new BoundLocalAccess(nestedArgsLocal),
                    [new BoundLocalAccess(nestedIndexLocal)],
                    typeType))
        ]));

        AppendFriendlyTypeNameStatements(
            compilation,
            method,
            new BoundLocalAccess(nestedTypeLocal),
            displayNameLocal,
            nestedNameLocal,
            nestedTickIndexLocal,
            typeNameGetter,
            typeGenericArgsGetter,
            arrayLengthGetter,
            stringIndexOf,
            stringSubstring,
            unitType,
            intType,
            typeType,
            nestedLoopStatements,
            remainingDepth - 1);

        nestedLoopStatements.Add(CreateAssignmentStatement(
            compilation,
            nestedIndexLocal,
            CreateBinaryExpression(
                compilation,
                SyntaxKind.PlusToken,
                new BoundLocalAccess(nestedIndexLocal),
                CreateIntLiteral(compilation, 1)),
            unitType));

        nestedBlockStatements.Add(new BoundWhileStatement(
            CreateBinaryExpression(
                compilation,
                SyntaxKind.LessThanToken,
                new BoundLocalAccess(nestedIndexLocal),
                new BoundLocalAccess(nestedLengthLocal)),
            new BoundBlockStatement(nestedLoopStatements)));

        nestedBlockStatements.Add(CreateAppendAssignment(compilation, displayNameLocal, CreateStringLiteral(compilation, ">"), unitType));

        statements.Add(new BoundIfStatement(hasNestedArgs, new BoundBlockStatement(nestedBlockStatements)));
    }

    private static BoundExpression CreateUnionTagEquality(
        Compilation compilation,
        IMethodSymbol method,
        SourceDiscriminatedUnionSymbol unionSymbol,
        int ordinal)
    {
        var tagAccess = new BoundFieldAccess(new BoundSelfExpression(method.ContainingType!), unionSymbol.DiscriminatorField);
        var tagType = unionSymbol.DiscriminatorField.Type;
        var literal = new BoundLiteralExpression(BoundLiteralExpressionKind.NumericLiteral, (byte)ordinal, tagType);

        if (!BoundBinaryOperator.TryLookup(compilation, SyntaxKind.EqualsEqualsToken, tagType, tagType, out var equals))
            throw new InvalidOperationException("Failed to bind synthesized union discriminator comparison.");

        return new BoundBinaryExpression(tagAccess, equals, literal);
    }

    private static BoundExpression InvokeUnionDisplayNameHelper(IMethodSymbol method)
    {
        var containingType = GetInvocationContainingType(method);
        var helper = containingType
            .GetMembers(SynthesizedUnionMethodNames.DisplayNameHelper)
            .OfType<IMethodSymbol>()
            .First(m => m.Parameters.Length == 0 && m.ReturnType.SpecialType == SpecialType.System_String);

        return new BoundInvocationExpression(helper, Array.Empty<BoundExpression>(), new BoundSelfExpression(containingType));
    }

    private static BoundExpression CreateFriendlyTypeNameInvocation(
        Compilation compilation,
        INamedTypeSymbol containingType,
        ITypeSymbol typeSymbol)
    {
        var helper = containingType
            .GetMembers(SynthesizedUnionMethodNames.FriendlyTypeNameHelper)
            .OfType<IMethodSymbol>()
            .First(m => m.Parameters.Length == 1 &&
                        m.Parameters[0].Type.SpecialType == SpecialType.System_Type &&
                        m.ReturnType.SpecialType == SpecialType.System_String);

        return new BoundInvocationExpression(
            helper,
            [new BoundTypeOfExpression(typeSymbol, compilation.GetSpecialType(SpecialType.System_Type)!)]);
    }

    private static BoundExpression FormatUnionValue(Compilation compilation, BoundExpression value)
        => CreateObjectToStringInvocation(compilation, value);

    private static BoundExpression CreateObjectToStringInvocation(Compilation compilation, BoundExpression receiver)
    {
        var objectToString = compilation.GetSpecialType(SpecialType.System_Object)!
            .GetMembers(nameof(object.ToString))
            .OfType<IMethodSymbol>()
            .First(m => m.Parameters.Length == 0);

        return new BoundInvocationExpression(objectToString, Array.Empty<BoundExpression>(), receiver);
    }

    private static IMethodSymbol ResolveMethod(INamedTypeSymbol type, string name, IReadOnlyList<ITypeSymbol> parameterTypes)
    {
        var methods = type.GetMembers(name).OfType<IMethodSymbol>().ToArray();
        var match = methods.FirstOrDefault(method =>
            method.Parameters.Length == parameterTypes.Count &&
            method.Parameters.Select(parameter => parameter.Type)
                .Zip(parameterTypes, ParameterTypesMatch)
                .All(static matched => matched));

        if (match is not null)
            return match;

        var expectedParameters = string.Join(", ", parameterTypes.Select(typeSymbol => typeSymbol.ToDisplayString(SymbolDisplayFormat.RavenSymbolKeyFormat)));
        var candidates = string.Join(", ", methods.Select(method => method.ToDisplayString(SymbolDisplayFormat.RavenSymbolKeyFormat)));
        throw new InvalidOperationException(
            $"Failed to resolve method '{type.ToDisplayString(SymbolDisplayFormat.RavenSymbolKeyFormat)}.{name}({expectedParameters})'. Candidates: {candidates}");
    }

    private static bool ParameterTypesMatch(ITypeSymbol left, ITypeSymbol right)
    {
        if (SymbolEqualityComparer.Default.Equals(left, right))
            return true;

        if (left.SpecialType != SpecialType.None &&
            left.SpecialType == right.SpecialType)
        {
            return true;
        }

        if (left is IArrayTypeSymbol leftArray &&
            right is IArrayTypeSymbol rightArray &&
            leftArray.Rank == rightArray.Rank)
        {
            return ParameterTypesMatch(leftArray.ElementType, rightArray.ElementType);
        }

        return string.Equals(
            left.ToDisplayString(SymbolDisplayFormat.RavenSymbolKeyFormat),
            right.ToDisplayString(SymbolDisplayFormat.RavenSymbolKeyFormat),
            StringComparison.Ordinal);
    }

    private static IMethodSymbol ResolvePropertyGetter(INamedTypeSymbol type, string propertyName)
    {
        for (INamedTypeSymbol? current = type; current is not null; current = current.BaseType)
        {
            var getter = current.GetMembers(propertyName)
                .OfType<IPropertySymbol>()
                .Select(property => property.GetMethod)
                .FirstOrDefault(static candidate => candidate is not null);

            if (getter is not null)
                return getter;
        }

        throw new InvalidOperationException($"Failed to resolve property getter '{type.ToDisplayString()}.{propertyName}'.");
    }

    private static SourceLocalSymbol CreateSynthesizedLocal(IMethodSymbol method, ITypeSymbol type, string name)
    {
        return new SourceLocalSymbol(
            name,
            type,
            isMutable: true,
            method,
            method.ContainingType,
            method.ContainingNamespace,
            method.Locations.ToArray(),
            method.DeclaringSyntaxReferences.ToArray());
    }

    private static BoundExpressionStatement CreateExpressionStatement(BoundExpression expression)
        => new(expression);

    private static BoundAssignmentStatement CreateAssignmentStatement(
        Compilation compilation,
        ILocalSymbol local,
        BoundExpression right,
        ITypeSymbol unitType)
    {
        return new BoundAssignmentStatement(
            new BoundLocalAssignmentExpression(local, new BoundLocalAccess(local), right, unitType));
    }

    private static BoundAssignmentStatement CreateAppendAssignment(
        Compilation compilation,
        ILocalSymbol local,
        BoundExpression value,
        ITypeSymbol unitType)
        => CreateAssignmentStatement(
            compilation,
            local,
            CreateStringConcat(compilation, new BoundLocalAccess(local), value),
            unitType);

    private static BoundExpression CreateBinaryExpression(
        Compilation compilation,
        SyntaxKind operatorKind,
        BoundExpression left,
        BoundExpression right)
    {
        if (!BoundBinaryOperator.TryLookup(compilation, operatorKind, left.Type, right.Type, out var op))
            throw new InvalidOperationException($"Failed to bind synthesized binary operator '{operatorKind}'.");

        return new BoundBinaryExpression(left, op, right);
    }

    private static BoundLiteralExpression CreateIntLiteral(Compilation compilation, int value)
        => new(BoundLiteralExpressionKind.NumericLiteral, value, compilation.GetSpecialType(SpecialType.System_Int32)!);

    private static BoundExpression ConcatSequence(Compilation compilation, IEnumerable<BoundExpression> expressions)
    {
        using var enumerator = expressions.GetEnumerator();
        if (!enumerator.MoveNext())
            return CreateStringLiteral(compilation, string.Empty);

        var current = enumerator.Current;
        while (enumerator.MoveNext())
            current = CreateStringConcat(compilation, current, enumerator.Current);

        return current;
    }

    private static BoundExpression CreateStringConcat(Compilation compilation, BoundExpression left, BoundExpression right)
    {
        var stringType = compilation.GetSpecialType(SpecialType.System_String)!;
        var candidates = stringType.GetMembers(nameof(string.Concat)).OfType<IMethodSymbol>().ToArray();
        var resolution = OverloadResolver.ResolveOverload(
            candidates,
            [
                new BoundArgument(left, RefKind.None, null),
                new BoundArgument(right, RefKind.None, null)
            ],
            compilation);

        var concat = resolution.Method
            ?? candidates.First(m => m.Parameters.Length == 2);

        return new BoundInvocationExpression(concat, [left, right]);
    }

    private static BoundLiteralExpression CreateStringLiteral(Compilation compilation, string value)
        => new(BoundLiteralExpressionKind.StringLiteral, value, compilation.GetSpecialType(SpecialType.System_String)!);

    private static List<(string Name, SourceFieldSymbol Field)> CollectUnionCaseParameters(SourceDiscriminatedUnionCaseTypeSymbol caseSymbol)
    {
        var parameters = new List<(string Name, SourceFieldSymbol Field)>();

        foreach (var parameter in caseSymbol.ConstructorParameters)
        {
            if (parameter.RefKind != RefKind.None || parameter.Type is null)
                continue;

            var propertyName = UnionFacts.GetCasePropertyName(parameter.Name);
            if (caseSymbol.GetMembers(propertyName).OfType<IPropertySymbol>().FirstOrDefault() is not SourcePropertySymbol property ||
                property.BackingField is not SourceFieldSymbol backingField)
            {
                continue;
            }

            parameters.Add((property.Name, backingField));
        }

        return parameters;
    }

    private static INamedTypeSymbol GetInvocationContainingType(IMethodSymbol method)
    {
        var containingType = (INamedTypeSymbol)method.ContainingType!;
        if (!containingType.IsGenericType || !containingType.TypeArguments.IsDefaultOrEmpty)
            return containingType;

        if (containingType.TypeParameters.IsDefaultOrEmpty)
            return containingType;

        return (INamedTypeSymbol)containingType.Construct(containingType.TypeParameters.Cast<ITypeSymbol>().ToArray());
    }
}
