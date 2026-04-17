using System;
using System.Collections.Generic;
using System.Linq;
using System.Reflection;

using Raven.CodeAnalysis.Symbols;
using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis;

internal static partial class SynthesizedMethodBodyFactory
{
    private static BoundBlockStatement CreateUnionDisplayNameHelperBody(
        Compilation compilation,
        SourceUnionSymbol unionSymbol)
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
        SourceUnionCaseTypeSymbol caseSymbol)
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
        SourceUnionSymbol unionSymbol)
    {
        var statements = new List<BoundStatement>();

        if (!unionSymbol.CaseTypes.IsDefaultOrEmpty && unionSymbol.CaseTypes.Length > 0)
        {
            foreach (var caseType in unionSymbol.CaseTypes.OfType<SourceUnionCaseTypeSymbol>())
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
            var formatted = FormatUnionValue(compilation, method, payloadAccess);
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

    private static BoundBlockStatement CreateUnionValuePropertyGetterBody(
        Compilation compilation,
        IMethodSymbol method,
        SourceUnionSymbol unionSymbol)
    {
        var statements = new List<BoundStatement>();
        var objectType = compilation.GetSpecialType(SpecialType.System_Object)
            ?? throw new InvalidOperationException("Failed to resolve System.Object.");

        if (!unionSymbol.CaseTypes.IsDefaultOrEmpty && unionSymbol.CaseTypes.Length > 0)
        {
            foreach (var caseType in unionSymbol.CaseTypes.OfType<SourceUnionCaseTypeSymbol>())
            {
                var payloadField = (SourceFieldSymbol)UnionFieldUtilities.GetRequiredPayloadField(unionSymbol, caseType);
                var payloadAccess = new BoundFieldAccess(new BoundSelfExpression(method.ContainingType!), payloadField);
                var boxedValue = CreateConversion(compilation, payloadAccess, objectType);
                statements.Add(new BoundIfStatement(
                    CreateUnionTagEquality(compilation, method, unionSymbol, caseType.Ordinal),
                    new BoundReturnStatement(
                        SymbolEqualityComparer.Default.Equals(method.ReturnType, objectType)
                            ? boxedValue
                            : CreateConversion(compilation, boxedValue, method.ReturnType))));
            }
        }
        else
        {
            var payloadFields = unionSymbol.PayloadFields.OfType<SourceFieldSymbol>().ToArray();
            for (var index = 0; index < payloadFields.Length; index++)
            {
                var payloadAccess = new BoundFieldAccess(new BoundSelfExpression(method.ContainingType!), payloadFields[index]);
                var boxedValue = CreateConversion(compilation, payloadAccess, objectType);
                statements.Add(new BoundIfStatement(
                    CreateUnionTagEquality(compilation, method, unionSymbol, index),
                    new BoundReturnStatement(
                        SymbolEqualityComparer.Default.Equals(method.ReturnType, objectType)
                            ? boxedValue
                            : CreateConversion(compilation, boxedValue, method.ReturnType))));
            }
        }

        statements.Add(new BoundReturnStatement(CreateNullLiteral(compilation)));
        return new BoundBlockStatement(statements);
    }

    private static BoundBlockStatement CreateUnionHasValuePropertyGetterBody(
        Compilation compilation,
        IMethodSymbol method,
        SourceUnionSymbol unionSymbol)
    {
        var tagField = unionSymbol.DiscriminatorField;
        var tagAccess = new BoundFieldAccess(new BoundSelfExpression(method.ContainingType!), tagField);
        var hasValue = CreateBinaryExpression(
            compilation,
            SyntaxKind.NotEqualsToken,
            tagAccess,
            CreateByteLiteral(compilation, 0));

        return new BoundBlockStatement([
            new BoundReturnStatement(hasValue)
        ]);
    }

    private static BoundBlockStatement CreateUnionCaseToStringBody(
        Compilation compilation,
        IMethodSymbol method,
        SourceUnionCaseTypeSymbol caseSymbol)
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

        var parameterInfos = CollectUnionCaseParameters(method.ContainingType, caseSymbol);
        if (parameterInfos.Count > 0)
        {
            parts.Add(CreateStringLiteral(compilation, "("));
            AppendFormattedMemberList(
                compilation,
                parts,
                CreateUnionCaseDisplayMembers(compilation, method, parameterInfos));
            parts.Add(CreateStringLiteral(compilation, ")"));
        }

        statements.Add(new BoundReturnStatement(ConcatSequence(compilation, parts)));
        return new BoundBlockStatement(statements);
    }

    private static BoundBlockStatement CreateFriendlyTypeNameHelperBody(
        Compilation compilation,
        IMethodSymbol method)
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
        var unitType = compilation.GetSpecialType(SpecialType.System_Unit)
            ?? throw new InvalidOperationException("Failed to resolve System.Unit.");
        var statements = new List<BoundStatement>();
        var displayNameLocal = CreateSynthesizedLocal(method, stringType, "displayName");
        var currentNameLocal = CreateSynthesizedLocal(method, stringType, "currentName");
        var tickIndexLocal = CreateSynthesizedLocal(method, intType, "tickIndex");

        var typeNameGetter = ResolvePropertyGetter(memberInfoType, nameof(MemberInfo.Name));
        var typeGenericArgsGetter = ResolveMethod(typeType, nameof(Type.GetGenericArguments), []);
        var arrayLengthGetter = ResolvePropertyGetter(arrayType, nameof(Array.Length));
        var stringIndexOf = ResolveMethod(stringType, nameof(string.IndexOf), [stringType]);
        var stringSubstring = ResolveMethod(stringType, nameof(string.Substring), [intType, intType]);

        statements.Add(new BoundLocalDeclarationStatement([
            new BoundVariableDeclarator(displayNameLocal, CreateStringLiteral(compilation, string.Empty))
        ]));

        AppendFriendlyTypeNameStatements(
            compilation,
            method,
            new BoundParameterAccess(method.Parameters[0]),
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
            statements,
            remainingDepth: 4);

        statements.Add(new BoundReturnStatement(new BoundLocalAccess(displayNameLocal)));
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
        var typeDeclaringTypeGetter = ResolvePropertyGetter(typeType, nameof(Type.DeclaringType));
        var typeNameGetter = ResolvePropertyGetter(memberInfoType, nameof(MemberInfo.Name));
        var typeGenericArgsGetter = ResolveMethod(typeType, nameof(Type.GetGenericArguments), []);
        var arrayLengthGetter = ResolvePropertyGetter(arrayType, nameof(Array.Length));
        var stringIndexOf = ResolveMethod(stringType, nameof(string.IndexOf), [stringType]);
        var stringSubstring = ResolveMethod(stringType, nameof(string.Substring), [intType, intType]);

        var runtimeTypeLocal = CreateSynthesizedLocal(method, typeType, "runtimeType");
        var declaringTypeLocal = CreateSynthesizedLocal(method, typeType, "declaringType");
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
                new BoundInvocationExpression(
                    objectGetType,
                    Array.Empty<BoundExpression>(),
                    new BoundSelfExpression(method.ContainingType!)))
        ]));

        statements.Add(new BoundLocalDeclarationStatement([
            new BoundVariableDeclarator(
                declaringTypeLocal,
                new BoundInvocationExpression(
                    typeDeclaringTypeGetter,
                    Array.Empty<BoundExpression>(),
                    new BoundLocalAccess(runtimeTypeLocal)))
        ]));

        statements.Add(new BoundIfStatement(
            CreateBinaryExpression(
                compilation,
                SyntaxKind.NotEqualsToken,
                new BoundLocalAccess(declaringTypeLocal),
                CreateNullLiteral(compilation)),
            CreateAssignmentStatement(
                compilation,
                runtimeTypeLocal,
                new BoundLocalAccess(declaringTypeLocal),
                unitType)));

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
        SourceUnionSymbol unionSymbol,
        int ordinal)
    {
        var tagAccess = new BoundFieldAccess(new BoundSelfExpression(method.ContainingType!), unionSymbol.DiscriminatorField);
        var tagType = unionSymbol.DiscriminatorField.Type;
        var literal = new BoundLiteralExpression(BoundLiteralExpressionKind.NumericLiteral, GetStoredUnionTag(ordinal), tagType);

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

    private static BoundExpression InvokeFriendlyTypeNameHelper(IMethodSymbol method, BoundExpression typeExpression)
    {
        var containingType = GetInvocationContainingType(method);
        var helper = containingType
            .GetMembers(SynthesizedUnionMethodNames.FriendlyTypeNameHelper)
            .OfType<IMethodSymbol>()
            .First(m => m.IsStatic &&
                        m.Parameters.Length == 1 &&
                        m.Parameters[0].Type.SpecialType == SpecialType.System_Type &&
                        m.ReturnType.SpecialType == SpecialType.System_String);

        return new BoundInvocationExpression(helper, [typeExpression]);
    }

    private static BoundExpression FormatUnionValue(Compilation compilation, IMethodSymbol method, BoundExpression value, ITypeSymbol? declaredType = null)
        => InvokeUnionFormatValueHelper(compilation, method, value, declaredType);

    private static List<(string? Name, BoundExpression Value)> CreateUnionCaseDisplayMembers(
        Compilation compilation,
        IMethodSymbol method,
        IReadOnlyList<(string Name, SourcePropertySymbol Property, ITypeSymbol DeclaredType)> parameterInfos)
    {
        var includeParameterNames = parameterInfos.Count > 1;
        var members = new List<(string? Name, BoundExpression Value)>(parameterInfos.Count);
        var self = new BoundSelfExpression(method.ContainingType!);

        foreach (var (name, property, declaredType) in parameterInfos)
        {
            var field = ResolveUnionCaseBackingField(method.ContainingType, property);
            if (field is null)
                continue;

            var payloadAccess = new BoundFieldAccess(self, field);
            members.Add((includeParameterNames ? name : null, FormatUnionValue(compilation, method, payloadAccess, declaredType)));
        }

        return members;
    }

    private static BoundBlockStatement CreateUnionFormatValueHelperBody(Compilation compilation, IMethodSymbol method)
    {
        var statements = new List<BoundStatement>();
        var stringType = compilation.GetSpecialType(SpecialType.System_String)!;
        var charType = compilation.GetSpecialType(SpecialType.System_Char)!;
        var decimalType = compilation.GetSpecialType(SpecialType.System_Decimal)!;
        var typeType = compilation.GetSpecialType(SpecialType.System_Type)!;
        var unitType = compilation.GetSpecialType(SpecialType.System_Unit)!;
        var bindingFlagsType = compilation.GetTypeByMetadataName("System.Reflection.BindingFlags")
            ?? throw new InvalidOperationException("Failed to resolve System.Reflection.BindingFlags.");
        var typeIsEnumGetter = ResolvePropertyGetter(typeType, nameof(Type.IsEnum));
        var typeIsPrimitiveGetter = ResolvePropertyGetter(typeType, nameof(Type.IsPrimitive));
        var typeGetMethod = ResolveMethod(typeType, nameof(Type.GetMethod), [stringType, bindingFlagsType]);
        var helperLookupFlags = new BoundLiteralExpression(
            BoundLiteralExpressionKind.NumericLiteral,
            (int)(BindingFlags.Public | BindingFlags.NonPublic | BindingFlags.Static),
            bindingFlagsType);

        var valueParameter = method.Parameters[0];
        var valueTypeParameter = method.Parameters[1];
        var renderStructuredParameter = method.Parameters[2];
        var valueAccess = new BoundParameterAccess(valueParameter);
        var valueTypeAccess = new BoundParameterAccess(valueTypeParameter);
        var renderStructuredAccess = new BoundParameterAccess(renderStructuredParameter);
        var runtimeStructuredAccess = CreateBinaryExpression(
            compilation,
            SyntaxKind.BarBarToken,
            new BoundInvocationExpression(
                typeIsEnumGetter,
                Array.Empty<BoundExpression>(),
                valueTypeAccess),
            CreateBinaryExpression(
                compilation,
                SyntaxKind.BarBarToken,
                new BoundInvocationExpression(
                    typeIsPrimitiveGetter,
                    Array.Empty<BoundExpression>(),
                    valueTypeAccess),
                CreateBinaryExpression(
                    compilation,
                    SyntaxKind.BarBarToken,
                    CreateBinaryExpression(
                        compilation,
                        SyntaxKind.EqualsEqualsToken,
                        valueTypeAccess,
                        new BoundTypeOfExpression(decimalType, typeType)),
                    CreateBinaryExpression(
                        compilation,
                        SyntaxKind.BarBarToken,
                        CreateBinaryExpression(
                            compilation,
                            SyntaxKind.EqualsEqualsToken,
                            valueTypeAccess,
                            new BoundTypeOfExpression(unitType, typeType)),
                        CreateBinaryExpression(
                            compilation,
                            SyntaxKind.NotEqualsToken,
                            new BoundInvocationExpression(
                                typeGetMethod,
                                [
                                    CreateStringLiteral(compilation, SynthesizedUnionMethodNames.FriendlyTypeNameHelper),
                                    helperLookupFlags
                                ],
                                valueTypeAccess),
                            CreateNullLiteral(compilation))))));

        statements.Add(new BoundIfStatement(
            CreateBinaryExpression(
                compilation,
                SyntaxKind.EqualsEqualsToken,
                valueAccess,
                CreateNullLiteral(compilation)),
            new BoundReturnStatement(CreateStringLiteral(compilation, "null"))));

        statements.Add(new BoundIfStatement(
            CreateBinaryExpression(
                compilation,
                SyntaxKind.EqualsEqualsToken,
                valueTypeAccess,
                new BoundTypeOfExpression(stringType, typeType)),
            new BoundReturnStatement(CreateQuotedStringValue(
                compilation,
                CreateConversion(compilation, valueAccess, stringType),
                quote: "\""))));

        statements.Add(new BoundIfStatement(
            CreateBinaryExpression(
                compilation,
                SyntaxKind.EqualsEqualsToken,
                valueTypeAccess,
                new BoundTypeOfExpression(charType, typeType)),
            new BoundReturnStatement(CreateQuotedStringValue(
                compilation,
                CreateObjectToStringInvocation(compilation, valueAccess),
                quote: "'"))));

        statements.Add(new BoundIfStatement(
            new BoundInvocationExpression(
                typeIsEnumGetter,
                Array.Empty<BoundExpression>(),
                valueTypeAccess),
            new BoundReturnStatement(ConcatSequence(
                compilation,
                [
                    InvokeFriendlyTypeNameHelper(method, valueTypeAccess),
                    CreateStringLiteral(compilation, "."),
                    CreateObjectToStringInvocation(compilation, valueAccess)
                ]))));

        statements.Add(new BoundIfStatement(
            CreateBinaryExpression(
                compilation,
                SyntaxKind.BarBarToken,
                renderStructuredAccess,
                runtimeStructuredAccess),
            new BoundReturnStatement(CreateObjectToStringInvocation(compilation, valueAccess))));

        statements.Add(new BoundReturnStatement(ConcatSequence(
            compilation,
            [
                CreateStringLiteral(compilation, "<"),
                InvokeFriendlyTypeNameHelper(method, valueTypeAccess),
                CreateStringLiteral(compilation, ">")
            ])));
        return new BoundBlockStatement(statements);
    }

    private static bool CanRenderStructuredDisplay(ITypeSymbol type)
    {
        var plainType = type.GetPlainType();

        if (plainType.TypeKind == TypeKind.Enum ||
            plainType.IsUnion ||
            plainType.IsUnionCase)
        {
            return true;
        }

        if (TryGetSourceNamedTypeDefinition(plainType as INamedTypeSymbol) is { IsRecord: true })
            return true;

        return plainType.SpecialType switch
        {
            SpecialType.System_Boolean => true,
            SpecialType.System_SByte => true,
            SpecialType.System_Byte => true,
            SpecialType.System_Int16 => true,
            SpecialType.System_UInt16 => true,
            SpecialType.System_Int32 => true,
            SpecialType.System_UInt32 => true,
            SpecialType.System_Int64 => true,
            SpecialType.System_UInt64 => true,
            SpecialType.System_Decimal => true,
            SpecialType.System_Single => true,
            SpecialType.System_Double => true,
            SpecialType.System_IntPtr => true,
            SpecialType.System_UIntPtr => true,
            SpecialType.System_Unit => true,
            _ => false
        };
    }

    private static BoundBlockStatement CreateUnionTryGetValueBody(
        Compilation compilation,
        IMethodSymbol method,
        SourceUnionSymbol unionSymbol,
        IParameterSymbol targetParameter)
    {
        var statements = new List<BoundStatement>();
        if (!TryGetUnionPayloadSlot(unionSymbol, targetParameter.Type, out var ordinal, out var payloadFieldSymbol))
        {
            statements.Add(new BoundReturnStatement(CreateBoolLiteral(compilation, false)));
            return new BoundBlockStatement(statements);
        }

        var payloadAccess = new BoundFieldAccess(new BoundSelfExpression(method.ContainingType!), payloadFieldSymbol);
        var parameterAccess = new BoundParameterAccess(targetParameter);
        var targetType = targetParameter.GetByRefElementType();
        var convertedPayload = CreateConversion(compilation, payloadAccess, targetType);
        var factory = new BoundNodeFactory(compilation);

        statements.Add(new BoundIfStatement(
            CreateUnionTagEquality(compilation, method, unionSymbol, ordinal),
            new BoundBlockStatement(
            [
                new BoundAssignmentStatement(
                    factory.CreateByRefAssignmentExpression(parameterAccess, targetType, convertedPayload)),
                new BoundReturnStatement(CreateBoolLiteral(compilation, true))
            ])));

        statements.Add(new BoundReturnStatement(CreateBoolLiteral(compilation, false)));
        return new BoundBlockStatement(statements);
    }

    private static BoundBlockStatement CreateUnionCarrierConstructorBody(
        Compilation compilation,
        IMethodSymbol method,
        SourceUnionSymbol unionSymbol)
    {
        var statements = new List<BoundStatement>();
        var unitType = compilation.GetSpecialType(SpecialType.System_Unit)
            ?? throw new InvalidOperationException("Failed to resolve System.Unit.");

        if (method.Parameters.Length == 1 &&
            TryGetUnionPayloadSlot(unionSymbol, method.Parameters[0].Type, out var ordinal, out var payloadField))
        {
            var self = new BoundSelfExpression(method.ContainingType!);
            var tagAssignment = new BoundFieldAssignmentExpression(
                self,
                unionSymbol.DiscriminatorField,
                CreateByteLiteral(compilation, GetStoredUnionTag(ordinal)),
                unitType);
            statements.Add(new BoundAssignmentStatement(tagAssignment));

            var payloadAssignment = new BoundFieldAssignmentExpression(
                self,
                payloadField,
                CreateConversion(compilation, new BoundParameterAccess(method.Parameters[0]), payloadField.Type),
                unitType);
            statements.Add(new BoundAssignmentStatement(payloadAssignment));
        }

        return WithImplicitUnitReturn(statements);
    }

    private static BoundBlockStatement CreateUnionCaseConstructorBody(
        Compilation compilation,
        IMethodSymbol method,
        IUnionCaseTypeSymbol caseType)
    {
        var statements = new List<BoundStatement>();
        var unitType = compilation.GetSpecialType(SpecialType.System_Unit)
            ?? throw new InvalidOperationException("Failed to resolve System.Unit.");

        foreach (var parameter in method.Parameters)
        {
            if (parameter.RefKind != RefKind.None)
                continue;

            var backingFieldName = $"<{parameter.Name}>k__BackingField";
            var backingField = method.ContainingType?
                .GetMembers(backingFieldName)
                .OfType<IFieldSymbol>()
                .FirstOrDefault();

            if (backingField is null)
                continue;

            var assignment = new BoundFieldAssignmentExpression(
                new BoundSelfExpression(method.ContainingType!),
                backingField,
                CreateConversion(compilation, new BoundParameterAccess(parameter), backingField.Type),
                unitType);
            statements.Add(new BoundAssignmentStatement(assignment));
        }

        return WithImplicitUnitReturn(statements);
    }

    private static BoundBlockStatement CreateUnionCaseDeconstructBody(
        Compilation compilation,
        IMethodSymbol method,
        IUnionCaseTypeSymbol caseType)
    {
        var deconstructParameters = caseType.ConstructorParameters;
        var parameterCount = Math.Min(deconstructParameters.Length, method.Parameters.Length);
        var sourceValues = new List<BoundExpression>(parameterCount);

        for (var index = 0; index < parameterCount; index++)
        {
            var sourceParameter = deconstructParameters[index];
            if (sourceParameter.RefKind != RefKind.None)
                continue;

            var propertyName = GetUnionCasePropertyName(sourceParameter.Name);
            var property = caseType.GetMembers(propertyName)
                .OfType<IPropertySymbol>()
                .FirstOrDefault();

            if (property?.GetMethod is null)
                continue;

            sourceValues.Add(CreateSelfPropertyGetterAccess(method, property));
        }

        return CreateDeconstructBody(compilation, method, sourceValues);
    }

    private static BoundBlockStatement CreateUnionCasePropertyGetterBody(
        IMethodSymbol method,
        IPropertySymbol property)
    {
        var backingField = ResolveUnionCaseBackingField(method.ContainingType, property);
        if (backingField is null)
            return CreateImplicitUnitReturnBody();

        var receiver = property.IsStatic ? null : new BoundSelfExpression(method.ContainingType!);
        var fieldAccess = new BoundFieldAccess(receiver, backingField);
        return new BoundBlockStatement([new BoundReturnStatement(fieldAccess)]);
    }

    private static List<(string Name, SourcePropertySymbol Property, ITypeSymbol DeclaredType)> CollectUnionCaseParameters(
        INamedTypeSymbol? containingType,
        SourceUnionCaseTypeSymbol caseSymbol)
    {
        var parameters = new List<(string Name, SourcePropertySymbol Property, ITypeSymbol DeclaredType)>();
        var constructorParameters = containingType is IUnionCaseTypeSymbol constructedCaseType &&
                                    !constructedCaseType.ConstructorParameters.IsDefaultOrEmpty
            ? constructedCaseType.ConstructorParameters
            : caseSymbol.ConstructorParameters.Cast<IParameterSymbol>();

        foreach (var parameter in constructorParameters)
        {
            if (parameter.RefKind != RefKind.None || parameter.Type is null)
                continue;

            var propertyName = UnionFacts.GetCasePropertyName(parameter.Name);
            if (caseSymbol.GetMembers(propertyName).OfType<IPropertySymbol>().FirstOrDefault() is not SourcePropertySymbol property)
            {
                continue;
            }

            parameters.Add((property.Name, property, parameter.Type));
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

    private static SourceUnionSymbol? TryGetSourceDiscriminatedUnionDefinition(INamedTypeSymbol? typeSymbol)
    {
        return typeSymbol switch
        {
            SourceUnionSymbol sourceUnion => sourceUnion,
            ConstructedNamedTypeSymbol { OriginalDefinition: SourceUnionSymbol sourceUnion } => sourceUnion,
            _ => null
        };
    }

    private static IUnionCaseTypeSymbol? TryGetUnionCaseType(INamedTypeSymbol? typeSymbol)
    {
        return typeSymbol switch
        {
            SourceUnionCaseTypeSymbol sourceCase => sourceCase,
            ConstructedNamedTypeSymbol { OriginalDefinition: SourceUnionCaseTypeSymbol } constructedCase => constructedCase,
            _ => null
        };
    }

    private static IPropertySymbol? TryGetSynthesizableUnionCaseProperty(IMethodSymbol method)
    {
        if (method.ContainingType?.TryGetUnionCase() is null)
            return null;

        if (method.ContainingSymbol is not IPropertySymbol property)
            return null;

        if (property.OriginalDefinition is IPropertySymbol originalProperty)
            property = originalProperty;

        return property is SourcePropertySymbol { BackingField: SourceFieldSymbol } sourceProperty
            ? sourceProperty
            : null;
    }

    private static IFieldSymbol? ResolveUnionCaseBackingField(INamedTypeSymbol? containingType, IPropertySymbol property)
    {
        if (property is not SourcePropertySymbol { BackingField: SourceFieldSymbol sourceBackingField })
            return null;

        if (containingType is null)
            return sourceBackingField;

        return containingType.GetMembers(sourceBackingField.Name).OfType<IFieldSymbol>().FirstOrDefault()
            ?? sourceBackingField;
    }

    private static string GetUnionCasePropertyName(string parameterName)
    {
        if (string.IsNullOrEmpty(parameterName) || char.IsUpper(parameterName[0]))
            return parameterName;

        Span<char> buffer = stackalloc char[parameterName.Length];
        parameterName.AsSpan().CopyTo(buffer);
        buffer[0] = char.ToUpperInvariant(buffer[0]);
        return new string(buffer);
    }

    private static bool TryGetUnionPayloadSlot(
        SourceUnionSymbol unionSymbol,
        ITypeSymbol memberType,
        out int ordinal,
        out SourceFieldSymbol payloadFieldSymbol)
    {
        if (memberType.TryGetUnionCase() is { } caseSymbol)
        {
            ordinal = caseSymbol.Ordinal;
            payloadFieldSymbol = (SourceFieldSymbol)UnionFieldUtilities.GetRequiredPayloadField(unionSymbol, caseSymbol);
            return true;
        }

        var payloadFields = unionSymbol.PayloadFields.OfType<SourceFieldSymbol>().ToArray();
        for (var index = 0; index < payloadFields.Length; index++)
        {
            var payloadField = payloadFields[index];
            if (!SymbolEqualityComparer.Default.Equals(payloadField.Type, memberType))
                continue;

            ordinal = index;
            payloadFieldSymbol = payloadField;
            return true;
        }

        ordinal = -1;
        payloadFieldSymbol = null!;
        return false;
    }

    private static byte GetStoredUnionTag(int ordinal)
    {
        checked
        {
            return (byte)(ordinal + 1);
        }
    }
}
