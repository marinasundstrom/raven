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

        if (method.Name == SynthesizedUnionMethodNames.FriendlyTypeNameHelper &&
            method.IsStatic &&
            method.Parameters.Length == 1 &&
            method.Parameters[0].Type.SpecialType == SpecialType.System_Type &&
            method.ReturnType.SpecialType == SpecialType.System_String &&
            method.ContainingType is SourceDiscriminatedUnionSymbol)
        {
            body = CreateFriendlyTypeNameHelperBody(compilation, method);
            return true;
        }

        if (method.Name == SynthesizedUnionMethodNames.FormatValueHelper &&
            method.IsStatic &&
            method.Parameters.Length == 2 &&
            method.Parameters[0].Type.SpecialType == SpecialType.System_Object &&
            method.Parameters[1].Type.SpecialType == SpecialType.System_Type &&
            method.ReturnType.SpecialType == SpecialType.System_String)
        {
            body = CreateUnionFormatValueHelperBody(compilation, method);
            return true;
        }

        if (method.MethodKind is MethodKind.PropertyGet or MethodKind.PropertySet or MethodKind.InitOnly &&
            method.ContainingSymbol is SourcePropertySymbol { IsAutoProperty: true, BackingField: SourceFieldSymbol } autoProperty)
        {
            body = CreateAutoPropertyAccessorBody(compilation, method, autoProperty);
            return true;
        }

        if (method.Name == "TryGetValue" &&
            method.Parameters.Length == 1 &&
            method.Parameters[0].RefKind == RefKind.Out &&
            method.ReturnType.SpecialType == SpecialType.System_Boolean &&
            method.DeclaringSyntaxReferences.IsDefaultOrEmpty &&
            TryGetSourceDiscriminatedUnionDefinition(method.ContainingType) is { } tryGetUnion)
        {
            body = CreateUnionTryGetValueBody(compilation, method, tryGetUnion, method.Parameters[0]);
            return true;
        }

        if (method.MethodKind == MethodKind.Constructor &&
            method.DeclaringSyntaxReferences.IsDefaultOrEmpty &&
            TryGetSourceDiscriminatedUnionDefinition(method.ContainingType) is { } constructorUnion)
        {
            body = CreateUnionCarrierConstructorBody(compilation, method, constructorUnion);
            return true;
        }

        if (method.MethodKind == MethodKind.Constructor &&
            method.DeclaringSyntaxReferences.IsDefaultOrEmpty &&
            TryGetSourceNamedTypeDefinition(method.ContainingType) is { IsRecord: true } recordNominalType &&
            method.Parameters.Length == 1 &&
            SymbolEqualityComparer.Default.Equals(method.Parameters[0].Type, method.ContainingType))
        {
            body = CreateRecordCopyConstructorBody(compilation, method, recordNominalType);
            return true;
        }

        if (method.MethodKind == MethodKind.Constructor &&
            TryGetUnionCaseType(method.ContainingType) is { } caseType)
        {
            body = CreateUnionCaseConstructorBody(compilation, method, caseType);
            return true;
        }

        if (method.MethodKind == MethodKind.PropertyGet &&
            TryGetSynthesizableUnionCaseProperty(method) is { } unionCaseProperty)
        {
            body = CreateUnionCasePropertyGetterBody(method, unionCaseProperty);
            return true;
        }

        if (method.Name == "Deconstruct" &&
            method.MethodKind == MethodKind.Ordinary &&
            method.ReturnType.SpecialType == SpecialType.System_Unit &&
            method.DeclaringSyntaxReferences.IsDefaultOrEmpty &&
            TryGetUnionCaseType(method.ContainingType) is { } deconstructCase)
        {
            body = CreateUnionCaseDeconstructBody(compilation, method, deconstructCase);
            return true;
        }

        if (method.Name == "Deconstruct" &&
            method.MethodKind == MethodKind.Ordinary &&
            method.ReturnType.SpecialType == SpecialType.System_Unit &&
            method.DeclaringSyntaxReferences.IsDefaultOrEmpty &&
            TryGetSourceNamedTypeDefinition(method.ContainingType) is { } nominalType)
        {
            var deconstructProperties = GetNominalDeconstructProperties(nominalType);
            if (!deconstructProperties.IsDefaultOrEmpty)
            {
                body = CreateNominalDeconstructBody(compilation, method, deconstructProperties);
                return true;
            }
        }

        if (method.Name == nameof(object.ToString) &&
            method.Parameters.Length == 0 &&
            method.ReturnType.SpecialType == SpecialType.System_String)
        {
            switch (method.ContainingType)
            {
                case SourceNamedTypeSymbol { IsRecord: true } recordType:
                    body = CreateRecordToStringBody(compilation, method, recordType);
                    return true;

                case SourceDiscriminatedUnionSymbol unionSymbol:
                    body = CreateUnionToStringBody(compilation, method, unionSymbol);
                    return true;

                case SourceDiscriminatedUnionCaseTypeSymbol caseSymbol:
                    body = CreateUnionCaseToStringBody(compilation, method, caseSymbol);
                    return true;
            }
        }

        if (method.Name == nameof(object.GetHashCode) &&
            method.Parameters.Length == 0 &&
            method.ReturnType.SpecialType == SpecialType.System_Int32 &&
            method.DeclaringSyntaxReferences.IsDefaultOrEmpty &&
            TryGetSourceNamedTypeDefinition(method.ContainingType) is { IsRecord: true } hashRecordType)
        {
            body = CreateRecordGetHashCodeBody(compilation, method, hashRecordType);
            return true;
        }

        if (method.Name == nameof(object.Equals) &&
            method.Parameters.Length == 1 &&
            method.ReturnType.SpecialType == SpecialType.System_Boolean &&
            method.DeclaringSyntaxReferences.IsDefaultOrEmpty &&
            TryGetSourceNamedTypeDefinition(method.ContainingType) is { IsRecord: true, IsValueType: false } equalsRecordType)
        {
            if (method.Parameters[0].Type.SpecialType == SpecialType.System_Object)
            {
                body = CreateRecordObjectEqualsBody(compilation, method, equalsRecordType);
                return true;
            }

            if (SymbolEqualityComparer.Default.Equals(method.Parameters[0].Type, method.ContainingType))
            {
                body = CreateRecordTypedEqualsBody(compilation, method, equalsRecordType);
                return true;
            }
        }

        if (method.MethodKind == MethodKind.UserDefinedOperator &&
            method.Parameters.Length == 2 &&
            method.ReturnType.SpecialType == SpecialType.System_Boolean &&
            method.DeclaringSyntaxReferences.IsDefaultOrEmpty &&
            TryGetSourceNamedTypeDefinition(method.ContainingType) is { IsRecord: true } operatorRecordType &&
            SymbolEqualityComparer.Default.Equals(method.Parameters[0].Type, method.ContainingType) &&
            SymbolEqualityComparer.Default.Equals(method.Parameters[1].Type, method.ContainingType))
        {
            if (string.Equals(method.Name, "op_Equality", StringComparison.Ordinal))
            {
                body = CreateRecordEqualityOperatorBody(compilation, method, operatorRecordType);
                return true;
            }

            if (string.Equals(method.Name, "op_Inequality", StringComparison.Ordinal))
            {
                body = CreateRecordInequalityOperatorBody(compilation, method, operatorRecordType);
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
            AppendFormattedMemberList(
                compilation,
                parts,
                CreateUnionCaseDisplayMembers(compilation, method, parameterInfos));
            parts.Add(CreateStringLiteral(compilation, ")"));
        }

        statements.Add(new BoundReturnStatement(ConcatSequence(compilation, parts)));
        return new BoundBlockStatement(statements);
    }

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
        var intType = compilation.GetSpecialType(SpecialType.System_Int32)
            ?? throw new InvalidOperationException("Failed to resolve System.Int32.");

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

    private static BoundExpression FormatUnionValue(Compilation compilation, IMethodSymbol method, BoundExpression value)
        => InvokeUnionFormatValueHelper(compilation, method, value);

    private static BoundExpression FormatNonGenericNominalValue(Compilation compilation, BoundExpression value)
    {
        var valueType = value.Type.GetPlainType();

        if (valueType.SpecialType == SpecialType.System_String)
            return CreateQuotedStringValue(compilation, value, quote: "\"");

        if (valueType.SpecialType == SpecialType.System_Char)
            return CreateQuotedStringValue(compilation, CreateObjectToStringInvocation(compilation, value), quote: "'");

        return CreateObjectToStringInvocation(compilation, value);
    }

    private static void AppendFormattedMemberList(
        Compilation compilation,
        List<BoundExpression> parts,
        IReadOnlyList<(string? Name, BoundExpression Value)> members)
    {
        for (var index = 0; index < members.Count; index++)
        {
            if (index > 0)
                parts.Add(CreateStringLiteral(compilation, ", "));

            var (name, value) = members[index];
            if (!string.IsNullOrEmpty(name))
            {
                parts.Add(CreateStringLiteral(compilation, name!));
                parts.Add(CreateStringLiteral(compilation, " = "));
            }

            parts.Add(value);
        }
    }

    private static List<(string? Name, BoundExpression Value)> CreateUnionCaseDisplayMembers(
        Compilation compilation,
        IMethodSymbol method,
        IReadOnlyList<(string Name, SourceFieldSymbol Field)> parameterInfos)
    {
        var includeParameterNames = parameterInfos.Count > 1;
        var members = new List<(string? Name, BoundExpression Value)>(parameterInfos.Count);
        var self = new BoundSelfExpression(method.ContainingType!);

        foreach (var (name, field) in parameterInfos)
        {
            var payloadAccess = new BoundFieldAccess(self, field);
            members.Add((includeParameterNames ? name : null, FormatUnionValue(compilation, method, payloadAccess)));
        }

        return members;
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

    private static BoundBlockStatement CreateUnionFormatValueHelperBody(Compilation compilation, IMethodSymbol method)
    {
        var statements = new List<BoundStatement>();
        var stringType = compilation.GetSpecialType(SpecialType.System_String)!;
        var charType = compilation.GetSpecialType(SpecialType.System_Char)!;
        var typeType = compilation.GetSpecialType(SpecialType.System_Type)!;

        var valueParameter = method.Parameters[0];
        var valueTypeParameter = method.Parameters[1];
        var valueAccess = new BoundParameterAccess(valueParameter);
        var valueTypeAccess = new BoundParameterAccess(valueTypeParameter);

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

        statements.Add(new BoundReturnStatement(CreateObjectToStringInvocation(compilation, valueAccess)));
        return new BoundBlockStatement(statements);
    }

    private static BoundBlockStatement CreateUnionTryGetValueBody(
        Compilation compilation,
        IMethodSymbol method,
        SourceDiscriminatedUnionSymbol unionSymbol,
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
        SourceDiscriminatedUnionSymbol unionSymbol)
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
                CreateByteLiteral(compilation, (byte)ordinal),
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

    private static BoundBlockStatement CreateAutoPropertyAccessorBody(
        Compilation compilation,
        IMethodSymbol method,
        SourcePropertySymbol property)
    {
        if (property.BackingField is not SourceFieldSymbol backingField)
            return CreateImplicitUnitReturnBody();

        var receiver = property.IsStatic ? null : new BoundSelfExpression(method.ContainingType!);
        if (method.MethodKind == MethodKind.PropertyGet)
            return new BoundBlockStatement([new BoundReturnStatement(new BoundFieldAccess(receiver, backingField))]);

        var unitType = compilation.GetSpecialType(SpecialType.System_Unit)
            ?? throw new InvalidOperationException("Failed to resolve System.Unit.");
        var valueParameter = method.Parameters.FirstOrDefault();
        if (valueParameter is null)
            return CreateImplicitUnitReturnBody();

        var assignment = new BoundFieldAssignmentExpression(
            receiver,
            backingField,
            CreateConversion(compilation, new BoundParameterAccess(valueParameter), backingField.Type),
            unitType);
        return new BoundBlockStatement([
            new BoundAssignmentStatement(assignment),
            CreateImplicitUnitReturn()
        ]);
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

    private static BoundBlockStatement CreateDeconstructBody(
        Compilation compilation,
        IMethodSymbol method,
        IReadOnlyList<BoundExpression> sourceValues)
    {
        var statements = new List<BoundStatement>();
        var factory = new BoundNodeFactory(compilation);
        var parameterCount = Math.Min(sourceValues.Count, method.Parameters.Length);

        for (var index = 0; index < parameterCount; index++)
        {
            var targetParameter = method.Parameters[index];
            var targetType = targetParameter.GetByRefElementType();
            var convertedValue = CreateConversion(compilation, sourceValues[index], targetType);

            statements.Add(new BoundAssignmentStatement(
                factory.CreateByRefAssignmentExpression(
                    new BoundParameterAccess(targetParameter),
                    targetType,
                    convertedValue)));
        }

        return WithImplicitUnitReturn(statements);
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

    private static BoundExpression CreateSelfPropertyGetterAccess(
        IMethodSymbol method,
        IPropertySymbol property)
    {
        return CreatePropertyGetterAccess(new BoundSelfExpression(method.ContainingType!), property);
    }

    private static BoundBlockStatement CreateImplicitUnitReturnBody()
    {
        return new BoundBlockStatement([CreateImplicitUnitReturn()]);
    }

    private static BoundBlockStatement WithImplicitUnitReturn(List<BoundStatement> statements)
    {
        statements.Add(CreateImplicitUnitReturn());
        return new BoundBlockStatement(statements);
    }

    private static BoundReturnStatement CreateImplicitUnitReturn()
    {
        return new BoundReturnStatement(null);
    }

    private static BoundExpression InvokeUnionFormatValueHelper(Compilation compilation, IMethodSymbol method, BoundExpression value)
    {
        var containingType = GetInvocationContainingType(method);
        var helper = containingType
            .GetMembers(SynthesizedUnionMethodNames.FormatValueHelper)
            .OfType<IMethodSymbol>()
            .First(m => m.IsStatic &&
                        m.Parameters.Length == 2 &&
                        m.Parameters[0].Type.SpecialType == SpecialType.System_Object &&
                        m.Parameters[1].Type.SpecialType == SpecialType.System_Type &&
                        m.ReturnType.SpecialType == SpecialType.System_String);
        var objectType = compilation.GetSpecialType(SpecialType.System_Object)!;

        return new BoundInvocationExpression(
            helper,
            [
                CreateConversion(compilation, value, objectType),
                new BoundTypeOfExpression(value.Type, compilation.GetSpecialType(SpecialType.System_Type)!)
            ]);
    }

    private static BoundExpression CreateObjectToStringInvocation(Compilation compilation, BoundExpression receiver)
    {
        var objectToString = compilation.GetSpecialType(SpecialType.System_Object)!
            .GetMembers(nameof(object.ToString))
            .OfType<IMethodSymbol>()
            .First(m => m.Parameters.Length == 0);

        return new BoundInvocationExpression(objectToString, Array.Empty<BoundExpression>(), receiver);
    }

    private static BoundExpression CreatePropertyGetterAccess(BoundExpression receiver, IPropertySymbol property)
    {
        if (property.GetMethod is null)
            throw new InvalidOperationException($"Property '{property.Name}' does not have a getter.");

        return new BoundInvocationExpression(property.GetMethod, Array.Empty<BoundExpression>(), receiver);
    }

    private static BoundExpression CreateQuotedStringValue(
        Compilation compilation,
        BoundExpression value,
        string quote)
    {
        return ConcatSequence(
            compilation,
            [
                CreateStringLiteral(compilation, quote),
                value,
                CreateStringLiteral(compilation, quote)
            ]);
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

    private static BoundExpression CreateLogicalNotExpression(Compilation compilation, BoundExpression operand)
    {
        if (!BoundUnaryOperator.TryLookup(compilation, SyntaxKind.ExclamationToken, operand.Type, out var op))
            throw new InvalidOperationException("Failed to bind synthesized unary operator '!'.");

        return new BoundUnaryExpression(op, operand);
    }

    private static BoundLiteralExpression CreateIntLiteral(Compilation compilation, int value)
        => new(BoundLiteralExpressionKind.NumericLiteral, value, compilation.GetSpecialType(SpecialType.System_Int32)!);

    private static BoundLiteralExpression CreateBoolLiteral(Compilation compilation, bool value)
        => new(
            value ? BoundLiteralExpressionKind.TrueLiteral : BoundLiteralExpressionKind.FalseLiteral,
            value,
            compilation.GetSpecialType(SpecialType.System_Boolean)!);

    private static BoundLiteralExpression CreateByteLiteral(Compilation compilation, byte value)
        => new(BoundLiteralExpressionKind.NumericLiteral, value, compilation.GetSpecialType(SpecialType.System_Byte)!);

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

    private static BoundLiteralExpression CreateNullLiteral(Compilation compilation)
        => new(BoundLiteralExpressionKind.NullLiteral, null!, compilation.NullTypeSymbol);

    private static BoundExpression CreateConversion(Compilation compilation, BoundExpression expression, ITypeSymbol targetType)
    {
        var conversion = compilation.ClassifyConversion(expression.Type, targetType, includeUserDefined: false);
        if (!conversion.Exists)
            throw new InvalidOperationException($"Failed to bind synthesized conversion from '{expression.Type}' to '{targetType}'.");

        return new BoundConversionExpression(expression, targetType, conversion);
    }

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

    private static SourceDiscriminatedUnionSymbol? TryGetSourceDiscriminatedUnionDefinition(INamedTypeSymbol? typeSymbol)
    {
        return typeSymbol switch
        {
            SourceDiscriminatedUnionSymbol sourceUnion => sourceUnion,
            ConstructedNamedTypeSymbol { OriginalDefinition: SourceDiscriminatedUnionSymbol sourceUnion } => sourceUnion,
            _ => null
        };
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

    private static IUnionCaseTypeSymbol? TryGetUnionCaseType(INamedTypeSymbol? typeSymbol)
    {
        return typeSymbol switch
        {
            SourceDiscriminatedUnionCaseTypeSymbol sourceCase => sourceCase,
            ConstructedNamedTypeSymbol { OriginalDefinition: SourceDiscriminatedUnionCaseTypeSymbol } constructedCase => constructedCase,
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
        SourceDiscriminatedUnionSymbol unionSymbol,
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
