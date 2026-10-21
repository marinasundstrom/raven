using Raven.CodeAnalysis.CodeGen.Portable;
using Raven.CodeAnalysis.Symbols;
using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.NeoClr;

// Native record value semantics use the selected core's methods, not CLR comparer
// singleton or HashCode.Add<T> dependencies. Ordinary CLR synthesis is unchanged.
internal static class NativeRecordBodies
{
    internal static SourceCallablePlan? Create(Compilation compilation, IMethodSymbol method,
        RecordDeclarationSyntax anchor, EmissionCapabilities capabilities)
    {
        if (!method.DeclaringSyntaxReferences.IsEmpty || method.ContainingType is not SourceNamedTypeSymbol { IsRecord: true } owner ||
            method.Name is not ("op_Equality" or "op_Inequality" or "Equals" or "GetHashCode"))
            return null;
        var equatable = compilation.GetTypeByMetadataName(compilation.RuntimeRecordEquatableTypeName);
        if (!SupportsValueSemantics(owner, new HashSet<ITypeSymbol>(SymbolEqualityComparer.Default), equatable))
            return null;
        if (!CallableSignature.TryCreate(method, out var signature, capabilities))
            return null;
        // A selected-core compiler service avoids materializing TypeInfo objects.
        // Older cores retain the descriptor-based policy below.
        var typePolicy = compilation.GetTypeByMetadataName("System.Runtime.CompilerServices.RuntimeHelpers")?
            .GetMembers("IsAssignable").OfType<IMethodSymbol>().SingleOrDefault(m =>
                m.IsStatic && m.DeclaredAccessibility == Accessibility.Public && m.Arity == 2 &&
                m.Parameters.IsEmpty && m.ReturnType.SpecialType == SpecialType.System_Boolean &&
                m.TypeParameters.All(p => p.ConstraintKind == TypeParameterConstraintKind.None && p.ConstraintTypes.IsEmpty) &&
                SymbolEqualityComparer.Default.Equals(m.ContainingAssembly,
                    compilation.GetSpecialType(SpecialType.System_Object).ContainingAssembly));
        var typeOf = compilation.ResolveRuntimeTypeOfContract();
        var assignable = typeOf?.Type.GetMembers("AssignableTo").OfType<IMethodSymbol>()
            .SingleOrDefault(m => !m.IsStatic && m.Parameters.Length == 1 &&
                m.ReturnType.SpecialType == SpecialType.System_Boolean &&
                SymbolEqualityComparer.Default.Equals(m.Parameters[0].Type, typeOf!.Type));
        if (!owner.TypeParameters.IsEmpty && owner.GetMembers().OfType<IFieldSymbol>()
            .Any(f => ContainsTypeParameter(f.Type)) && typePolicy is null && assignable is null)
            return null;
        var boolean = compilation.GetSpecialType(SpecialType.System_Boolean);
        var integer = compilation.GetSpecialType(SpecialType.System_Int32);
        var obj = compilation.GetSpecialType(SpecialType.System_Object);
        var referenceEquals = obj.GetMembers("ReferenceEquals").OfType<IMethodSymbol>().SingleOrDefault(m => m.IsStatic && m.Parameters.Length == 2);
        var objectEquals = obj.GetMembers("Equals").OfType<IMethodSymbol>().SingleOrDefault(m => !m.IsStatic && m.Parameters.Length == 1);
        var objectHash = obj.GetMembers("GetHashCode").OfType<IMethodSymbol>().SingleOrDefault(m => !m.IsStatic && m.Parameters.IsEmpty);
        var hashType = compilation.GetTypeByMetadataName("System.HashCode");
        var combine = hashType?.GetMembers("Combine").OfType<IMethodSymbol>().SingleOrDefault(m =>
            m.IsStatic && !m.IsGenericMethod && m.Parameters.Length == 2 &&
            m.Parameters.All(p => p.Type.SpecialType == SpecialType.System_Int32));
        if (referenceEquals is null || objectEquals is null || objectHash is null || combine is null)
            return null;

        BoundExpression Bool(bool value) => new BoundLiteralExpression(value ? BoundLiteralExpressionKind.TrueLiteral : BoundLiteralExpressionKind.FalseLiteral, value, boolean);
        BoundExpression Int(int value) => new BoundLiteralExpression(BoundLiteralExpressionKind.NumericLiteral, value, integer);
        BoundExpression Box(BoundExpression value) => new BoundConversionExpression(value, obj, compilation.ClassifyConversion(value.Type, obj, includeUserDefined: false));
        BoundExpression ToInt(BoundExpression value) => new BoundConversionExpression(value, integer,
            compilation.ClassifyConversion(value.Type, integer, includeUserDefined: false));
        BoundExpression Null(BoundExpression value) => new BoundInvocationExpression(referenceEquals,
            [Box(value), new BoundLiteralExpression(BoundLiteralExpressionKind.NullLiteral, null!, obj)]);
        BoundExpression Binary(SyntaxKind kind, BoundExpression left, BoundExpression right)
        {
            if (!BoundBinaryOperator.TryLookup(compilation, kind, left.Type, right.Type, out var op))
                throw new InvalidOperationException("Native record binary operator unavailable: " + kind);
            return new BoundBinaryExpression(left, op, right);
        }
        BoundExpression Not(BoundExpression value)
        {
            if (!BoundUnaryOperator.TryLookup(compilation, SyntaxKind.ExclamationToken, value.Type, out var op))
                throw new InvalidOperationException("Native record negation unavailable");
            return new BoundUnaryExpression(op, value);
        }
        var nextCaseLocal = 0;
        SourceLocalSymbol Temporary(ITypeSymbol type) => new("recordValue" + nextCaseLocal++, type, false,
            method, owner, owner.ContainingNamespace, method.Locations.ToArray(), []);
        BoundStatement Declare(SourceLocalSymbol local, BoundExpression value) =>
            new BoundLocalDeclarationStatement([new BoundVariableDeclarator(local, value)]);
        (BoundExpression Test, BoundExpression Value) Case(BoundExpression value, INamedTypeSymbol caseType)
        {
            var local = new SourceLocalSymbol("recordCase" + nextCaseLocal++, caseType, false,
                method, owner, owner.ContainingNamespace, method.Locations.ToArray(), []);
            return (new BoundIsPatternExpression(value,
                new BoundDeclarationPattern(caseType, new BoundSingleVariableDesignator(local)), boolean),
                new BoundLocalAccess(local));
        }
        BoundExpression Property(BoundExpression value, IPropertySymbol property) =>
            new BoundInvocationExpression(property.GetMethod!, [], value, requiresReceiverAddress: true);

        BoundExpression Equal(BoundExpression left, BoundExpression right)
        {
            var type = left.Type.GetNonNullableType();
            if (type is ITypeParameterSymbol && equatable is not null)
            {
                var contract = (INamedTypeSymbol)equatable.Construct(type);
                BoundExpression selected = typePolicy is not null
                    ? new BoundInvocationExpression(typePolicy.Construct(type, contract), [])
                    : new BoundInvocationExpression(assignable!,
                    [new BoundTypeOfExpression(contract, typeOf!.Type)],
                    new BoundTypeOfExpression(type, typeOf!.Type));
                var leftBox = Temporary(obj);
                var rightBox = Temporary(obj);
                var boxedLeft = new BoundLocalAccess(leftBox);
                var boxedRight = new BoundLocalAccess(rightBox);
                var (test, receiver) = Case(boxedLeft, contract);
                var equals = contract.GetMembers("Equals").OfType<IMethodSymbol>().Single();
                var dispatch = new BoundIfExpression(Binary(SyntaxKind.AmpersandAmpersandToken, selected, test),
                    new BoundInvocationExpression(equals, [right], receiver),
                    new BoundInvocationExpression(objectEquals, [boxedRight], boxedLeft));
                return new BoundBlockExpression([
                    Declare(leftBox, Box(left)), Declare(rightBox, Box(right)),
                    new BoundExpressionStatement(new BoundIfExpression(Null(boxedLeft), Null(boxedRight),
                        Binary(SyntaxKind.AmpersandAmpersandToken, Not(Null(boxedRight)), dispatch)))
                ], boolean);
            }
            if (type.TryGetUnion() is { } union)
            {
                // No matching case on either side represents the default carrier.
                BoundExpression result = Bool(true);
                foreach (var caseType in union.DeclaredCaseTypes.Reverse())
                {
                    var (leftTest, leftValue) = Case(left, caseType);
                    var (rightTest, rightValue) = Case(right, caseType);
                    BoundExpression payloadEqual = Bool(true);
                    foreach (var property in CaseProperties(caseType)!)
                        payloadEqual = Binary(SyntaxKind.AmpersandAmpersandToken, payloadEqual,
                            Equal(Property(leftValue, property), Property(rightValue, property)));
                    result = new BoundIfExpression(leftTest,
                        Binary(SyntaxKind.AmpersandAmpersandToken, rightTest, payloadEqual),
                        new BoundIfExpression(rightTest, Bool(false), result));
                }
                return result;
            }
            if (TryImportedValueContract(type, equatable, out var componentEquals, out _))
                return new BoundInvocationExpression(componentEquals!, [right], left, requiresReceiverAddress: true);
            if (type.SpecialType is SpecialType.System_Boolean or SpecialType.System_Int32 or SpecialType.System_Int64)
                return Binary(SyntaxKind.EqualsEqualsToken, left, right);
            if (type.TypeKind == TypeKind.Enum)
                return Binary(SyntaxKind.EqualsEqualsToken, ToInt(left), ToInt(right));
            var typed = (type.IsValueType || type.SpecialType == SpecialType.System_String ? type.GetMembers("Equals") : []).OfType<IMethodSymbol>().FirstOrDefault(m => !m.IsStatic && m.Parameters.Length == 1 &&
                SymbolEqualityComparer.Default.Equals(m.Parameters[0].Type.GetNonNullableType(), type));
            BoundExpression equal = typed is not null
                ? new BoundInvocationExpression(typed, [right], left, requiresReceiverAddress: type.IsValueType)
                : new BoundInvocationExpression(objectEquals, [Box(right)], Box(left));
            if (type.IsReferenceType)
                equal = new BoundIfExpression(Null(left), Null(right),
                    Binary(SyntaxKind.AmpersandAmpersandToken, Not(Null(right)), equal));
            return equal;
        }
        var fields = owner.GetMembers().OfType<SourcePropertySymbol>().Where(p => !p.IsStatic && p.BackingField is not null).ToArray();
        BoundExpression ValuesEqual(BoundExpression left, BoundExpression right)
        {
            BoundExpression result = Bool(true);
            foreach (var property in fields)
                result = Binary(SyntaxKind.AmpersandAmpersandToken, result,
                    Equal(new BoundFieldAccess(left, property.BackingField!), new BoundFieldAccess(right, property.BackingField!)));
            return result;
        }
        BoundExpression Hash(BoundExpression value)
        {
            if (value.Type is ITypeParameterSymbol)
            {
                var local = Temporary(obj);
                var boxed = new BoundLocalAccess(local);
                return new BoundBlockExpression([
                    Declare(local, Box(value)),
                    new BoundExpressionStatement(new BoundIfExpression(Null(boxed), Int(0),
                        new BoundInvocationExpression(objectHash, [], boxed)))
                ], integer);
            }
            if (value.Type.TryGetUnion() is { } union)
            {
                BoundExpression result = Int(0);
                foreach (var caseType in union.DeclaredCaseTypes.Reverse())
                {
                    var (test, payload) = Case(value, caseType);
                    BoundExpression caseHash = Int(caseType.Ordinal + 1);
                    foreach (var property in CaseProperties(caseType)!)
                        caseHash = new BoundInvocationExpression(combine,
                            [caseHash, Hash(Property(payload, property))]);
                    result = new BoundIfExpression(test, caseHash, result);
                }
                return result;
            }
            if (TryImportedValueContract(value.Type, equatable, out _, out var componentHash))
                return new BoundInvocationExpression(componentHash!, [], value, requiresReceiverAddress: true);
            if (value.Type.SpecialType == SpecialType.System_Int32)
                return value;
            if (value.Type.SpecialType == SpecialType.System_Boolean)
                return new BoundIfExpression(value, Int(1), Int(0));
            if (value.Type.SpecialType == SpecialType.System_Int64)
                return Binary(SyntaxKind.CaretToken, ToInt(value),
                    ToInt(Binary(SyntaxKind.GreaterThanGreaterThanToken, value, Int(32))));
            if (value.Type.TypeKind == TypeKind.Enum)
                return ToInt(value);
            var directHash = value.Type.GetNonNullableType().GetMembers("GetHashCode").OfType<IMethodSymbol>()
                .FirstOrDefault(m => !m.IsStatic && m.Parameters.IsEmpty && m.IsOverride);
            BoundExpression hash = directHash is null
                ? new BoundInvocationExpression(objectHash, [], Box(value))
                : new BoundInvocationExpression(directHash, [], value, requiresReceiverAddress: value.Type.IsValueType);
            return value.Type.IsReferenceType ? new BoundIfExpression(Null(value), Int(0), hash) : hash;
        }
        var statements = new List<BoundStatement>();
        if (method.Name == "GetHashCode")
        {
            BoundExpression hash = Int(0);
            foreach (var property in fields)
                hash = new BoundInvocationExpression(combine,
                    [hash, Hash(new BoundFieldAccess(new BoundSelfExpression(owner), property.BackingField!))]);
            statements.Add(new BoundReturnStatement(hash));
        }
        else
        {
            BoundExpression left = method.IsStatic ? new BoundParameterAccess(method.Parameters[0]) : new BoundSelfExpression(owner);
            BoundExpression right = new BoundParameterAccess(method.Parameters[method.IsStatic ? 1 : 0]);
            if (!method.IsStatic && method.Parameters[0].Type.GetNonNullableType().SpecialType == SpecialType.System_Object)
            {
                var local = new SourceLocalSymbol("other", owner, true, method, owner, owner.ContainingNamespace, method.Locations.ToArray(), []);
                var pattern = new BoundIsPatternExpression(right, new BoundDeclarationPattern(owner, new BoundSingleVariableDesignator(local)), boolean);
                statements.Add(new BoundIfStatement(Not(pattern), new BoundReturnStatement(Bool(false))));
                right = new BoundLocalAccess(local);
            }
            var unequal = method.Name == "op_Inequality";
            if (owner.IsReferenceType)
            {
                statements.Add(new BoundIfStatement(new BoundInvocationExpression(referenceEquals, [Box(left), Box(right)]), new BoundReturnStatement(Bool(!unequal))));
                statements.Add(new BoundIfStatement(Binary(SyntaxKind.BarBarToken, Null(left), Null(right)), new BoundReturnStatement(Bool(unequal))));
            }
            var equal = ValuesEqual(left, right);
            statements.Add(new BoundReturnStatement(unequal ? Not(equal) : equal));
        }
        return new SourceCallablePlan(method, anchor, anchor, owner, method.MetadataName, signature,
            PreparedBody: Lowerer.LowerBlock(method, new BoundBlockStatement(statements)));
    }

    private static bool ContainsTypeParameter(ITypeSymbol type) =>
        type is ITypeParameterSymbol || type is INamedTypeSymbol named &&
            named.TypeArguments.Any(ContainsTypeParameter);

    // Do not emit apparently structural methods whose component fallback would
    // silently compare fresh value boxes by identity.
    private static bool SupportsValueSemantics(ITypeSymbol type, HashSet<ITypeSymbol> visited, INamedTypeSymbol? equatable)
    {
        if (type is not INamedTypeSymbol { OriginalDefinition: SourceNamedTypeSymbol { IsRecord: true } } record)
            return false;
        if (!visited.Add(type))
            return true;
        return record.GetMembers().OfType<IFieldSymbol>().Where(f => !f.IsStatic)
            .All(f => SupportsComponent(f.Type, visited, equatable));
    }

    internal static IPropertySymbol[]? CaseProperties(IUnionCaseTypeSymbol caseType)
    {
        var properties = new List<IPropertySymbol>();
        foreach (var parameter in caseType.ConstructorParameters)
        {
            var candidates = caseType.GetMembers(UnionFacts.GetCasePropertyName(parameter.Name))
                .OfType<IPropertySymbol>().Where(p => !p.IsStatic && p.Parameters.IsEmpty &&
                    p.GetMethod is { DeclaredAccessibility: Accessibility.Public } &&
                    SymbolEqualityComparer.Default.Equals(p.Type, parameter.Type)).Take(2).ToArray();
            if (candidates.Length != 1)
                return null;
            properties.Add(candidates[0]);
        }
        return properties.ToArray();
    }

    private static bool SupportsComponent(ITypeSymbol type, HashSet<ITypeSymbol> visited, INamedTypeSymbol? equatable)
    {
        var component = type.GetNonNullableType();
        if (component is ITypeParameterSymbol)
            return equatable is not null;
        if (component.TryGetUnion() is { } union)
        {
            if (union.DeclaredCaseTypes.IsEmpty || !visited.Add(component))
                return false;
            var supported = union.DeclaredCaseTypes.All(c => CaseProperties(c) is { } properties &&
                properties.All(p => SupportsComponent(p.Type, visited, equatable)));
            visited.Remove(component);
            return supported;
        }
        if (component is INamedTypeSymbol { OriginalDefinition: SourceNamedTypeSymbol { IsRecord: true } })
            return SupportsValueSemantics(component, visited, equatable);
        if (TryImportedValueContract(component, equatable, out _, out _))
            return true;
        if (component.IsReferenceType)
        {
            // A separately authored typed equality contract needs its matching
            // comparer policy; Object dispatch alone is not a substitute.
            return component.SpecialType == SpecialType.System_String || !component.AllInterfaces.Any(i =>
                SymbolEqualityComparer.Default.Equals(i.OriginalDefinition, equatable));
        }
        return component.SpecialType is SpecialType.System_Boolean or SpecialType.System_Int32 or
            SpecialType.System_Int64 or SpecialType.System_Char or SpecialType.System_Single or
            SpecialType.System_Double || component.TypeKind == TypeKind.Enum;
    }

    // Reuse an explicit imported self-equality contract; do not infer a record
    // from method names or inspect its private fields. Explicit mappings require
    // full interface implementation resolution and remain outside this policy.
    private static bool TryImportedValueContract(ITypeSymbol type, INamedTypeSymbol? equatable,
        out IMethodSymbol? equals, out IMethodSymbol? hash)
    {
        equals = hash = null;
        if (equatable is null || type is not INamedTypeSymbol { OriginalDefinition: NativeNamedTypeSymbol, IsValueType: true, IsRefLikeType: false } native || native.SpecialType != SpecialType.None ||
            !native.AllInterfaces.Any(i => SymbolEqualityComparer.Default.Equals(i.OriginalDefinition, equatable) &&
                i.TypeArguments.Length == 1 && SymbolEqualityComparer.Default.Equals(i.TypeArguments[0], native)))
            return false;
        var methods = native.GetMembers().OfType<IMethodSymbol>().ToArray();
        if (methods.Any(m => m.OriginalDefinition is NativeMethodSymbol { HasExplicitInterfaceMappings: true }))
            return false;
        bool Callable(IMethodSymbol m) => !m.IsStatic && !m.IsAbstract && !m.IsGenericMethod &&
            m.DeclaredAccessibility == Accessibility.Public &&
            SymbolEqualityComparer.Default.Equals(m.ContainingType, native);
        var equality = methods.Where(m => Callable(m) && m.Name == "Equals" &&
            m.ReturnType.SpecialType == SpecialType.System_Boolean && m.Parameters.Length == 1 &&
            m.Parameters[0].RefKind == RefKind.None &&
            SymbolEqualityComparer.Default.Equals(m.Parameters[0].Type, native)).Take(2).ToArray();
        var hashes = methods.Where(m => Callable(m) && m.Name == "GetHashCode" &&
            m.ReturnType.SpecialType == SpecialType.System_Int32 && m.Parameters.IsEmpty).Take(2).ToArray();
        if (equality.Length != 1 || hashes.Length != 1)
            return false;
        equals = equality[0];
        hash = hashes[0];
        return true;
    }
}
