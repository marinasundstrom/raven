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
        if (!SupportsValueSemantics(owner, new HashSet<ITypeSymbol>(SymbolEqualityComparer.Default)))
            return null;
        if (!CallableSignature.TryCreate(method, out var signature, capabilities))
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
        BoundExpression Equal(BoundExpression left, BoundExpression right)
        {
            var type = left.Type.GetNonNullableType();
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
        var statements = new List<BoundStatement>();
        if (method.Name == "GetHashCode")
        {
            BoundExpression hash = Int(0);
            foreach (var property in fields)
            {
                var value = new BoundFieldAccess(new BoundSelfExpression(owner), property.BackingField!);
                // Virtual Object dispatch handles primitive boxes and user overrides.
                var directHash = value.Type.GetNonNullableType().GetMembers("GetHashCode").OfType<IMethodSymbol>()
                    .FirstOrDefault(m => !m.IsStatic && m.Parameters.IsEmpty && m.IsOverride);
                BoundExpression part = directHash is null
                    ? new BoundInvocationExpression(objectHash, [], Box(value))
                    : new BoundInvocationExpression(directHash, [], value, requiresReceiverAddress: value.Type.IsValueType);
                if (value.Type.SpecialType == SpecialType.System_Int32)
                    part = value;
                else if (value.Type.SpecialType == SpecialType.System_Boolean)
                    part = new BoundIfExpression(value, Int(1), Int(0));
                else if (value.Type.SpecialType == SpecialType.System_Int64)
                    part = Binary(SyntaxKind.CaretToken, ToInt(value),
                        ToInt(Binary(SyntaxKind.GreaterThanGreaterThanToken, value, Int(32))));
                else if (value.Type.TypeKind == TypeKind.Enum)
                    part = ToInt(value);
                else if (value.Type.IsReferenceType)
                    part = new BoundIfExpression(Null(value), Int(0), part);
                hash = new BoundInvocationExpression(combine, [hash, part]);
            }
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

    // Storage-only records remain usable. Do not emit apparently structural methods
    // whose component fallback would silently compare fresh value boxes by identity.
    private static bool SupportsValueSemantics(ITypeSymbol type, HashSet<ITypeSymbol> visited)
    {
        if (type is not SourceNamedTypeSymbol { IsRecord: true } record || !record.TypeParameters.IsEmpty)
            return false;
        if (!visited.Add(type))
            return true;
        foreach (var field in record.GetMembers().OfType<IFieldSymbol>().Where(f => !f.IsStatic))
        {
            var component = field.Type.GetNonNullableType();
            if (component is ITypeParameterSymbol)
                return false;
            if (component is SourceNamedTypeSymbol { IsRecord: true })
            {
                if (!SupportsValueSemantics(component, visited))
                    return false;
                continue;
            }
            if (component.IsReferenceType)
            {
                // A separately authored typed equality contract needs its matching
                // comparer policy; Object dispatch alone is not a substitute.
                if (component.SpecialType != SpecialType.System_String && component.AllInterfaces.Any(i =>
                    i.OriginalDefinition.ToFullyQualifiedMetadataName() == "System.EquatableTo`1"))
                    return false;
                continue;
            }
            if (component.SpecialType is SpecialType.System_Boolean or SpecialType.System_Int32 or
                SpecialType.System_Int64 or SpecialType.System_Char)
                continue;
            if (component.TypeKind == TypeKind.Enum)
                continue;
            return false;
        }
        return true;
    }
}
