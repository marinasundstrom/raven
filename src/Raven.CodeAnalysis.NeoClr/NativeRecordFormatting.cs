using Raven.CodeAnalysis.CodeGen.Portable;
using Raven.CodeAnalysis.Symbols;
using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.NeoClr;

// Preserve Raven's positional display syntax through the selected core's contracts.
internal static class NativeRecordFormatting
{
    internal static SourceCallablePlan? Create(Compilation compilation, IMethodSymbol method,
        RecordDeclarationSyntax anchor, EmissionCapabilities capabilities)
    {
        if (method.Name != "ToString" || !method.DeclaringSyntaxReferences.IsEmpty ||
            method.ContainingType is not SourceNamedTypeSymbol { IsRecord: true } owner ||
            !Supported(owner, new HashSet<ITypeSymbol>(SymbolEqualityComparer.Default)) ||
            !CallableSignature.TryCreate(method, out var signature, capabilities))
            return null;
        var text = compilation.GetSpecialType(SpecialType.System_String);
        var obj = compilation.GetSpecialType(SpecialType.System_Object);
        var builder = compilation.GetTypeByMetadataName("System.Text.StringBuilder");
        var ctor = builder?.Constructors.SingleOrDefault(m => !m.IsStatic && m.Parameters.IsEmpty);
        var append = builder?.GetMembers("Append").OfType<IMethodSymbol>().SingleOrDefault(m =>
            !m.IsStatic && m.Parameters.Length == 1 && m.Parameters[0].Type.SpecialType == SpecialType.System_String);
        var finish = builder?.GetMembers("ToString").OfType<IMethodSymbol>().SingleOrDefault(m => !m.IsStatic && m.Parameters.IsEmpty);
        var replace = text.GetMembers("Replace").OfType<IMethodSymbol>().SingleOrDefault(m =>
            !m.IsStatic && m.Parameters.Length == 2 && m.Parameters.All(p => p.Type.SpecialType == SpecialType.System_String));
        var objectFormatter = Formatter(obj);
        var referenceEquals = obj.GetMembers("ReferenceEquals").OfType<IMethodSymbol>().SingleOrDefault(m => m.IsStatic && m.Parameters.Length == 2);
        if (ctor is null || append is null || finish is null || replace is null || referenceEquals is null || objectFormatter is null)
            return null;
        var local = new SourceLocalSymbol("recordText", builder!, false, method, owner,
            owner.ContainingNamespace, method.Locations.ToArray(), []);
        var statements = new List<BoundStatement>
        {
            new BoundLocalDeclarationStatement([new BoundVariableDeclarator(local, new BoundObjectCreationExpression(ctor, []))])
        };
        BoundExpression Literal(string value) => new BoundLiteralExpression(BoundLiteralExpressionKind.StringLiteral, value, text);
        BoundStatement Append(BoundExpression value) => new BoundExpressionStatement(new BoundInvocationExpression(append, [value], new BoundLocalAccess(local)));
        void Quoted(BoundExpression value, string quote, List<BoundStatement> output)
        {
            output.Add(Append(Literal(quote)));
            var escaped = new BoundInvocationExpression(replace, [Literal("\\"), Literal("\\\\")], value);
            output.Add(Append(new BoundInvocationExpression(replace, [Literal(quote), Literal("\\" + quote)], escaped)));
            output.Add(Append(Literal(quote)));
        }
        var caseIndex = 0;
        void Value(BoundExpression value, List<BoundStatement> output)
        {
            var type = value.Type.GetNonNullableType();
            if (type.TryGetUnion() is { } union)
            {
                BoundStatement branch = Append(Literal("<Uninitialized>"));
                foreach (var caseType in union.DeclaredCaseTypes.Reverse())
                {
                    var payload = new SourceLocalSymbol("recordCase" + caseIndex++, caseType, false,
                        method, owner, owner.ContainingNamespace, method.Locations.ToArray(), []);
                    var test = new BoundIsPatternExpression(value,
                        new BoundDeclarationPattern(caseType, new BoundSingleVariableDesignator(payload)),
                        compilation.GetSpecialType(SpecialType.System_Boolean));
                    var body = new List<BoundStatement> { Append(Literal(union.Name + "." + caseType.Name)) };
                    var properties = NativeRecordBodies.CaseProperties(caseType)!;
                    if (properties.Length > 0)
                    {
                        body.Add(Append(Literal("(")));
                        for (var index = 0; index < properties.Length; index++)
                        {
                            if (index > 0)
                                body.Add(Append(Literal(", ")));
                            if (properties.Length > 1)
                                body.Add(Append(Literal(properties[index].Name + "=")));
                            Value(new BoundInvocationExpression(properties[index].GetMethod!, [],
                                new BoundLocalAccess(payload), requiresReceiverAddress: true), body);
                        }
                        body.Add(Append(Literal(")")));
                    }
                    branch = new BoundIfStatement(test, new BoundBlockStatement(body), branch);
                }
                output.Add(branch);
                return;
            }
            if (type is SourceNamedTypeSymbol { IsRecord: true } record)
            {
                Record(value, record, output);
                return;
            }
            if (type.SpecialType == SpecialType.System_String)
            {
                var quoted = new List<BoundStatement>();
                Quoted(value, "\"", quoted);
                var isNull = new BoundInvocationExpression(referenceEquals,
                    [new BoundConversionExpression(value, obj, compilation.ClassifyConversion(value.Type, obj, includeUserDefined: false)),
                     new BoundLiteralExpression(BoundLiteralExpressionKind.NullLiteral, null!, obj)]);
                output.Add(new BoundIfStatement(isNull, Append(Literal("null")), new BoundBlockStatement(quoted)));
            }
            else if (type.TypeKind == TypeKind.Enum)
            {
                // Reuse ordinary enum Object display, including flags and unnamed values.
                // The current runtime contract needs a box; do not duplicate its policy here.
                var boxed = new BoundConversionExpression(value, obj,
                    compilation.ClassifyConversion(value.Type, obj, includeUserDefined: false));
                output.Add(Append(new BoundInvocationExpression(objectFormatter, [], boxed)));
            }
            else if (type.SpecialType == SpecialType.System_Boolean)
                output.Add(Append(new BoundIfExpression(value, Literal("True"), Literal("False"))));
            else
            {
                var formatter = Formatter(type)!;
                var formatted = new BoundInvocationExpression(formatter, [], value, requiresReceiverAddress: true);
                if (type.SpecialType == SpecialType.System_Char)
                    Quoted(formatted, "'", output);
                else
                    output.Add(Append(formatted));
            }
        }
        void Record(BoundExpression value, SourceNamedTypeSymbol record, List<BoundStatement> output)
        {
            output.Add(Append(Literal(record.Name + " { ")));
            var properties = record.GetMembers().OfType<SourcePropertySymbol>().Where(p => !p.IsStatic && p.BackingField is not null).ToArray();
            for (var index = 0; index < properties.Length; index++)
            {
                var property = properties[index];
                output.Add(Append(Literal((index == 0 ? "" : ", ") + property.Name + " = ")));
                Value(new BoundInvocationExpression(property.GetMethod!, [], value, requiresReceiverAddress: true), output);
            }
            output.Add(Append(Literal(" }")));
        }
        Record(new BoundSelfExpression(owner), owner, statements);
        statements.Add(new BoundReturnStatement(new BoundInvocationExpression(finish, [], new BoundLocalAccess(local))));
        return new SourceCallablePlan(method, anchor, anchor, owner, method.MetadataName, signature,
            PreparedBody: Lowerer.LowerBlock(method, new BoundBlockStatement(statements)));
    }

    // Use a declared public contract, never an inherited Object fallback or private layout.
    private static IMethodSymbol? Formatter(ITypeSymbol type)
    {
        var methods = type.GetMembers("ToString").OfType<IMethodSymbol>().Where(m =>
            !m.IsStatic && !m.IsAbstract && !m.IsGenericMethod && m.Parameters.IsEmpty &&
            m.DeclaredAccessibility == Accessibility.Public &&
            m.ReturnType.SpecialType == SpecialType.System_String &&
            SymbolEqualityComparer.Default.Equals(m.ContainingType, type)).Take(2).ToArray();
        return methods.Length == 1 ? methods[0] : null;
    }

    private static bool Supported(ITypeSymbol type, HashSet<ITypeSymbol> path)
    {
        type = type.GetNonNullableType();
        if (type.TryGetUnion() is { } union)
        {
            if (union.DeclaredCaseTypes.IsEmpty || !path.Add(type))
                return false;
            var supported = union.DeclaredCaseTypes.All(c => NativeRecordBodies.CaseProperties(c) is { } properties &&
                properties.All(p => Supported(p.Type, path)));
            path.Remove(type);
            return supported;
        }
        if (type is SourceNamedTypeSymbol { IsRecord: true } record)
        {
            // Recursive reference graphs require a separate cycle/display policy.
            if (!record.IsValueType || !record.TypeParameters.IsEmpty || !path.Add(type))
                return false;
            var supported = record.GetMembers().OfType<IFieldSymbol>().Where(f => !f.IsStatic).All(f => Supported(f.Type, path));
            path.Remove(type);
            return supported;
        }
        if (type is INamedTypeSymbol { TypeKind: TypeKind.Enum, EnumUnderlyingType.SpecialType: SpecialType.System_Int32 } ||
            type.SpecialType is SpecialType.System_String or SpecialType.System_Boolean)
            return true;
        return (type.SpecialType is SpecialType.System_Int32 or SpecialType.System_Int64 or SpecialType.System_Char or
            SpecialType.System_Single or SpecialType.System_Double ||
            type is INamedTypeSymbol { OriginalDefinition: NativeNamedTypeSymbol, IsValueType: true, IsRefLikeType: false, SpecialType: SpecialType.None }) &&
            Formatter(type) is not null;
    }
}
