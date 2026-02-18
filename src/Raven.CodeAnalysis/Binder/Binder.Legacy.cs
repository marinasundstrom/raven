using System.Collections.Generic;
using System.Linq;

using Raven.CodeAnalysis.Symbols;
using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis;

internal abstract partial class Binder
{
    // Legacy fallback path used by BindTypeSyntaxDirect during migration.
    private ITypeSymbol ResolveTypeInternalLegacy(TypeSyntax typeSyntax, RefKind? refKindHint)
    {
        if (typeSyntax is NullTypeSyntax)
            return ApplyRefKindHint(Compilation.NullTypeSymbol, refKindHint);

        if (typeSyntax is LiteralTypeSyntax literalType)
        {
            var token = literalType.Token;
            var value = token.Value ?? token.Text!;
            ITypeSymbol underlying = value switch
            {
                int => Compilation.GetSpecialType(SpecialType.System_Int32),
                long => Compilation.GetSpecialType(SpecialType.System_Int64),
                float => Compilation.GetSpecialType(SpecialType.System_Single),
                double => Compilation.GetSpecialType(SpecialType.System_Double),
                bool => Compilation.GetSpecialType(SpecialType.System_Boolean),
                char => Compilation.GetSpecialType(SpecialType.System_Char),
                string => Compilation.GetSpecialType(SpecialType.System_String),
                _ => Compilation.ErrorTypeSymbol
            };

            return ApplyRefKindHint(new LiteralTypeSymbol(underlying, value, Compilation), refKindHint);
        }

        if (typeSyntax is ByRefTypeSyntax refType)
        {
            var elementType = ResolveTypeInternalLegacy(refType.ElementType, refKindHint: null);
            return new RefTypeSymbol(elementType);
        }

        if (typeSyntax is PointerTypeSyntax pointer)
        {
            if (!IsUnsafeEnabled)
            {
                _diagnostics.ReportPointerTypeRequiresUnsafe(pointer.GetLocation());
                return ApplyRefKindHint(Compilation.ErrorTypeSymbol, refKindHint);
            }

            var elementType = ResolveTypeInternalLegacy(pointer.ElementType, refKindHint: null);
            var pointerType = Compilation.CreatePointerTypeSymbol(elementType);
            return ApplyRefKindHint(pointerType, refKindHint);
        }

        if (typeSyntax is NullableTypeSyntax nb)
        {
            var elementType = ResolveTypeInternalLegacy(nb.ElementType, refKindHint: null);
            if (elementType is ITypeParameterSymbol typeParameter &&
                (typeParameter.ConstraintKind & TypeParameterConstraintKind.NotNull) != 0)
            {
                ReportNotNullConstraintViolation(typeParameter, nb.GetLocation());
                return ApplyRefKindHint(Compilation.ErrorTypeSymbol, refKindHint);
            }

            return ApplyRefKindHint(elementType.MakeNullable(), refKindHint);
        }

        if (typeSyntax is UnionTypeSyntax ut)
        {
            var types = new List<ITypeSymbol>();
            foreach (var t in ut.Types)
            {
                if (t is NullableTypeSyntax nt)
                {
                    _diagnostics.ReportNullableTypeInUnion(nt.GetLocation());
                    return ApplyRefKindHint(Compilation.ErrorTypeSymbol, refKindHint);
                }

                types.Add(ResolveTypeInternalLegacy(t, refKindHint: null));
            }

            var normalized = TypeSymbolNormalization.NormalizeUnion(
                types,
                _diagnostics,
                ut.GetLocation(),
                Compilation.ErrorTypeSymbol);
            return ApplyRefKindHint(normalized, refKindHint);
        }

        if (typeSyntax is ArrayTypeSyntax arrayTypeSyntax)
        {
            var currentElementType = ResolveTypeInternalLegacy(arrayTypeSyntax.ElementType, refKindHint: null);

            foreach (var rankSpecifier in arrayTypeSyntax.RankSpecifiers)
            {
                var rank = rankSpecifier.CommaTokens.Count + 1;
                currentElementType = Compilation.CreateArrayTypeSymbol(currentElementType, rank);
            }

            return ApplyRefKindHint(currentElementType, refKindHint);
        }

        if (typeSyntax is TupleTypeSyntax tupleTypeSyntax)
        {
            var elements = tupleTypeSyntax.Elements
                .Select(e => (e.NameColon?.Name.ToString(), ResolveTypeInternalLegacy(e.Type, refKindHint: null)))
                .ToArray();
            return ApplyRefKindHint(Compilation.CreateTupleTypeSymbol(elements), refKindHint);
        }

        if (typeSyntax is FunctionTypeSyntax functionTypeSyntax)
        {
            var parameterTypes = new List<ITypeSymbol>();

            if (functionTypeSyntax.ParameterList is not null)
            {
                foreach (var parameter in functionTypeSyntax.ParameterList.Parameters)
                {
                    parameterTypes.Add(ResolveTypeInternalLegacy(parameter, refKindHint: null));
                }
            }
            else if (functionTypeSyntax.Parameter is not null)
            {
                parameterTypes.Add(ResolveTypeInternalLegacy(functionTypeSyntax.Parameter, refKindHint: null));
            }

            var returnType = ResolveTypeInternalLegacy(functionTypeSyntax.ReturnType, refKindHint: null);
            var delegateType = Compilation.CreateFunctionTypeSymbol(parameterTypes.ToArray(), returnType);
            return ApplyRefKindHint(delegateType, refKindHint);
        }

        if (typeSyntax is PredefinedTypeSyntax predefinedTypeSyntax)
            return ApplyRefKindHint(Compilation.ResolvePredefinedType(predefinedTypeSyntax), refKindHint);

        if (typeSyntax is UnitTypeSyntax)
            return ApplyRefKindHint(Compilation.GetSpecialType(SpecialType.System_Unit), refKindHint);

        if (typeSyntax is IdentifierNameSyntax ident)
        {
            if (ident.Identifier.IsMissing)
                return ApplyRefKindHint(Compilation.ErrorTypeSymbol, refKindHint);

            var type = LookupType(ident.Identifier.ValueText);
            if (type is INamedTypeSymbol named)
            {
                if (named.IsAlias)
                    return ApplyAccessibilityAndRefKind(named, ident.Identifier.GetLocation(), refKindHint);

                if (named.Arity > 0 && named.IsUnboundGenericType)
                {
                    var zeroArity = FindAccessibleNamedType(ident.Identifier.ValueText, 0);
                    if (zeroArity is null)
                    {
                        _diagnostics.ReportTypeRequiresTypeArguments(named.Name, named.Arity, ident.Identifier.GetLocation());
                        return ApplyRefKindHint(Compilation.ErrorTypeSymbol, refKindHint);
                    }

                    return ApplyAccessibilityAndRefKind(zeroArity, ident.Identifier.GetLocation(), refKindHint);
                }

                var normalized = NormalizeDefinition(named);
                if (!IsSymbolAccessible(normalized))
                    return ReportInaccessibleType(normalized, ident.Identifier.GetLocation(), refKindHint);

                return ApplyRefKindHint(normalized, refKindHint);
            }

            if (type is not null)
            {
                if (!IsSymbolAccessible(type))
                    return ReportInaccessibleType(type, ident.Identifier.GetLocation(), refKindHint);

                return ApplyRefKindHint(type, refKindHint);
            }
        }

        if (typeSyntax is GenericNameSyntax generic)
        {
            var arity = ComputeGenericArity(generic);
            var typeArgs = ResolveGenericTypeArguments(generic);

            var symbol = FindNamedTypeForGeneric(generic, arity);

            if (symbol is null)
            {
                _diagnostics.ReportTheNameDoesNotExistInTheCurrentContext(generic.Identifier.ValueText, generic.GetLocation() ?? Location.None);
                return ApplyRefKindHint(Compilation.ErrorTypeSymbol, refKindHint);
            }

            if (!ValidateTypeArgumentConstraints(symbol, typeArgs, i => GetTypeArgumentLocation(generic.TypeArgumentList.Arguments, generic.GetLocation(), i), symbol.ToDisplayString(SymbolDisplayFormat.MinimallyQualifiedFormat)))
                return ApplyRefKindHint(Compilation.ErrorTypeSymbol, refKindHint);

            var constructed = TryConstructGeneric(symbol, typeArgs, arity);
            if (constructed is not null)
                return ApplyAccessibilityAndRefKind(constructed, generic.Identifier.GetLocation(), refKindHint);

            return ApplyAccessibilityAndRefKind(symbol, generic.Identifier.GetLocation(), refKindHint);
        }

        if (typeSyntax is QualifiedNameSyntax qualified)
        {
            var symbol = ResolveQualifiedType(qualified);
            if (symbol is not null)
            {
                if (!IsSymbolAccessible(symbol))
                    return ReportInaccessibleType(symbol, qualified.GetLocation(), refKindHint);

                return ApplyRefKindHint(symbol, refKindHint);
            }
        }

        var name = typeSyntax switch
        {
            IdentifierNameSyntax id => id.Identifier.ValueText,
            _ => typeSyntax.ToString()
        };

        _diagnostics.ReportTheNameDoesNotExistInTheCurrentContext(name, typeSyntax.GetLocation() ?? Location.None);
        return ApplyRefKindHint(Compilation.ErrorTypeSymbol, refKindHint);
    }

    protected ISymbol? ResolveName(NameSyntax name)
    {
        return name switch
        {
            IdentifierNameSyntax id => LookupSymbol(id.Identifier.ValueText)
                ?? (ISymbol?)LookupNamespace(id.Identifier.ValueText)
                ?? LookupType(id.Identifier.ValueText),
            GenericNameSyntax gen => ResolveGenericName(gen),
            QualifiedNameSyntax qn => ResolveQualifiedName(qn),
            _ => null
        };
    }

    private ISymbol? ResolveGenericName(GenericNameSyntax gen)
    {
        var arity = ComputeGenericArity(gen);
        var typeArgs = ResolveGenericTypeArguments(gen);

        var symbol = FindNamedTypeForGeneric(gen, arity);

        if (symbol is null)
            return null;

        if (!ValidateTypeArgumentConstraints(symbol, typeArgs, i => GetTypeArgumentLocation(gen.TypeArgumentList.Arguments, gen.GetLocation(), i), symbol.ToDisplayString(SymbolDisplayFormat.MinimallyQualifiedFormat)))
            return Compilation.ErrorTypeSymbol;

        return TryConstructGeneric(symbol, typeArgs, arity) ?? symbol;
    }

    private ISymbol? ResolveQualifiedName(QualifiedNameSyntax qn)
    {
        var left = ResolveName(qn.Left);

        if (left is INamespaceSymbol ns)
        {
            if (qn.Right is IdentifierNameSyntax id)
                return (ISymbol?)ns.LookupNamespace(id.Identifier.ValueText)
                    ?? SelectByArity(ns.GetMembers(id.Identifier.ValueText)
                        .OfType<INamedTypeSymbol>(), 0)
                    ?? ns.LookupType(id.Identifier.ValueText);

            if (qn.Right is GenericNameSyntax gen)
                return ResolveGenericMember(ns, gen);

            return null;
        }

        if (left is ITypeSymbol type)
        {
            if (qn.Right is IdentifierNameSyntax id)
                return SelectByArity(type.GetMembers(id.Identifier.ValueText)
                    .OfType<INamedTypeSymbol>(), 0);

            if (qn.Right is GenericNameSyntax gen)
                return ResolveGenericMember(type, gen);
        }

        return null;
    }
}
