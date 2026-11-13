using System;
using System.Collections.Generic;
using System.Linq;

using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis;

// Concrete source ownership is classified by SourceSymbol; routing stays shared.
internal readonly record struct SourceAttributeDeclarationContext(
    bool IsNamespaceContainer,
    bool IsSourceConstructor,
    bool IsSourceUnionCase,
    bool IsSourceProperty,
    bool IsSourceEvent,
    bool HasFieldOnlyAssociatedProperty);

internal static class SourceAttributeDeclarationFacts
{
    public static IEnumerable<AttributeListSyntax> GetAttributeListsForDeclaration(Symbol owner, SourceAttributeDeclarationContext context, SyntaxNode syntax)
        => syntax switch
        {
            CompilationUnitSyntax compilationUnit when owner is AssemblySymbol => compilationUnit
                .AttributeLists
                .OfType<AttributeListSyntax>()
                .Where(static list => HasExplicitTarget(list, "assembly")),
            CompilationUnitSyntax compilationUnit when owner is ModuleSymbol => compilationUnit
                .AttributeLists
                .OfType<AttributeListSyntax>()
                .Where(static list => HasExplicitTarget(list, "module")),
            BaseNamespaceDeclarationSyntax namespaceDeclaration when context.IsNamespaceContainer =>
                namespaceDeclaration.AttributeLists,
            MemberDeclarationSyntax member when context.IsNamespaceContainer =>
                member.AttributeLists,
            TypeDeclarationSyntax { ParameterList: not null } typeDeclaration when context.IsSourceConstructor =>
                typeDeclaration.AttributeLists.Where(static list => HasExplicitTarget(list, "method")),
            BaseTypeDeclarationSyntax typeDeclaration when owner is TypeSymbol => typeDeclaration.AttributeLists,
            DelegateDeclarationSyntax delegateDeclaration when owner is TypeSymbol => delegateDeclaration.AttributeLists,
            CaseDeclarationSyntax caseDeclaration when context.IsSourceUnionCase => caseDeclaration.AttributeLists,
            EnumMemberDeclarationSyntax enumMember => enumMember.AttributeLists,
            MethodDeclarationSyntax methodDeclaration => methodDeclaration.AttributeLists,
            FunctionStatementSyntax functionStatement => functionStatement.AttributeLists,
            ConstructorDeclarationSyntax constructorDeclaration => constructorDeclaration.AttributeLists,
            ParameterlessConstructorDeclarationSyntax initDeclaration => initDeclaration.AttributeLists,
            InitializerBlockDeclarationSyntax initBlockDeclaration => initBlockDeclaration.AttributeLists,
            FinallyDeclarationSyntax finalDeclaration => finalDeclaration.AttributeLists,
            PropertyDeclarationSyntax propertyDeclaration => propertyDeclaration.AttributeLists,
            IndexerDeclarationSyntax indexerDeclaration => indexerDeclaration.AttributeLists,
            EventDeclarationSyntax eventDeclaration => eventDeclaration.AttributeLists,
            AccessorDeclarationSyntax accessorDeclaration => accessorDeclaration.AttributeLists,
            FieldDeclarationSyntax fieldDeclaration => fieldDeclaration.AttributeLists,
            ConstDeclarationSyntax constDeclaration => constDeclaration.AttributeLists,
            VariableDeclaratorSyntax { Parent: { Parent: FieldDeclarationSyntax field } } => field.AttributeLists,
            VariableDeclaratorSyntax { Parent: { Parent: ConstDeclarationSyntax constant } } => constant.AttributeLists,
            ParameterSyntax parameter => parameter.AttributeLists,
            TypeParameterSyntax parameter => parameter.AttributeLists,
            ArrowTypeClauseSyntax arrowTypeClause => arrowTypeClause.AttributeLists,
            _ => Enumerable.Empty<AttributeListSyntax>()
        };

    public static bool ShouldBindAttributeForOwner(Symbol owner, SourceAttributeDeclarationContext context, AttributeSyntax attribute)
    {
        if (attribute.Parent is not AttributeListSyntax list)
            return !context.IsNamespaceContainer;

        if (context.IsNamespaceContainer)
            return AttributeUsageHelper.IsNamespaceContainerAttributeDeclaration(list);

        // A property's default target is the property, not its synthesized field.
        // Field-only Raven storage declarations still own their unqualified annotations.
        if (owner is FieldSymbol { AssociatedSymbol: PropertySymbol or EventSymbol } &&
            list.Parent is PropertyDeclarationSyntax or EventDeclarationSyntax)
        {
            return HasExplicitTarget(list, "field") || list.Target is null &&
                context.HasFieldOnlyAssociatedProperty;
        }

        if (list.Target is null)
            return true;

        if (HasExplicitTarget(list, "class"))
            return false;

        if (owner is AssemblySymbol)
            return HasExplicitTarget(list, "assembly") && IsAssemblyAttributeDeclarationContext(list);

        if (owner is ModuleSymbol)
            return HasExplicitTarget(list, "module") && IsAssemblyAttributeDeclarationContext(list);

        if (owner is TypeSymbol &&
            list.Parent is TypeDeclarationSyntax { ParameterList: not null } &&
            HasExplicitTarget(list, "method"))
        {
            return false;
        }

        if (context.IsSourceProperty &&
            HasExplicitTarget(list, "field"))
        {
            return false;
        }

        if (context.IsSourceEvent &&
            HasExplicitTarget(list, "field"))
        {
            return false;
        }

        return true;
    }

    private static bool HasExplicitTarget(AttributeListSyntax list, string targetName)
        => string.Equals(
            list.Target?.Identifier.ValueText,
            targetName,
            StringComparison.OrdinalIgnoreCase);

    private static bool IsAssemblyAttributeDeclarationContext(AttributeListSyntax list)
        => list.Parent is CompilationUnitSyntax;
}
