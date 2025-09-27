using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;

using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;

namespace BoundNodeGenerator;

static class SpecificationLoader
{
    public static IReadOnlyList<BoundNodeModel> LoadBoundNodes(string directory)
    {
        var models = new Dictionary<string, BoundNodeModel>(StringComparer.Ordinal);

        foreach (var file in Directory.EnumerateFiles(directory, "*.cs", SearchOption.AllDirectories))
        {
            if (IsGenerated(file))
                continue;

            var text = File.ReadAllText(file);
            var root = CSharpSyntaxTree.ParseText(text).GetRoot();

            foreach (var classDeclaration in root.DescendantNodes().OfType<ClassDeclarationSyntax>())
            {
                if (!classDeclaration.Modifiers.Any(static m => m.IsKind(SyntaxKind.PartialKeyword)))
                    continue;

                if (!IsBoundNodeClass(classDeclaration))
                    continue;

                var namespaceName = SyntaxUtilities.GetNamespace(classDeclaration);
                if (namespaceName is null)
                    continue;

                var accessibility = SyntaxUtilities.GetAccessibility(classDeclaration.Modifiers);
                var isAbstract = classDeclaration.Modifiers.Any(static m => m.IsKind(SyntaxKind.AbstractKeyword));

                var ctor = classDeclaration.Members
                    .OfType<ConstructorDeclarationSyntax>()
                    .Where(c => c.Identifier.Text == classDeclaration.Identifier.Text)
                    .OrderByDescending(c => c.ParameterList.Parameters.Count)
                    .FirstOrDefault();

                var parameters = ctor is null
                    ? new List<ParameterModel>()
                    : ctor.ParameterList.Parameters.Select(CreateParameterModel).ToList();

                var model = new BoundNodeModel(
                    classDeclaration.Identifier.Text,
                    namespaceName,
                    accessibility,
                    isAbstract,
                    parameters);

                models[$"{namespaceName}.{model.Name}"] = model;
            }
        }

        return models.Values
            .OrderBy(m => m.Namespace, StringComparer.Ordinal)
            .ThenBy(m => m.Name, StringComparer.Ordinal)
            .ToList();
    }

    public static IReadOnlyList<SymbolClassModel> LoadSymbolClasses(string directory)
    {
        var models = new Dictionary<string, SymbolClassModel>(StringComparer.Ordinal);

        foreach (var file in Directory.EnumerateFiles(directory, "*.cs", SearchOption.AllDirectories))
        {
            if (IsGenerated(file))
                continue;

            var text = File.ReadAllText(file);
            var root = CSharpSyntaxTree.ParseText(text).GetRoot();

            foreach (var classDeclaration in root.DescendantNodes().OfType<ClassDeclarationSyntax>())
            {
                if (!classDeclaration.Modifiers.Any(static m => m.IsKind(SyntaxKind.PartialKeyword)))
                    continue;

                if (classDeclaration.Identifier.Text.Contains("Synthesized", StringComparison.Ordinal))
                    continue;

                if (!InheritsFromSymbol(classDeclaration))
                    continue;

                var namespaceName = SyntaxUtilities.GetNamespace(classDeclaration);
                if (namespaceName is null)
                    continue;

                var accessibility = SyntaxUtilities.GetAccessibility(classDeclaration.Modifiers);
                var model = new SymbolClassModel(classDeclaration.Identifier.Text, namespaceName, accessibility);
                models[$"{namespaceName}.{model.Name}"] = model;
            }
        }

        return models.Values
            .OrderBy(m => m.Namespace, StringComparer.Ordinal)
            .ThenBy(m => m.Name, StringComparer.Ordinal)
            .ToList();
    }

    public static IReadOnlyList<SymbolInterfaceModel> LoadSymbolInterfaces(string directory)
    {
        var interfaces = new Dictionary<string, SymbolInterfaceModel>(StringComparer.Ordinal);

        foreach (var file in Directory.EnumerateFiles(directory, "*.cs", SearchOption.AllDirectories))
        {
            if (IsGenerated(file))
                continue;

            var text = File.ReadAllText(file);
            var root = CSharpSyntaxTree.ParseText(text).GetRoot();

            foreach (var interfaceDeclaration in root.DescendantNodes().OfType<InterfaceDeclarationSyntax>())
            {
                var name = interfaceDeclaration.Identifier.Text;
                if (name.Equals("ISymbol", StringComparison.Ordinal))
                    continue;

                if (!name.EndsWith("Symbol", StringComparison.Ordinal))
                    continue;

                if (!ImplementsSymbol(interfaceDeclaration))
                    continue;

                var namespaceName = SyntaxUtilities.GetNamespace(interfaceDeclaration);
                if (namespaceName is null)
                    continue;

                interfaces[$"{namespaceName}.{name}"] = new SymbolInterfaceModel(name, namespaceName);
            }
        }

        return interfaces.Values
            .OrderBy(m => m.Namespace, StringComparer.Ordinal)
            .ThenBy(m => m.Name, StringComparer.Ordinal)
            .ToList();
    }

    private static ParameterModel CreateParameterModel(ParameterSyntax parameter)
    {
        var rawType = parameter.Type?.ToString() ?? "object";
        var isNullable = rawType.EndsWith("?", StringComparison.Ordinal);
        var nonNullableType = isNullable ? rawType[..^1] : rawType;

        var propertyName = NameHelpers.ToPascalCase(parameter.Identifier.ValueText);
        var parameterName = NameHelpers.EscapeIdentifier(NameHelpers.ToCamelCase(propertyName));

        var kind = DetermineParameterKind(nonNullableType, out var elementType, out var elementNullable);

        return new ParameterModel(
            propertyName,
            parameterName,
            rawType,
            nonNullableType,
            isNullable,
            kind,
            elementType,
            elementNullable);
    }

    private static ParameterKind DetermineParameterKind(string type, out string? elementType, out bool elementNullable)
    {
        elementType = null;
        elementNullable = false;
        var trimmed = type.Trim();

        if (TryGetGenericElement(trimmed, out var genericName, out var innerType))
        {
            var element = innerType.Trim();
            var nullable = element.EndsWith("?", StringComparison.Ordinal);
            if (nullable)
                element = element[..^1];

            elementType = element;
            elementNullable = nullable;

            if (IsBoundNodeType(element))
                return ParameterKind.BoundNodeList;

            if (IsSymbolType(element))
                return ParameterKind.SymbolList;

            return ParameterKind.Other;
        }

        if (IsBoundNodeType(trimmed))
            return ParameterKind.BoundNode;

        if (IsSymbolType(trimmed))
            return ParameterKind.Symbol;

        return ParameterKind.Other;
    }

    private static bool TryGetGenericElement(string type, out string genericName, out string innerType)
    {
        genericName = string.Empty;
        innerType = string.Empty;

        var angleIndex = type.IndexOf('<');
        if (angleIndex < 0)
            return false;

        var closeIndex = type.LastIndexOf('>');
        if (closeIndex < angleIndex)
            return false;

        genericName = type[..angleIndex];
        innerType = type[(angleIndex + 1)..closeIndex];

        return genericName is "IEnumerable" or "ImmutableArray" or "IReadOnlyList" or "IList" or "List";
    }

    private static bool IsBoundNodeType(string type)
    {
        if (!type.StartsWith("Bound", StringComparison.Ordinal))
            return false;

        if (type.Contains("Visitor", StringComparison.Ordinal) ||
            type.Contains("Walker", StringComparison.Ordinal) ||
            type.Contains("Rewriter", StringComparison.Ordinal))
        {
            return false;
        }

        return type.EndsWith("Expression", StringComparison.Ordinal) ||
               type.EndsWith("Statement", StringComparison.Ordinal) ||
               type.EndsWith("Node", StringComparison.Ordinal) ||
               type.EndsWith("Pattern", StringComparison.Ordinal) ||
               type.EndsWith("Designator", StringComparison.Ordinal) ||
               type.EndsWith("Clause", StringComparison.Ordinal) ||
               type.EndsWith("Declarator", StringComparison.Ordinal);
    }

    private static bool IsSymbolType(string type)
        => type.EndsWith("Symbol", StringComparison.Ordinal) || type.Equals("ISymbol", StringComparison.Ordinal);

    private static bool IsBoundNodeClass(ClassDeclarationSyntax classDeclaration)
    {
        if (!classDeclaration.Identifier.Text.StartsWith("Bound", StringComparison.Ordinal))
            return false;

        if (classDeclaration.BaseList is null)
            return false;

        foreach (var baseType in classDeclaration.BaseList.Types)
        {
            var simpleName = SyntaxUtilities.GetSimpleName(baseType.Type);
            if (simpleName is null)
                continue;

            if (simpleName.Equals("BoundNode", StringComparison.Ordinal))
                return true;

            if (simpleName.StartsWith("Bound", StringComparison.Ordinal) &&
                !simpleName.Contains("Visitor", StringComparison.Ordinal) &&
                !simpleName.Contains("Walker", StringComparison.Ordinal) &&
                !simpleName.Contains("Rewriter", StringComparison.Ordinal))
            {
                return true;
            }
        }

        return false;
    }

    private static bool InheritsFromSymbol(ClassDeclarationSyntax classDeclaration)
    {
        if (classDeclaration.BaseList is null)
            return false;

        foreach (var baseType in classDeclaration.BaseList.Types)
        {
            var name = SyntaxUtilities.GetSimpleName(baseType.Type);
            if (name is null)
                continue;

            if (name.Equals("Symbol", StringComparison.Ordinal))
                return true;

            if (name.EndsWith("Symbol", StringComparison.Ordinal) && !name.Equals("SymbolVisitor", StringComparison.Ordinal))
                return true;
        }

        return false;
    }

    private static bool ImplementsSymbol(InterfaceDeclarationSyntax interfaceDeclaration)
    {
        if (interfaceDeclaration.BaseList is null)
            return false;

        foreach (var baseType in interfaceDeclaration.BaseList.Types)
        {
            var name = SyntaxUtilities.GetSimpleName(baseType.Type);
            if (name is null)
                continue;

            if (name.EndsWith("Symbol", StringComparison.Ordinal))
                return true;
        }

        return false;
    }

    private static bool IsGenerated(string filePath)
        => filePath.Contains($"{Path.DirectorySeparatorChar}Generated{Path.DirectorySeparatorChar}", StringComparison.Ordinal);
}

static class SyntaxUtilities
{
    public static string? GetNamespace(SyntaxNode node)
    {
        for (var current = node; current is not null; current = current.Parent)
        {
            switch (current)
            {
                case NamespaceDeclarationSyntax namespaceDeclaration:
                    return namespaceDeclaration.Name.ToString();
                case FileScopedNamespaceDeclarationSyntax fileScopedNamespaceDeclaration:
                    return fileScopedNamespaceDeclaration.Name.ToString();
            }
        }

        return null;
    }

    public static string GetAccessibility(SyntaxTokenList modifiers)
    {
        if (modifiers.Any(static m => m.IsKind(SyntaxKind.PublicKeyword)))
            return "public";

        if (modifiers.Any(static m => m.IsKind(SyntaxKind.PrivateKeyword)))
            return "private";

        var hasProtected = modifiers.Any(static m => m.IsKind(SyntaxKind.ProtectedKeyword));
        var hasInternal = modifiers.Any(static m => m.IsKind(SyntaxKind.InternalKeyword));

        if (hasProtected && hasInternal)
            return "protected internal";

        if (hasProtected)
            return "protected";

        if (hasInternal)
            return "internal";

        return "internal";
    }

    public static string? GetSimpleName(TypeSyntax type)
        => type switch
        {
            IdentifierNameSyntax identifierName => identifierName.Identifier.Text,
            QualifiedNameSyntax qualifiedName => qualifiedName.Right.Identifier.Text,
            GenericNameSyntax genericName => genericName.Identifier.Text,
            AliasQualifiedNameSyntax aliasQualified => aliasQualified.Name.Identifier.Text,
            _ => null,
        };
}
