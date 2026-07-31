using System;
using System.Linq;
using System.Reflection;

using Raven.CodeAnalysis.Syntax;

using Xunit;

namespace Raven.CodeAnalysis.Tests.Syntax;

public class GeneratedSyntaxNodeContractTests
{
    [Fact]
    public void RedNodeLazyCacheFields_ArePrivateToTheirDeclaringNode()
    {
        Type[] representativeHierarchyNodes =
        [
            typeof(BaseNamespaceDeclarationSyntax),
            typeof(NamespaceDeclarationSyntax),
            typeof(BaseMethodDeclarationSyntax),
            typeof(MethodDeclarationSyntax),
            typeof(BasePropertyDeclarationSyntax),
            typeof(PropertyDeclarationSyntax),
        ];

        foreach (var nodeType in representativeHierarchyNodes)
        {
            var cacheFields = nodeType
                .GetFields(BindingFlags.Instance | BindingFlags.NonPublic | BindingFlags.DeclaredOnly)
                .Where(field => field.Name.StartsWith('_'))
                .ToArray();

            Assert.NotEmpty(cacheFields);
            Assert.All(cacheFields, field => Assert.True(field.IsPrivate, $"{nodeType.Name}.{field.Name} must be private."));
        }
    }
}
