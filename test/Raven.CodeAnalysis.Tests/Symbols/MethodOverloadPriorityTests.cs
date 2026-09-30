using Raven.CodeAnalysis.Metadata;
using Raven.CodeAnalysis.Symbols;

namespace Raven.CodeAnalysis.Tests;

public sealed class MethodOverloadPriorityTests
{
    [Theory]
    [InlineData(false, 1)]
    [InlineData(true, 1)]
    [InlineData(false, null)]
    [InlineData(true, null)]
    [InlineData(false, -1)]
    [InlineData(true, -1)]
    public void NonPePriorityParticipatesInOverloadSelection(bool constructed, int? priority)
    {
        var compilation = Compilation.Create("test", [], TestMetadataReferences.Default);
        var objectType = compilation.GetSpecialType(SpecialType.System_Object);
        var stringType = compilation.GetSpecialType(SpecialType.System_String);
        var owner = new SourceNamedTypeSymbol("Owner", objectType, TypeKind.Class,
            compilation.Assembly, null, compilation.SourceGlobalNamespace, [], [], addAsMember: false);
        var broadDefinition = new ProviderMethod(compilation, owner, objectType, priority);
        var specific = new ProviderMethod(compilation, owner, stringType, null);
        IMethodSymbol broad = constructed ? new ConstructedMethodSymbol(broadDefinition, []) : broadDefinition;
        BoundArgument[] arguments = [new(new BoundLiteralExpression(BoundLiteralExpressionKind.StringLiteral, "test", stringType), RefKind.None, null)];

        foreach (var candidates in new[] { new IMethodSymbol[] { broad, specific }, [specific, broad] })
        {
            var result = OverloadResolver.ResolveOverload(candidates, arguments, compilation);
            Assert.True(result.Success);
            Assert.Same(priority > 0 ? broad : specific, result.Method);
        }
    }

    [Theory]
    [InlineData(false)]
    [InlineData(true)]
    public void PeProviderDecodesPriorityAndReportsAbsentAttribute(bool virtualMethod)
    {
        var reference = TestMetadataFactory.CreateFromSource("""
            import System.Runtime.CompilerServices.*
            public open class Library {
                [OverloadResolutionPriority(2)]
                public static func Preferred(value: object) -> int => 1
                public static func Ordinary(value: string) -> int => 2
            }
            """.Replace("public static func Preferred", virtualMethod ? "public virtual func Preferred" : "public static func Preferred"), "PriorityFixture");
        var compilation = Compilation.Create("test", [], [.. TestMetadataReferences.Default, reference]);
        var type = compilation.GetTypeByMetadataName("Library")!;
        var preferred = Assert.IsAssignableFrom<IMethodOverloadPriority>(type.GetMembers("Preferred").Single());
        var ordinary = Assert.IsAssignableFrom<IMethodOverloadPriority>(type.GetMembers("Ordinary").Single());
        Assert.True(preferred.TryGetOverloadResolutionPriority(out var priority));
        Assert.Equal(2, priority);
        Assert.False(ordinary.TryGetOverloadResolutionPriority(out _));
    }

    private sealed class ProviderMethod : SourceMethodSymbol, IMethodOverloadPriority
    {
        private readonly int? _priority;

        internal ProviderMethod(Compilation compilation, INamedTypeSymbol owner, ITypeSymbol parameterType, int? priority)
            : base("Pick", compilation.GetSpecialType(SpecialType.System_Int32), [], owner, owner,
                compilation.SourceGlobalNamespace, [], [])
        {
            _priority = priority;
            SetParameters([new SourceParameterSymbol("value", parameterType, this, owner,
                compilation.SourceGlobalNamespace, [], [])]);
        }

        public bool TryGetOverloadResolutionPriority(out int priority)
        {
            priority = _priority.GetValueOrDefault();
            return _priority.HasValue;
        }
    }
}
