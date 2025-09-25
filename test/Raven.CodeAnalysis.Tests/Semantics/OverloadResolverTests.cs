using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.IO;
using System.Linq;
using System.Runtime.InteropServices;

using Raven.CodeAnalysis;
using Raven.CodeAnalysis.Symbols;
using Raven.CodeAnalysis.Syntax;
using Raven.CodeAnalysis.Tests;

using Xunit;

namespace Raven.CodeAnalysis.Semantics.Tests;

public sealed class OverloadResolverTests : CompilationTestBase
{
    [Fact]
    public void ResolveOverload_PrefersIdentityOverNumeric()
    {
        var compilation = CreateInitializedCompilation();
        var intType = compilation.GetSpecialType(SpecialType.System_Int32);
        var longType = compilation.GetSpecialType(SpecialType.System_Int64);

        var identity = CreateMethod(compilation, "Identity", intType);
        var numeric = CreateMethod(compilation, "Numeric", longType);

        var arguments = CreateArguments(new TestBoundExpression(intType));

        var result = OverloadResolver.ResolveOverload([identity, numeric], arguments, compilation);

        Assert.True(result.Success);
        Assert.Same(identity, result.Method);
    }

    [Fact]
    public void ResolveOverload_PrefersNumericOverBoxing()
    {
        var compilation = CreateInitializedCompilation();
        var intType = compilation.GetSpecialType(SpecialType.System_Int32);
        var longType = compilation.GetSpecialType(SpecialType.System_Int64);
        var objectType = compilation.GetSpecialType(SpecialType.System_Object);

        var numeric = CreateMethod(compilation, "Numeric", longType);
        var boxing = CreateMethod(compilation, "Boxing", objectType);

        var arguments = CreateArguments(new TestBoundExpression(intType));

        var result = OverloadResolver.ResolveOverload([numeric, boxing], arguments, compilation);

        Assert.True(result.Success);
        Assert.Same(numeric, result.Method);
    }

    [Fact]
    public void ResolveOverload_UsesCommonDenominatorForUnionArguments()
    {
        var compilation = CreateInitializedCompilation();
        var objectType = compilation.GetSpecialType(SpecialType.System_Object);
        var streamType = compilation.GetTypeByMetadataName("System.IO.Stream") ?? throw new InvalidOperationException("Missing System.IO.Stream");
        var memoryStream = compilation.GetTypeByMetadataName("System.IO.MemoryStream") ?? throw new InvalidOperationException("Missing System.IO.MemoryStream");
        var fileStream = compilation.GetTypeByMetadataName("System.IO.FileStream") ?? throw new InvalidOperationException("Missing System.IO.FileStream");

        var union = new UnionTypeSymbol(new[] { memoryStream, fileStream }, compilation.Assembly, null, null, Array.Empty<Location>());
        var stream = CreateMethod(compilation, "Stream", streamType);
        var obj = CreateMethod(compilation, "Object", objectType);

        var arguments = CreateArguments(new TestBoundExpression(union));

        var result = OverloadResolver.ResolveOverload([stream, obj], arguments, compilation);

        Assert.True(result.Success);
        Assert.Same(stream, result.Method);
    }

    [Fact]
    public void ResolveOverload_LiteralArgumentPrefersUnderlyingPrimitive()
    {
        var compilation = CreateInitializedCompilation();
        var intType = compilation.GetSpecialType(SpecialType.System_Int32);
        var doubleType = compilation.GetSpecialType(SpecialType.System_Double);
        var literalType = new LiteralTypeSymbol(intType, 1, compilation);

        var literalArgument = new BoundLiteralExpression(BoundLiteralExpressionKind.NumericLiteral, 1, literalType);
        var identity = CreateMethod(compilation, "Identity", intType);
        var numeric = CreateMethod(compilation, "Numeric", doubleType);

        var arguments = CreateArguments(literalArgument);

        var result = OverloadResolver.ResolveOverload([identity, numeric], arguments, compilation);

        Assert.True(result.Success);
        Assert.Same(identity, result.Method);
    }

    [Fact]
    public void ResolveOverload_NullableArgumentPrefersNullableParameter()
    {
        var compilation = CreateInitializedCompilation();
        var stringType = compilation.GetSpecialType(SpecialType.System_String);

        var nullableArgumentType = new NullableTypeSymbol(
            stringType,
            compilation.Assembly,
            null,
            compilation.Assembly.GlobalNamespace,
            Array.Empty<Location>());

        var nullableParameterType = new NullableTypeSymbol(
            stringType,
            compilation.Assembly,
            null,
            compilation.Assembly.GlobalNamespace,
            Array.Empty<Location>());

        var nonNullable = CreateMethod(compilation, "NonNullable", stringType);
        var nullable = CreateMethod(compilation, "Nullable", nullableParameterType);

        var arguments = CreateArguments(new TestBoundExpression(nullableArgumentType));

        var result = OverloadResolver.ResolveOverload([nonNullable, nullable], arguments, compilation);

        Assert.True(result.Success);
        Assert.Same(nullable, result.Method);
    }

    [Fact]
    public void ResolveOverload_ReturnsNullWhenAmbiguous()
    {
        var compilation = CreateInitializedCompilation();
        var intType = compilation.GetSpecialType(SpecialType.System_Int32);
        var longType = compilation.GetSpecialType(SpecialType.System_Int64);
        var doubleType = compilation.GetSpecialType(SpecialType.System_Double);

        var toLong = CreateMethod(compilation, "ToLong", longType);
        var toDouble = CreateMethod(compilation, "ToDouble", doubleType);

        var arguments = CreateArguments(new TestBoundExpression(intType));

        var result = OverloadResolver.ResolveOverload([toLong, toDouble], arguments, compilation);

        Assert.True(result.IsAmbiguous);
        Assert.False(result.Success);
        Assert.Contains(toLong, result.AmbiguousCandidates, SymbolEqualityComparer.Default);
        Assert.Contains(toDouble, result.AmbiguousCandidates, SymbolEqualityComparer.Default);
    }

    protected override MetadataReference[] GetMetadataReferences()
    {
        var runtimeDirectory = RuntimeEnvironment.GetRuntimeDirectory();
        var references = new List<MetadataReference>();

        var coreLibPath = Path.Combine(runtimeDirectory, "System.Private.CoreLib.dll");
        if (File.Exists(coreLibPath))
            references.Add(MetadataReference.CreateFromFile(coreLibPath));

        references.AddRange(Directory
            .EnumerateFiles(runtimeDirectory, "*.dll")
            .Where(path => !path.Equals(coreLibPath, StringComparison.OrdinalIgnoreCase))
            .Select(MetadataReference.CreateFromFile));

        references.AddRange(base.GetMetadataReferences());

        return references
            .OfType<PortableExecutableReference>()
            .Where(r => !string.IsNullOrEmpty(r.FilePath))
            .GroupBy(r => Path.GetFileNameWithoutExtension(r.FilePath), StringComparer.OrdinalIgnoreCase)
            .Select(group => (MetadataReference)group.First())
            .ToArray();
    }

    private Compilation CreateInitializedCompilation()
    {
        var compilation = CreateCompilation();
        compilation.EnsureSetup();
        return compilation;
    }

    private static FakeMethodSymbol CreateMethod(Compilation compilation, string name, ITypeSymbol parameterType)
    {
        var parameter = new FakeParameterSymbol("arg", parameterType, RefKind.None, isParams: false);
        var parameters = ImmutableArray.Create<IParameterSymbol>(parameter);
        return new FakeMethodSymbol(name, compilation.GetSpecialType(SpecialType.System_Unit), parameters);
    }

    private static BoundExpression[] CreateArguments(params BoundExpression[] expressions)
        => expressions;

    private sealed class TestBoundExpression : BoundExpression
    {
        public TestBoundExpression(ITypeSymbol type)
            : base(type)
        {
        }

        public override void Accept(BoundTreeVisitor visitor)
            => visitor.DefaultVisit(this);

        public override TResult Accept<TResult>(BoundTreeVisitor<TResult> visitor)
            => visitor.DefaultVisit(this);
    }

    private abstract class FakeSymbol : ISymbol
    {
        protected FakeSymbol(SymbolKind kind, string name)
        {
            Kind = kind;
            Name = name;
            MetadataName = name;
        }

        public SymbolKind Kind { get; }

        public string Name { get; }

        public string MetadataName { get; }

        public ISymbol? ContainingSymbol { get; private set; }

        public IAssemblySymbol? ContainingAssembly => null;

        public IModuleSymbol? ContainingModule => null;

        public INamedTypeSymbol? ContainingType { get; private set; }

        public INamespaceSymbol? ContainingNamespace { get; private set; }

        public ImmutableArray<Location> Locations { get; } = ImmutableArray<Location>.Empty;

        public Accessibility DeclaredAccessibility => Accessibility.Public;

        public ImmutableArray<SyntaxReference> DeclaringSyntaxReferences { get; } = ImmutableArray<SyntaxReference>.Empty;

        public bool IsImplicitlyDeclared => true;

        public bool IsStatic => false;

        public ISymbol UnderlyingSymbol => this;

        public bool IsAlias => false;

        public bool Equals(ISymbol? other, SymbolEqualityComparer comparer)
            => ReferenceEquals(this, other);

        public bool Equals(ISymbol? other)
            => ReferenceEquals(this, other);

        public override bool Equals(object? obj)
            => ReferenceEquals(this, obj);

        public override int GetHashCode()
            => HashCode.Combine(Kind, Name);

        public void Accept(SymbolVisitor visitor)
            => visitor.DefaultVisit(this);

        public TResult Accept<TResult>(SymbolVisitor<TResult> visitor)
            => visitor.DefaultVisit(this);

        public void SetContainer(ISymbol? container, INamedTypeSymbol? containingType = null, INamespaceSymbol? containingNamespace = null)
        {
            ContainingSymbol = container;
            ContainingType = containingType;
            ContainingNamespace = containingNamespace;
        }
    }

    private sealed class FakeMethodSymbol : FakeSymbol, IMethodSymbol
    {
        public FakeMethodSymbol(string name, ITypeSymbol returnType, ImmutableArray<IParameterSymbol> parameters)
            : base(SymbolKind.Method, name)
        {
            ReturnType = returnType;
            Parameters = parameters;

            foreach (var parameter in parameters)
            {
                if (parameter is FakeSymbol fakeParameter)
                {
                    fakeParameter.SetContainer(this);
                }
            }
        }

        public MethodKind MethodKind => MethodKind.Ordinary;

        public ITypeSymbol ReturnType { get; }

        public ImmutableArray<IParameterSymbol> Parameters { get; }

        public IMethodSymbol? OriginalDefinition => this;

        public bool IsAbstract => false;

        public bool IsAsync => false;

        public bool IsCheckedBuiltin => false;

        public bool IsDefinition => true;

        public bool IsExtensionMethod => false;

        public bool IsExtern => false;

        public bool IsGenericMethod => false;

        public bool IsOverride => false;

        public bool IsReadOnly => false;

        public bool IsSealed => false;

        public bool IsVirtual => false;

        public ImmutableArray<IMethodSymbol> ExplicitInterfaceImplementations => ImmutableArray<IMethodSymbol>.Empty;

        public ImmutableArray<ITypeParameterSymbol> TypeParameters => ImmutableArray<ITypeParameterSymbol>.Empty;

        public ImmutableArray<ITypeSymbol> TypeArguments => ImmutableArray<ITypeSymbol>.Empty;

        public IMethodSymbol? ConstructedFrom => this;

        public IMethodSymbol Construct(params ITypeSymbol[] typeArguments)
        {
            if (typeArguments is null)
                throw new ArgumentNullException(nameof(typeArguments));

            if (typeArguments.Length != 0)
                throw new InvalidOperationException("FakeMethodSymbol does not support generic construction.");

            return this;
        }
    }

    private sealed class FakeParameterSymbol : FakeSymbol, IParameterSymbol
    {
        public FakeParameterSymbol(string name, ITypeSymbol type, RefKind refKind, bool isParams)
            : base(SymbolKind.Parameter, name)
        {
            Type = type;
            RefKind = refKind;
            IsParams = isParams;
        }

        public ITypeSymbol Type { get; }

        public bool IsParams { get; }

        public RefKind RefKind { get; }

        public void SetContainer(ISymbol? container)
            => base.SetContainer(container);
    }
}
