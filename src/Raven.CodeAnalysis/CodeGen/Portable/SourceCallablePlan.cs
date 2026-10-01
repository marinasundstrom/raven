using Raven.CodeAnalysis.Syntax;
using Raven.CodeAnalysis.Symbols;

namespace Raven.CodeAnalysis.CodeGen.Portable;

// A source declaration, not a backend definition. In particular, an assembly function
// has no logical type owner even when the CLI symbol model supplies a carrier type.
internal sealed record SourceCallablePlan(
    IMethodSymbol Symbol, SyntaxNode Syntax, SyntaxNode? Body,
    INamedTypeSymbol? TypeOwner, string MetadataName, CallableSignature Signature)
{
    internal string Namespace { get; } = GetNamespace(Symbol.ContainingNamespace);
    private static string GetNamespace(INamespaceSymbol? scope)
    {
        if (scope is null || scope.IsGlobalNamespace) return "";
        var names = new Stack<string>();
        for (; scope is { IsGlobalNamespace: false }; scope = scope.ContainingNamespace)
            names.Push(scope.Name);
        return string.Join(".", names);
    }
    internal EmissionDeclarationKind DeclarationKind => IsAssemblyFunction
        ? Namespace.Length == 0 ? EmissionDeclarationKind.AssemblyFunction : EmissionDeclarationKind.NamespacedAssemblyFunction
        : Symbol.MethodKind == MethodKind.Constructor ? EmissionDeclarationKind.Constructor
        : Symbol.MethodKind is MethodKind.PropertyGet or MethodKind.PropertySet
            ? Symbol.ContainingSymbol is IPropertySymbol { IsIndexer: true } ? EmissionDeclarationKind.IndexerAccessor : EmissionDeclarationKind.PropertyAccessor
        : Symbol.IsStatic ? EmissionDeclarationKind.StaticMethod : EmissionDeclarationKind.InstanceMethod;
    internal Accessibility Visibility => Symbol.DeclaredAccessibility;
    internal bool IsSupportedBy(EmissionCapabilities capabilities) => capabilities.Allows(DeclarationKind) && capabilities.Allows(Signature) &&
        (IsAssemblyFunction ? capabilities.AllowsFunctionVisibility(Visibility) : capabilities.AllowsMethodVisibility(Visibility));

    internal bool IsAssemblyFunction => TypeOwner is null;

    internal static bool TryCreate(IMethodSymbol symbol, out SourceCallablePlan? plan, EmissionCapabilities? capabilities = null)
    {
        plan = null;
        if (symbol.IsExtern ||
            !CallableSignature.TryCreate(symbol, out var signature, capabilities)) return false;
        if (!symbol.IsStatic && (symbol.MethodKind is not (MethodKind.Ordinary or MethodKind.Constructor or MethodKind.PropertyGet or MethodKind.PropertySet) || symbol.IsVirtual || symbol.IsOverride || symbol.IsAbstract ||
            symbol.ContainingType is not { } receiver || !SourceTypePlan.TryCreate(receiver, out _))) return false;
        if (symbol.ContainingSymbol is SourcePropertySymbol { IsAutoProperty: true, IsStatic: false, BackingField: { } } property &&
            symbol.DeclaringSyntaxReferences.IsEmpty && property.DeclaringSyntaxReferences.Length == 1)
        {
            var propertySyntax = property.DeclaringSyntaxReferences[0].GetSyntax();
            plan = new(symbol, propertySyntax, propertySyntax, symbol.ContainingType, symbol.MetadataName, signature);
            if (capabilities is null || plan.IsSupportedBy(capabilities)) return true;
            plan = null; return false;
        }
        if (symbol is { MethodKind: MethodKind.Constructor, IsStatic: false, Parameters.Length: 0 } &&
            symbol.DeclaringSyntaxReferences.FirstOrDefault()?.GetSyntax() is ClassDeclarationSyntax ownerSyntax)
        {
            plan = new(symbol, ownerSyntax, ownerSyntax, symbol.ContainingType, symbol.MetadataName, signature);
            if (capabilities is null || plan.IsSupportedBy(capabilities)) return true;
            plan = null; return false;
        }
        if (symbol.DeclaringSyntaxReferences.Length != 1) return false;
        var syntax = symbol.DeclaringSyntaxReferences[0].GetSyntax();
        switch (syntax)
        {
            case IndexerDeclarationSyntax indexerDeclaration when symbol.MethodKind == MethodKind.PropertyGet && indexerDeclaration.ExpressionBody is { } indexerBody:
                plan = new(symbol, syntax, indexerBody, symbol.ContainingType, symbol.MetadataName, signature);
                if (capabilities is null || plan.IsSupportedBy(capabilities)) return true;
                plan = null; return false;
            case PropertyDeclarationSyntax propertyDeclaration when symbol.MethodKind == MethodKind.PropertyGet && propertyDeclaration.ExpressionBody is { } expressionBody:
                plan = new(symbol, syntax, expressionBody, symbol.ContainingType, symbol.MetadataName, signature);
                if (capabilities is null || plan.IsSupportedBy(capabilities)) return true;
                plan = null; return false;
            case AccessorDeclarationSyntax accessor when symbol.MethodKind is MethodKind.PropertyGet or MethodKind.PropertySet:
                plan = new(symbol, syntax, (SyntaxNode?)accessor.Body ?? accessor.ExpressionBody, symbol.ContainingType, symbol.MetadataName, signature);
                if (plan.Body is not null && (capabilities is null || plan.IsSupportedBy(capabilities))) return true;
                plan = null; return false;
            case ConstructorDeclarationSyntax constructor when HasRootInitialization(symbol, constructor) && symbol.ContainingType is { } constructorOwner:
                plan = new(symbol, syntax, (SyntaxNode?)constructor.Body ?? constructor.ExpressionBody, constructorOwner, symbol.MetadataName, signature);
                if (capabilities is null || plan.IsSupportedBy(capabilities)) return true;
                plan = null; return false;
            case MethodDeclarationSyntax method when symbol.ContainingType is { } owner:
                plan = new(symbol, syntax, (SyntaxNode?)method.Body ?? method.ExpressionBody, owner, symbol.MetadataName, signature);
                if (capabilities is null || plan.IsSupportedBy(capabilities)) return true;
                plan = null;
                return false;
            case FunctionStatementSyntax function when symbol.IsStatic && function.Parent is GlobalStatementSyntax { Parent: CompilationUnitSyntax or BaseNamespaceDeclarationSyntax }:
                plan = new(symbol, syntax, (SyntaxNode?)function.Body ?? function.ExpressionBody, null, symbol.Name, signature);
                if (capabilities is null || plan.IsSupportedBy(capabilities)) return true;
                plan = null;
                return false;
            default:
                return false;
        }
    }

    // Root construction is backend policy: CLI calls Object::.ctor; native roots have no base.
    // An explicit base() can use that policy only after binding proves the same contract.
    private static bool HasRootInitialization(IMethodSymbol symbol, ConstructorDeclarationSyntax syntax)
        => syntax.Initializer is null ||
           syntax.Initializer.Keyword.IsKind(SyntaxKind.BaseKeyword) &&
           syntax.Initializer.ArgumentList.Arguments.Count == 0 &&
           symbol is SourceMethodSymbol { ConstructorInitializer: { } initializer } &&
           initializer.Constructor is { MethodKind: MethodKind.Constructor, IsStatic: false, Parameters.Length: 0 } target &&
           target.ContainingType?.SpecialType == SpecialType.System_Object && !initializer.Arguments.Any();

    // The CLI adapter can supply a carrier/lifted name; native emission uses the source
    // metadata name. Concrete owner selection and physical visibility encoding remain backend policies.
    internal TMethod Define<TMethod>(ICallableDefinitionBuilder<TMethod> builder, string? emittedName = null)
        => builder.DefineMethod(emittedName ?? MetadataName, this);

    internal bool TryLowerBody(Compilation compilation, Func<BoundInvocationExpression, bool> permitsConsoleWrite,
        out LinearMethodBody? body, out LinearBodyFailure? failure, EmissionCapabilities capabilities)
    {
        if (!IsSupportedBy(capabilities))
        {
            body = null;
            failure = new("target does not support declaration " + DeclarationKind + " or its signature", Syntax);
            return false;
        }
        if (Body is null)
        {
            body = null;
            failure = new("source callable body unavailable", Syntax);
            return false;
        }
        return LinearMethodBody.TryLower(Symbol, compilation.GetSemanticModel(Body.SyntaxTree), Body,
            permitsConsoleWrite, out body, out failure, capabilities);
    }
}
