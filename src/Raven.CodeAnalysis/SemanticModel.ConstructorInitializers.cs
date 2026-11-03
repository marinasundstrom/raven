using System.Collections.Generic;
using System.Linq;

using Raven.CodeAnalysis.Symbols;
using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis;

public partial class SemanticModel
{
    private bool _constructorInitializersBound;

    internal void EnsureConstructorInitializersBound()
    {
        if (_constructorInitializersBound)
            return;

        _constructorInitializersBound = true;
        var types = new HashSet<NamedTypeSymbol>(SymbolEqualityComparer.Default);
        foreach (var declaration in SyntaxTree.GetRoot().DescendantNodes().OfType<TypeDeclarationSyntax>())
        {
            if (Compilation.TryGetDeclaredTypeSymbol(declaration, out var type) && types.Add(type))
                BindClassConstructorInitializers(type, GetBinderForIncrementalSemanticQuery(declaration));
        }
    }

    private void BindClassConstructorInitializers(NamedTypeSymbol type, Binder parent)
    {
        foreach (var constructor in type.GetMembers().OfType<SourceMethodSymbol>())
        {
            if (constructor.IsStatic || constructor.MethodKind != MethodKind.Constructor ||
                constructor.DeclaringSyntaxReferences.FirstOrDefault()?.SyntaxTree != SyntaxTree ||
                constructor.DeclaringSyntaxReferences.FirstOrDefault()?.GetSyntax() is not ConstructorDeclarationSyntax { Initializer: { } syntax }) continue;
            var binder = new ConstructorInitializerBinder(constructor, new MethodBinder(constructor, parent));
            constructor.SetConstructorInitializer(binder.Bind(syntax));
            foreach (var diagnostic in binder.Diagnostics.AsEnumerable())
                _declarationDiagnostics.Report(diagnostic);
        }
        foreach (var constructor in type.GetMembers().OfType<SourceMethodSymbol>())
        {
            if (constructor.DeclaringSyntaxReferences.FirstOrDefault()?.SyntaxTree != SyntaxTree ||
                constructor.DeclaringSyntaxReferences.FirstOrDefault()?.GetSyntax() is not ConstructorDeclarationSyntax { Initializer: SelfConstructorInitializerSyntax syntax })
                continue;
            var visited = new HashSet<MethodSymbol>(SymbolEqualityComparer.Default);
            SourceMethodSymbol? current = constructor;
            while (current is { DelegatesToSameTypeConstructor: true } && visited.Add(current))
            {
                var target = current.ConstructorInitializer!.Constructor.OriginalDefinition;
                if (SymbolEqualityComparer.Default.Equals(target, constructor))
                {
                    _declarationDiagnostics.ReportConstructorDelegationCycle(syntax.GetLocation());
                    break;
                }
                current = target as SourceMethodSymbol;
            }
        }
    }

}
