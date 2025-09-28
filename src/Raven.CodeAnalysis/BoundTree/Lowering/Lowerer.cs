using System;
using System.Collections.Generic;

using Raven.CodeAnalysis.Symbols;

namespace Raven.CodeAnalysis;

internal sealed partial class Lowerer : BoundTreeRewriter
{
    private readonly ISymbol _containingSymbol;
    private readonly Stack<(ILabelSymbol BreakLabel, ILabelSymbol ContinueLabel)> _loopStack = new();
    private int _labelCounter;
    private int _tempCounter;

    private Lowerer(ISymbol containingSymbol)
    {
        _containingSymbol = containingSymbol;
    }

    public static BoundBlockStatement LowerBlock(ISymbol containingSymbol, BoundBlockStatement block)
    {
        var lowerer = new Lowerer(containingSymbol);
        return (BoundBlockStatement)lowerer.VisitStatement(block);
    }

    public static BoundStatement LowerStatement(ISymbol containingSymbol, BoundStatement statement)
    {
        var lowerer = new Lowerer(containingSymbol);
        return (BoundStatement)lowerer.VisitStatement(statement);
    }

    private ILabelSymbol CreateLabel(string prefix)
    {
        var name = $"{prefix}_{_labelCounter++}";
        var containingType = _containingSymbol.ContainingType as INamedTypeSymbol;
        var containingNamespace = _containingSymbol.ContainingNamespace;
        return new LabelSymbol(name, _containingSymbol, containingType, containingNamespace,
            [Location.None], Array.Empty<SyntaxReference>());
    }

    private SourceLocalSymbol CreateTempLocal(string nameHint, ITypeSymbol type, bool isMutable)
    {
        var containingType = _containingSymbol.ContainingType as INamedTypeSymbol;
        var containingNamespace = _containingSymbol.ContainingNamespace;
        var name = $"<{nameHint}>__{_tempCounter++}";
        return new SourceLocalSymbol(name, type, isMutable, _containingSymbol, containingType, containingNamespace,
            [Location.None], Array.Empty<SyntaxReference>());
    }

    private static BoundStatement CreateLabelStatement(ILabelSymbol label)
    {
        return new BoundLabeledStatement(label, new BoundBlockStatement(Array.Empty<BoundStatement>()));
    }

    private Compilation GetCompilation()
    {
        if (_containingSymbol.ContainingAssembly is SourceAssemblySymbol sourceAssembly)
            return sourceAssembly.Compilation;

        throw new InvalidOperationException("Lowering requires a source assembly containing symbol.");
    }

    private static BoundExpression ApplyConversionIfNeeded(BoundExpression expression, ITypeSymbol targetType, Compilation compilation)
    {
        if (targetType is null)
            return expression;

        var sourceType = expression.Type ?? compilation.ErrorTypeSymbol;

        if (SymbolEqualityComparer.Default.Equals(sourceType, targetType))
            return expression;

        var conversion = compilation.ClassifyConversion(sourceType, targetType);
        if (!conversion.Exists || conversion.IsIdentity)
            return expression;

        return new BoundCastExpression(expression, targetType, conversion);
    }
}
