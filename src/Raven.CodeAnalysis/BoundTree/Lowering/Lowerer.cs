using System;
using System.Collections.Generic;
using System.Linq;

using Raven.CodeAnalysis.Symbols;

namespace Raven.CodeAnalysis;

internal sealed partial class Lowerer : BoundTreeRewriter
{
    private readonly ISymbol _containingSymbol;
    private readonly Stack<(ILabelSymbol BreakLabel, ILabelSymbol ContinueLabel)> _loopStack = new();
    private readonly ILoweringTraceSink? _loweringTrace;
    private int _labelCounter;
    private int _tempCounter;
    private IteratorState? _iteratorState;

    private Lowerer(ISymbol containingSymbol, ILoweringTraceSink? loweringTrace)
    {
        _containingSymbol = containingSymbol;
        _loweringTrace = loweringTrace;
    }

    public static BoundBlockStatement LowerBlock(ISymbol containingSymbol, BoundBlockStatement block)
    {
        var lowerer = CreateLowerer(containingSymbol);
        return (BoundBlockStatement)lowerer.VisitStatement(block);
    }

    public static BoundStatement LowerStatement(ISymbol containingSymbol, BoundStatement statement)
    {
        var lowerer = CreateLowerer(containingSymbol);
        return (BoundStatement)lowerer.VisitStatement(statement);
    }

    public static BoundExpression LowerExpression(ISymbol containingSymbol, BoundExpression expression)
    {
        var lowerer = CreateLowerer(containingSymbol);
        return (BoundExpression)lowerer.VisitExpression(expression)!;
    }

    private static Lowerer CreateLowerer(ISymbol containingSymbol)
    {
        var loweringTrace = TryGetLoweringTrace(containingSymbol);
        return new Lowerer(containingSymbol, loweringTrace);
    }

    private static ILoweringTraceSink? TryGetLoweringTrace(ISymbol containingSymbol)
    {
        if (containingSymbol.ContainingAssembly is SourceAssemblySymbol sourceAssembly)
            return sourceAssembly.Compilation.LoweringTrace;

        return null;
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

    private readonly struct IteratorState
    {
        public IteratorState(SourceLocalSymbol builderLocal, IMethodSymbol? addMethod, ITypeSymbol elementType, ITypeSymbol returnType)
        {
            BuilderLocal = builderLocal;
            AddMethod = addMethod;
            ElementType = elementType;
            ReturnType = returnType;
        }

        public SourceLocalSymbol BuilderLocal { get; }
        public IMethodSymbol? AddMethod { get; }
        public ITypeSymbol ElementType { get; }
        public ITypeSymbol ReturnType { get; }
    }

    private bool TryGetIteratorElementType(ITypeSymbol returnType, out ITypeSymbol elementType)
    {
        elementType = GetCompilation().ErrorTypeSymbol;

        if (returnType is INamedTypeSymbol named &&
            named.OriginalDefinition.SpecialType == SpecialType.System_Collections_Generic_IEnumerable_T &&
            named.TypeArguments.Length == 1)
        {
            elementType = named.TypeArguments[0];
            return true;
        }

        if (returnType.SpecialType == SpecialType.System_Collections_IEnumerable)
        {
            elementType = GetCompilation().GetSpecialType(SpecialType.System_Object);
            return true;
        }

        return false;
    }

    private static bool ContainsYield(BoundBlockStatement node)
    {
        var finder = new YieldFinder();
        finder.VisitStatement(node);
        return finder.Found;
    }

    private sealed class YieldFinder : BoundTreeWalker
    {
        public bool Found { get; private set; }

        public override void VisitYieldReturnStatement(BoundYieldReturnStatement node)
        {
            Found = true;
        }

        public override void VisitYieldBreakStatement(BoundYieldBreakStatement node)
        {
            Found = true;
        }

        public override void Visit(BoundNode boundNode)
        {
            if (Found)
                return;

            base.Visit(boundNode);
        }
    }
}
