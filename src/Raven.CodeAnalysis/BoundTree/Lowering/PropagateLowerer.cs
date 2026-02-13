using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;

using Raven.CodeAnalysis.Symbols;

namespace Raven.CodeAnalysis;

internal static class PropagateLowerer
{
    public static BoundBlockStatement Rewrite(ISymbol containingSymbol, BoundBlockStatement body)
    {
        var lowerer = new Rewriter(containingSymbol);
        return lowerer.Rewrite(body);
    }

    private sealed class Rewriter : BoundTreeRewriter
    {
        private readonly ISymbol _containingSymbol;
        private readonly Compilation _compilation;
        private int _tempCounter;

        public Rewriter(ISymbol containingSymbol)
        {
            _containingSymbol = containingSymbol;
            _compilation = GetCompilation(containingSymbol);
        }

        public BoundBlockStatement Rewrite(BoundBlockStatement body)
        {
            return (BoundBlockStatement)VisitBlockStatement(body)!;
        }

        public override BoundNode? VisitBlockStatement(BoundBlockStatement node)
        {
            var changed = false;
            var statements = new List<BoundStatement>();

            foreach (var statement in node.Statements)
            {
                if (statement is BoundLocalDeclarationStatement localDeclaration
                    && TryRewriteLocalDeclaration(localDeclaration, out var rewrittenLocalStatements))
                {
                    changed = true;
                    statements.AddRange(rewrittenLocalStatements);
                    continue;
                }

                if (statement is BoundExpressionStatement expressionStatement
                    && TryRewriteExpressionStatement(expressionStatement, out var rewrittenExpressionStatements))
                {
                    changed = true;
                    statements.AddRange(rewrittenExpressionStatements);
                    continue;
                }

                var visited = (BoundStatement)VisitStatement(statement);
                if (!ReferenceEquals(visited, statement))
                    changed = true;

                statements.Add(visited);
            }

            if (!changed)
                return node;

            return new BoundBlockStatement(statements, node.LocalsToDispose);
        }

        private bool TryRewriteLocalDeclaration(BoundLocalDeclarationStatement node, out List<BoundStatement> statements)
        {
            statements = new List<BoundStatement>();
            var changed = false;
            var declarators = ImmutableArray.CreateBuilder<BoundVariableDeclarator>();

            foreach (var declarator in node.Declarators)
            {
                if (declarator.Initializer is BoundPropagateExpression propagate)
                {
                    var lowering = LowerPropagate(propagate);
                    if (lowering is null)
                    {
                        var visitedInitializer = VisitExpression(propagate) ?? propagate;
                        declarators.Add(ReferenceEquals(visitedInitializer, propagate)
                            ? declarator
                            : new BoundVariableDeclarator(declarator.Local, visitedInitializer));
                        changed |= !ReferenceEquals(visitedInitializer, propagate);
                        continue;
                    }

                    if (declarators.Count > 0)
                    {
                        statements.Add(new BoundLocalDeclarationStatement(declarators.ToImmutable(), node.IsUsing));
                        declarators.Clear();
                    }

                    statements.AddRange(lowering.Statements);
                    statements.Add(new BoundLocalDeclarationStatement(
                        ImmutableArray.Create(new BoundVariableDeclarator(declarator.Local, lowering.SuccessExpression)),
                        node.IsUsing));
                    changed = true;
                    continue;
                }

                var initializer = VisitExpression(declarator.Initializer) ?? declarator.Initializer;
                if (!ReferenceEquals(initializer, declarator.Initializer))
                    changed = true;

                declarators.Add(ReferenceEquals(initializer, declarator.Initializer)
                    ? declarator
                    : new BoundVariableDeclarator(declarator.Local, initializer));
            }

            if (!changed)
                return false;

            if (declarators.Count > 0)
                statements.Add(new BoundLocalDeclarationStatement(declarators.ToImmutable(), node.IsUsing));

            return true;
        }

        private bool TryRewriteExpressionStatement(BoundExpressionStatement node, out List<BoundStatement> statements)
        {
            statements = new List<BoundStatement>();

            if (node.Expression is not BoundPropagateExpression propagate)
            {
                var expression = VisitExpression(node.Expression) ?? node.Expression;
                if (ReferenceEquals(expression, node.Expression))
                    return false;

                statements.Add(new BoundExpressionStatement(expression));
                return true;
            }

            var lowering = LowerPropagate(propagate);
            if (lowering is null)
                return false;

            statements.AddRange(lowering.Statements);
            statements.Add(new BoundExpressionStatement(lowering.SuccessExpression));
            return true;
        }

        public override BoundExpression? VisitExpression(BoundExpression? node)
        {
            if (node is null)
                return null;

            if (node is BoundPropagateExpression propagate)
                return RewritePropagateExpression(propagate) ?? propagate;

            return base.VisitExpression(node);
        }

        private BoundExpression? RewritePropagateExpression(BoundPropagateExpression propagate)
        {
            var lowering = LowerPropagate(propagate);
            if (lowering is null)
                return null;

            lowering.Statements.Add(new BoundExpressionStatement(lowering.SuccessExpression));
            return new BoundBlockExpression(lowering.Statements, _compilation.UnitTypeSymbol);
        }

        private PropagateLowering? LowerPropagate(BoundPropagateExpression propagate)
        {
            var operandType = propagate.Operand.Type;
            var operandNamedType = operandType?.GetPlainType() as INamedTypeSymbol;
            if (operandType is null || operandType.TypeKind == TypeKind.Error || operandNamedType is null)
                return null;

            var tryGetMethod = FindTryGetMethod(operandNamedType, propagate);
            if (tryGetMethod is null)
                return null;

            var okLocalType = tryGetMethod.Parameters[0].GetByRefElementType();
            var okLocal = CreateTempLocal("propagateOk", okLocalType, isMutable: true);

            var operandLocal = CreateTempLocal("propagateOperand", operandType, isMutable: true);
            var operandInitializer = VisitExpression(propagate.Operand) ?? propagate.Operand;

            var statements = new List<BoundStatement>
            {
                new BoundLocalDeclarationStatement(new[]
                {
                    new BoundVariableDeclarator(operandLocal, operandInitializer)
                }),
                new BoundLocalDeclarationStatement(new[]
                {
                    new BoundVariableDeclarator(okLocal, null)
                })
            };

            var operandAccess = new BoundLocalAccess(operandLocal);
            var okAccess = new BoundLocalAccess(okLocal);
            var receiver = tryGetMethod.IsExtensionMethod ? null : operandAccess;
            var extensionReceiver = tryGetMethod.IsExtensionMethod ? operandAccess : null;

            var tryGetInvocation = new BoundInvocationExpression(
                tryGetMethod,
                new BoundExpression[] { new BoundAddressOfExpression(okAccess) },
                receiver,
                extensionReceiver,
                requiresReceiverAddress: operandType.IsValueType);

            var errorExpression = CreatePropagateErrorExpression(propagate, operandAccess);
            if (errorExpression is null)
                return null;

            var failureBlock = new BoundBlockStatement(new BoundStatement[]
            {
                new BoundReturnStatement(errorExpression)
            });

            statements.Add(new BoundIfStatement(
                tryGetInvocation,
                new BoundBlockStatement(Array.Empty<BoundStatement>()),
                failureBlock));

            var successExpression = CreatePropagateSuccessExpression(propagate, okAccess);
            if (successExpression.Type is null || successExpression.Type.TypeKind == TypeKind.Error)
                return null;

            return new PropagateLowering(statements, successExpression);
        }

        private BoundExpression? CreatePropagateErrorExpression(BoundPropagateExpression propagate, BoundExpression operandAccess)
        {
            var ctor = propagate.EnclosingErrorConstructor;
            var arguments = new List<BoundExpression>();

            if (ctor.Parameters.Length == 1)
            {
                var payload = GetPropagateErrorPayload(propagate, operandAccess);
                if (payload is null)
                    return null;

                var targetType = ctor.Parameters[0].Type;
                payload = ApplyErrorConversion(payload, targetType, propagate.ErrorConversion);
                arguments.Add(payload);
            }
            else if (ctor.Parameters.Length != 0)
            {
                return null;
            }

            BoundExpression errorCaseExpression = ctor.MethodKind == MethodKind.Constructor
                ? new BoundObjectCreationExpression(ctor, arguments)
                : new BoundInvocationExpression(ctor, arguments);

            return ApplyConversionIfNeeded(errorCaseExpression, propagate.EnclosingResultType);
        }

        private BoundExpression? GetPropagateErrorPayload(BoundPropagateExpression propagate, BoundExpression operandAccess)
        {
            if (propagate.UnwrapErrorMethod is { } unwrapMethod)
            {
                var receiver = unwrapMethod.IsExtensionMethod ? null : operandAccess;
                var extensionReceiver = unwrapMethod.IsExtensionMethod ? operandAccess : null;

                return new BoundInvocationExpression(
                    unwrapMethod,
                    Array.Empty<BoundExpression>(),
                    receiver,
                    extensionReceiver,
                    requiresReceiverAddress: receiver?.Type?.IsValueType == true);
            }

            var operandNamed = operandAccess.Type as INamedTypeSymbol;
            if (operandNamed is null)
                return null;

            var errorProperty = operandNamed.GetMembers("ErrorValue").OfType<IPropertySymbol>().FirstOrDefault()
                ?? operandNamed.GetMembers("Payload").OfType<IPropertySymbol>().FirstOrDefault();

            return errorProperty is null ? null : new BoundMemberAccessExpression(operandAccess, errorProperty);
        }

        private BoundExpression CreatePropagateSuccessExpression(BoundPropagateExpression propagate, BoundLocalAccess okAccess)
        {
            if (propagate.OkCaseType is null)
                return ApplyConversionIfNeeded(okAccess, propagate.OkType);

            var okCaseType = (INamedTypeSymbol)propagate.OkCaseType;
            var valueProperty = propagate.OkValueProperty
                ?? okCaseType.GetMembers("Value").OfType<IPropertySymbol>().FirstOrDefault();

            if (valueProperty is null)
                return new BoundErrorExpression(_compilation.ErrorTypeSymbol, null, BoundExpressionReason.UnsupportedOperation);

            var caseAccess = ApplyConversionIfNeeded(okAccess, okCaseType);
            if (!SymbolEqualityComparer.Default.Equals(caseAccess.Type?.GetPlainType(), okCaseType.GetPlainType()))
                return new BoundErrorExpression(_compilation.ErrorTypeSymbol, null, BoundExpressionReason.UnsupportedOperation);

            var valueAccess = new BoundMemberAccessExpression(caseAccess, valueProperty);
            return ApplyConversionIfNeeded(valueAccess, propagate.OkType);
        }

        private static IMethodSymbol? FindTryGetMethod(INamedTypeSymbol operandType, BoundPropagateExpression propagate)
        {
            var tryGetName = $"TryGet{propagate.OkCaseName}";
            var candidates = operandType.GetMembers(tryGetName).OfType<IMethodSymbol>()
                .Where(m => m.Parameters.Length == 1 && m.Parameters[0].RefKind == RefKind.Out)
                .ToArray();

            if (candidates.Length == 0)
                return null;

            var okCaseType = propagate.OkCaseType?.GetPlainType();
            if (okCaseType is not null)
            {
                var caseMatch = candidates.FirstOrDefault(m =>
                    SymbolEqualityComparer.Default.Equals(m.Parameters[0].GetByRefElementType().GetPlainType(), okCaseType));
                if (caseMatch is not null)
                    return caseMatch;
            }

            var okPayloadType = propagate.OkType.GetPlainType();
            var payloadMatch = candidates.FirstOrDefault(m =>
                SymbolEqualityComparer.Default.Equals(m.Parameters[0].GetByRefElementType().GetPlainType(), okPayloadType));

            return payloadMatch ?? candidates[0];
        }

        private BoundExpression ApplyErrorConversion(BoundExpression payload, ITypeSymbol targetType, Conversion conversion)
        {
            if (conversion.Exists && !conversion.IsIdentity)
                return new BoundConversionExpression(payload, targetType, conversion);

            return ApplyConversionIfNeeded(payload, targetType);
        }

        private BoundExpression ApplyConversionIfNeeded(BoundExpression expression, ITypeSymbol targetType)
        {
            var sourceType = expression.Type ?? _compilation.ErrorTypeSymbol;

            if (SymbolEqualityComparer.Default.Equals(sourceType, targetType))
                return expression;

            var conversion = _compilation.ClassifyConversion(sourceType, targetType);
            if (!conversion.Exists || conversion.IsIdentity)
                return expression;

            return new BoundConversionExpression(expression, targetType, conversion);
        }

        private SourceLocalSymbol CreateTempLocal(string nameHint, ITypeSymbol type, bool isMutable)
        {
            var containingType = _containingSymbol.ContainingType as INamedTypeSymbol;
            var containingNamespace = _containingSymbol.ContainingNamespace;
            var name = $"<{nameHint}>__{_tempCounter++}";

            return new SourceLocalSymbol(
                name,
                type,
                isMutable,
                _containingSymbol,
                containingType,
                containingNamespace,
                new[] { Location.None },
                Array.Empty<SyntaxReference>());
        }

        private static Compilation GetCompilation(ISymbol containingSymbol)
        {
            if (containingSymbol.ContainingAssembly is SourceAssemblySymbol sourceAssembly)
                return sourceAssembly.Compilation;

            throw new InvalidOperationException("Propagate lowering requires a source assembly containing symbol.");
        }
    }

    private sealed record PropagateLowering(List<BoundStatement> Statements, BoundExpression SuccessExpression);
}
