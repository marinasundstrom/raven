using System;
using System.Collections.Generic;
using System.Linq;

using Raven.CodeAnalysis.Operations;
using Raven.CodeAnalysis.Symbols;
using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis.Diagnostics;

internal static class MemberInitializationAnalysis
{
    public static bool IsAssignedInInstanceInitialization(
        SemanticModel semanticModel,
        TypeDeclarationSyntax typeDeclaration,
        ISymbol member)
    {
        var declarations = member.ContainingType?.DeclaringSyntaxReferences
            .Select(reference => reference.GetSyntax()).OfType<TypeDeclarationSyntax>().ToArray()
            ?? [typeDeclaration];
        var analysis = new AssignmentAnalysis(member);
        var lifecycle = new Flow(Paths.Unassigned);
        foreach (var declaration in declarations)
        {
            var model = semanticModel.Compilation.GetSemanticModel(declaration.SyntaxTree);
            foreach (var initializer in declaration.Members.OfType<ParameterlessConstructorDeclarationSyntax>()
                         .Where(initializer => !initializer.Modifiers.Any(token => token.Kind == SyntaxKind.StaticKeyword)))
            {
                var body = (SyntaxNode?)initializer.Body ?? initializer.ExpressionBody?.Expression;
                if (body is not null)
                    lifecycle = analysis.Then(lifecycle, model.GetOperation(body));
            }
        }

        var constructors = declarations.SelectMany(declaration => declaration.Members)
            .OfType<BaseConstructorDeclarationSyntax>()
            .Where(ctor => !ctor.Modifiers.Any(token => token.Kind == SyntaxKind.StaticKeyword)).ToArray();
        foreach (var ctor in constructors)
        {
            var model = semanticModel.Compilation.GetSemanticModel(ctor.SyntaxTree);
            var body = (SyntaxNode?)ctor.Body ?? ctor.ExpressionBody?.Expression;
            var state = body is null ? lifecycle : analysis.Then(lifecycle, model.GetOperation(body));
            if (!IsInitialized(state))
                return false;
        }

        var hasLifecycleConstructor = member.ContainingType?.Constructors.Any(ctor => !ctor.IsStatic &&
            ctor.DeclaringSyntaxReferences.Any(reference => reference.GetSyntax() is ParameterlessConstructorDeclarationSyntax)) == true;
        if (hasLifecycleConstructor && !IsInitialized(lifecycle))
            return false;

        // Primary initializer blocks execute only on the primary constructor.
        // With no explicit constructor there is also an implicit construction path.
        var primaryDeclarations = declarations.Where(declaration => declaration.ParameterList is not null).ToArray();
        if (constructors.Length == 0 || primaryDeclarations.Length != 0)
        {
            var primary = lifecycle;
            foreach (var declaration in primaryDeclarations)
            {
                var model = semanticModel.Compilation.GetSemanticModel(declaration.SyntaxTree);
                foreach (var initializer in declaration.Members.OfType<InitializerBlockDeclarationSyntax>())
                    primary = analysis.Then(primary, model.GetOperation(initializer.Body));
            }
            if (!IsInitialized(primary))
                return false;
        }

        return true;
    }

    private static bool IsInitialized(Flow flow)
        => ((flow.Normal | flow.Returned) & Paths.Unassigned) == 0;

    [Flags]
    private enum Paths { None = 0, Unassigned = 1, Assigned = 2 }

    private readonly record struct Flow(
        Paths Normal, Paths Returned = Paths.None, Paths Broken = Paths.None, Paths Continued = Paths.None)
    {
        public Flow Join(Flow other) => new(Normal | other.Normal, Returned | other.Returned,
            Broken | other.Broken, Continued | other.Continued);
    }

    // Track possible assigned/unassigned states only; no assignment in a deferred
    // body, an optional branch, or another instance proves this instance initialized.
    private sealed class AssignmentAnalysis(ISymbol member)
    {
        public Flow Then(Flow before, IOperation? operation)
            => Analyze(operation, before.Normal).Join(before with { Normal = Paths.None });

        private Flow Sequence(IEnumerable<IOperation> operations, Paths incoming)
        {
            var state = new Flow(incoming);
            foreach (var operation in operations)
                state = Then(state, operation);
            return state;
        }

        private Flow Analyze(IOperation? operation, Paths incoming)
        {
            if (operation is null || incoming == Paths.None)
                return new Flow(incoming);

            switch (operation)
            {
                case ILambdaOperation or IFunctionOperation or INameOfOperation:
                    return new Flow(incoming);
                case IAssignmentOperation assignment:
                    var assigned = Analyze(assignment.Value, incoming);
                    return IsAssignmentToMember(assignment) && assigned.Normal != Paths.None
                        ? assigned with { Normal = Paths.Assigned }
                        : assigned;
                case IReturnOperation returned:
                    var result = Analyze(returned.ReturnedValue, incoming);
                    return result with { Returned = result.Returned | result.Normal, Normal = Paths.None };
                case IThrowOperation:
                    return new Flow(Paths.None);
                case IConditionalOperation conditional:
                    var condition = Analyze(conditional.Condition, incoming);
                    var branches = conditional.Condition is ILiteralOperation { Value: bool constant }
                        ? Analyze(constant ? conditional.WhenTrue : conditional.WhenFalse, condition.Normal)
                        : Analyze(conditional.WhenTrue, condition.Normal).Join(Analyze(conditional.WhenFalse, condition.Normal));
                    return branches.Join(condition with { Normal = Paths.None });
                case IConditionalAccessOperation conditionalAccess:
                    var receiver = Analyze(conditionalAccess.Receiver, incoming);
                    return receiver.Join(Then(receiver, conditionalAccess.WhenNotNull));
                case ICoalesceOperation coalesce:
                    var left = Analyze(coalesce.Left, incoming);
                    return left.Join(Then(left, coalesce.Right));
                case IBinaryOperation binary when operation.Syntax is InfixOperatorExpressionSyntax syntax &&
                    syntax.OperatorToken.Kind is SyntaxKind.AmpersandAmpersandToken or SyntaxKind.BarBarToken:
                    var first = Analyze(binary.Left, incoming);
                    return first.Join(Then(first, binary.Right));
                case IWhileLoopOperation loop:
                    var test = Analyze(loop.Condition, incoming);
                    if (loop.Condition is ILiteralOperation { Value: false })
                        return test;
                    var body = Analyze(loop.Body, test.Normal);
                    var zeroIterations = loop.Condition is ILiteralOperation { Value: true } ? Paths.None : test.Normal;
                    return new Flow(zeroIterations | body.Broken, body.Returned).Join(test with { Normal = Paths.None });
                case IForLoopOperation loop:
                    var collection = Analyze(loop.Collection, incoming);
                    var iteration = Analyze(loop.Body, collection.Normal);
                    return new Flow(collection.Normal | iteration.Broken, iteration.Returned)
                        .Join(collection with { Normal = Paths.None });
                case ILoopOperation loop:
                    var repeated = Analyze(loop.Body, incoming);
                    return new Flow(repeated.Broken, repeated.Returned);
                case ITryOperation guarded:
                    var alternatives = Analyze(guarded.Body, incoming);
                    foreach (var handler in guarded.Catches)
                        alternatives = alternatives.Join(Analyze(handler.Body, incoming));
                    return guarded.Finally is null ? alternatives : ApplyFinally(alternatives, guarded.Finally);
                case ITryExpressionOperation capture:
                    return new Flow(incoming).Join(Analyze(capture.Operation, incoming));
                case ISwitchOperation match:
                    var input = Analyze(match.Value, incoming);
                    var arms = new Flow(Paths.None);
                    foreach (var arm in match.ArmValues)
                        arms = arms.Join(Analyze(arm, input.Normal));
                    var exhaustive = match.Syntax switch
                    {
                        MatchExpressionSyntax expression => match.SemanticModel.GetMatchExhaustiveness(expression).IsExhaustive,
                        PostfixMatchExpressionSyntax expression => match.SemanticModel.GetMatchExhaustiveness(expression).IsExhaustive,
                        MatchStatementSyntax statement => match.SemanticModel.GetMatchExhaustiveness(statement).IsExhaustive,
                        _ => false
                    };
                    return arms.Join(input with { Normal = exhaustive ? Paths.None : input.Normal });
            }

            return operation.Kind switch
            {
                OperationKind.Break or OperationKind.BreakExpression => new Flow(Paths.None, Broken: incoming),
                OperationKind.Continue or OperationKind.ContinueExpression => new Flow(Paths.None, Continued: incoming),
                // Unknown jump destinations cannot establish definite initialization.
                OperationKind.Goto or OperationKind.ConditionalGoto => new Flow(incoming, Returned: incoming),
                _ => Sequence(operation.ChildOperations, incoming)
            };
        }

        private Flow ApplyFinally(Flow before, IOperation finallyOperation)
        {
            var normal = Analyze(finallyOperation, before.Normal);
            var returned = Analyze(finallyOperation, before.Returned);
            var broken = Analyze(finallyOperation, before.Broken);
            var continued = Analyze(finallyOperation, before.Continued);
            return normal
                .Join(returned with { Returned = returned.Returned | returned.Normal, Normal = Paths.None })
                .Join(broken with { Broken = broken.Broken | broken.Normal, Normal = Paths.None })
                .Join(continued with { Continued = continued.Continued | continued.Normal, Normal = Paths.None });
        }

        private bool IsAssignmentToMember(IAssignmentOperation assignment)
        {
            var left = assignment.Syntax switch
            {
                AssignmentStatementSyntax statement => statement.Left,
                AssignmentExpressionSyntax expression => expression.Left,
                _ => null
            };
            if (left is not IdentifierNameSyntax &&
                left is not MemberAccessExpressionSyntax { Expression: SelfExpressionSyntax })
            {
                return false;
            }

            var target = assignment.SemanticModel.GetSymbolInfo(left).Symbol?.UnderlyingSymbol;
            if (target is IFieldSymbol { AssociatedSymbol: IPropertySymbol property })
                target = property.UnderlyingSymbol;
            return SymbolEqualityComparer.Default.Equals(target, member.UnderlyingSymbol);
        }
    }
}
