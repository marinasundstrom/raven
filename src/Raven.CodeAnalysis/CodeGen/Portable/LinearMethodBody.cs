using System.Collections.Immutable;

using Raven.CodeAnalysis.Symbols;
using Raven.CodeAnalysis.Syntax;

using OperatorKind = Raven.CodeAnalysis.BinaryOperatorKind;

namespace Raven.CodeAnalysis.CodeGen.Portable;

// Logical instructions carry compiler symbols, never Reflection.Emit or native metadata handles.
internal enum LinearInstructionKind { Constant, Argument, Add, Subtract, Multiply, Call, ConsoleLiteral, Return, LoadLocal, StoreLocal, Boolean, Not, Equal, Less, Greater, Label, Branch, BranchTrue, BranchFalse }

internal readonly record struct LinearInstruction(
    LinearInstructionKind Kind, SyntaxNode Syntax, int Integer = 0, IMethodSymbol? Method = null, string? Text = null);

internal interface ILinearMethodBuilder
{
    void DeclareLocal(SpecialType type);
    void DefineLabel();
    void Emit(LinearInstruction instruction);
}

internal sealed record LinearBodyFailure(string Detail, SyntaxNode Syntax);

// Build an instruction plan from the compiler-lowered body before touching a backend.
// Unsupported .NET bodies stay on the general generator; native emission reports the
// source-located boundary. Language rewrites remain owned by the existing Lowerer.
internal sealed class LinearMethodBody(ImmutableArray<LinearInstruction> instructions, ImmutableArray<SpecialType> localTypes, int labelCount)
{
    internal void Emit(ILinearMethodBuilder builder)
    {
        foreach (var type in localTypes) builder.DeclareLocal(type);
        for (var i = 0; i < labelCount; i++) builder.DefineLabel();
        foreach (var instruction in instructions) builder.Emit(instruction);
    }

    internal static bool HasSupportedSignature(IMethodSymbol method)
        => PrimitiveCallableSignature.TryCreate(method, out _);

    internal static bool ReturnsValue(IMethodSymbol method) => method.ReturnType.SpecialType is SpecialType.System_Int32 or SpecialType.System_Boolean;

    internal static bool TryLower(IMethodSymbol source, SemanticModel model, SyntaxNode bodySyntax,
        Func<BoundInvocationExpression, bool> permitsConsoleLiteral, out LinearMethodBody? lowered, out LinearBodyFailure? failure)
    {
        var instructions = ImmutableArray.CreateBuilder<LinearInstruction>();
        var localTypes = ImmutableArray.CreateBuilder<SpecialType>();
        var nextLabel = 0;
        var labels = new Dictionary<ILabelSymbol, int>(SymbolEqualityComparer.Default);
        var locals = new Dictionary<ILocalSymbol, int>(SymbolEqualityComparer.Default);
        LinearBodyFailure? rejected = null;
        var body = model.GetBoundNode(bodySyntax, BoundTreeView.Lowered) as BoundBlockStatement;
        var success = body is not null ? LowerBody(body) : Reject("lowered block body unavailable", bodySyntax);
        lowered = success ? new(instructions.ToImmutable(), localTypes.ToImmutable(), nextLabel) : null;
        failure = rejected;
        return success;

        bool Reject(string detail, SyntaxNode syntax)
        {
            rejected = new(detail, syntax);
            return false;
        }
        void Add(LinearInstructionKind kind, SyntaxNode syntax, int integer = 0, IMethodSymbol? method = null, string? text = null)
            => instructions.Add(new(kind, syntax, integer, method, text));

        SyntaxNode Syntax(BoundNode node) => model.GetSyntax(node) ?? bodySyntax;

        int Label(ILabelSymbol symbol)
        {
            if (!labels.TryGetValue(symbol, out var index)) labels.Add(symbol, index = nextLabel++);
            return index;
        }

        IEnumerable<BoundStatement> Flatten(BoundStatement statement)
        {
            if (statement is BoundBlockStatement block && block.LocalsToDispose.IsEmpty)
            {
                foreach (var child in block.Statements)
                    foreach (var nested in Flatten(child)) yield return nested;
            }
            else
            {
                yield return statement;
                if (statement is BoundLabeledStatement labeled)
                    foreach (var nested in Flatten(labeled.Statement)) yield return nested;
            }
        }

        bool LowerBody(BoundBlockStatement body)
        {
            if (!HasSupportedSignature(source)) return Reject("only nongeneric Int32/Boolean parameters and Int32/Boolean/Unit results", bodySyntax);
            if (!body.LocalsToDispose.IsEmpty) return Reject("scope disposal", Syntax(body));
            if (!LowerStatements(body)) return false;
            if (!ReturnsValue(source) && instructions.LastOrDefault().Kind != LinearInstructionKind.Return)
                Add(LinearInstructionKind.Return, Syntax(body));
            return true;
        }

        bool LowerStatements(BoundStatement body)
        {
            foreach (var statement in Flatten(body))
            {
                if (statement is BoundIfStatement conditionalIf)
                {
                    if (!LowerValue(conditionalIf.Condition)) return false;
                    var otherwise = nextLabel++; var end = nextLabel++;
                    Add(LinearInstructionKind.BranchFalse, Syntax(statement), otherwise);
                    if (!LowerStatements(conditionalIf.ThenNode)) return false;
                    if (instructions.LastOrDefault().Kind is not (LinearInstructionKind.Return or LinearInstructionKind.Branch))
                        Add(LinearInstructionKind.Branch, Syntax(statement), end);
                    Add(LinearInstructionKind.Label, Syntax(statement), otherwise);
                    if (conditionalIf.ElseNode is { } alternative && !LowerStatements(alternative)) return false;
                    Add(LinearInstructionKind.Label, Syntax(statement), end);
                    continue;
                }
                if (statement is BoundLabeledStatement label)
                {
                    Add(LinearInstructionKind.Label, Syntax(statement), Label(label.Label));
                    continue;
                }
                if (statement is BoundGotoStatement jump)
                {
                    Add(LinearInstructionKind.Branch, Syntax(statement), Label(jump.Target));
                    continue;
                }
                if (statement is BoundConditionalGotoStatement conditional)
                {
                    if (!LowerValue(conditional.Condition)) return false;
                    Add(conditional.JumpIfTrue ? LinearInstructionKind.BranchTrue : LinearInstructionKind.BranchFalse,
                        Syntax(statement), Label(conditional.Target));
                    continue;
                }
                if (statement is BoundLocalDeclarationStatement declaration)
                {
                    if (declaration.IsUsing) return Reject("using local", Syntax(statement));
                    foreach (var variable in declaration.Declarators)
                    {
                        if (variable.Local.Type.SpecialType is not (SpecialType.System_Int32 or SpecialType.System_Boolean) || variable.Initializer is null ||
                            variable.FixedAddressInitializer is not null || variable.FixedPinnedLocal is not null)
                            return Reject("only initialized Int32/Boolean locals", Syntax(variable));
                        if (!LowerValue(variable.Initializer)) return false;
                        var slot = locals.Count;
                        locals.Add(variable.Local, slot);
                        localTypes.Add(variable.Local.Type.SpecialType);
                        Add(LinearInstructionKind.StoreLocal, Syntax(variable), slot);
                    }
                    continue;
                }
                var assignment = statement switch
                {
                    BoundAssignmentStatement { Expression: BoundLocalAssignmentExpression localAssignment } => localAssignment,
                    BoundExpressionStatement { Expression: BoundLocalAssignmentExpression localAssignment } => localAssignment,
                    _ => null
                };
                if (assignment is not null)
                {
                    if (!locals.TryGetValue(assignment.Local, out var slot)) return Reject("undeclared local", Syntax(statement));
                    if (!LowerValue(assignment.Right)) return false;
                    Add(LinearInstructionKind.StoreLocal, Syntax(assignment), slot);
                    continue;
                }
                if (statement is BoundExpressionStatement { Expression: BoundInvocationExpression call })
                {
                    if (permitsConsoleLiteral(call) && call.Arguments.ToArray() is [BoundLiteralExpression { Value: string text }])
                    {
                        Add(LinearInstructionKind.ConsoleLiteral, Syntax(call), method: call.Method, text: text);
                        continue;
                    }
                    if (ReturnsValue(call.Method)) return Reject("discarded value calls", Syntax(statement));
                    if (!LowerValue(call)) return false;
                    continue;
                }
                if (statement is BoundReturnStatement { Expression: null or BoundUnitExpression } && !ReturnsValue(source))
                {
                    Add(LinearInstructionKind.Return, Syntax(statement));
                    continue;
                }
                if (statement is not BoundReturnStatement { Expression: { } value })
                    return Reject("unsupported lowered statement " + statement.GetType().Name, Syntax(statement));
                if (!LowerValue(value)) return false;
                Add(LinearInstructionKind.Return, Syntax(statement));
            }
            return true;
        }

        bool LowerValue(BoundExpression expression)
        {
            switch (expression)
            {
                case BoundLiteralExpression { Value: bool boolean }:
                    Add(LinearInstructionKind.Boolean, Syntax(expression), boolean ? 1 : 0); return true;
                case BoundLiteralExpression { Value: int value }:
                    Add(LinearInstructionKind.Constant, Syntax(expression), value); return true;
                case BoundLocalAccess local:
                    if (!locals.TryGetValue(local.Local, out var slot)) return Reject("undeclared local", Syntax(expression));
                    Add(LinearInstructionKind.LoadLocal, Syntax(expression), slot); return true;
                case BoundParameterAccess parameter:
                    var index = source.Parameters.IndexOf(parameter.Parameter, 0, source.Parameters.Length, SymbolEqualityComparer.Default);
                    if (index < 0) return Reject("captured parameter", Syntax(expression));
                    Add(LinearInstructionKind.Argument, Syntax(expression), index); return true;
                case BoundUnaryExpression { Operator.OperatorKind: BoundUnaryOperatorKind.LogicalNot } unary:
                    if (!LowerValue(unary.Operand)) return false;
                    Add(LinearInstructionKind.Not, Syntax(expression)); return true;
                case BoundParenthesizedExpression parenthesized:
                    return LowerValue(parenthesized.Expression);
                case BoundConversionExpression { IsIdentity: true } conversion:
                    return LowerValue(conversion.Expression);
                case BoundBinaryExpression logical when logical.Operator.MethodSymbol is null &&
                    logical.Operator.LeftType.SpecialType == SpecialType.System_Boolean &&
                    logical.Operator.RightType.SpecialType == SpecialType.System_Boolean &&
                    logical.Operator.OperatorKind is OperatorKind.LogicalAnd or OperatorKind.LogicalOr:
                    var shortCircuit = nextLabel++;
                    var completed = nextLabel++;
                    var isOr = logical.Operator.OperatorKind == OperatorKind.LogicalOr;
                    if (!LowerValue(logical.Left)) return false;
                    Add(isOr ? LinearInstructionKind.BranchTrue : LinearInstructionKind.BranchFalse, Syntax(expression), shortCircuit);
                    if (!LowerValue(logical.Right)) return false;
                    Add(LinearInstructionKind.Branch, Syntax(expression), completed);
                    Add(LinearInstructionKind.Label, Syntax(expression), shortCircuit);
                    Add(LinearInstructionKind.Boolean, Syntax(expression), isOr ? 1 : 0);
                    Add(LinearInstructionKind.Label, Syntax(expression), completed);
                    return true;
                case BoundBinaryExpression binary when binary.Operator.MethodSymbol is null &&
                    ((binary.Operator.LeftType.SpecialType == SpecialType.System_Int32 &&
                      binary.Operator.RightType.SpecialType == SpecialType.System_Int32) ||
                     (binary.Operator.LeftType.SpecialType == SpecialType.System_Boolean &&
                      binary.Operator.RightType.SpecialType == SpecialType.System_Boolean &&
                      binary.Operator.OperatorKind is OperatorKind.Equality or OperatorKind.Inequality)):
                    if (binary.Operator.OperatorKind is not (OperatorKind.Addition or OperatorKind.Subtraction or OperatorKind.Multiplication or
                        OperatorKind.Equality or OperatorKind.LessThan or OperatorKind.GreaterThan or
                        OperatorKind.Inequality or OperatorKind.LessThanOrEqual or OperatorKind.GreaterThanOrEqual))
                        return Reject("binary operator " + binary.Operator.OperatorKind, Syntax(expression));
                    if (!LowerValue(binary.Left) || !LowerValue(binary.Right)) return false;
                    Add(binary.Operator.OperatorKind switch
                    {
                        OperatorKind.Addition => LinearInstructionKind.Add,
                        OperatorKind.Subtraction => LinearInstructionKind.Subtract,
                        OperatorKind.Multiplication => LinearInstructionKind.Multiply,
                        OperatorKind.Equality or OperatorKind.Inequality => LinearInstructionKind.Equal,
                        OperatorKind.LessThan or OperatorKind.GreaterThanOrEqual => LinearInstructionKind.Less,
                        _ => LinearInstructionKind.Greater
                    }, Syntax(expression));
                    if (binary.Operator.OperatorKind is OperatorKind.Inequality or OperatorKind.LessThanOrEqual or OperatorKind.GreaterThanOrEqual)
                        Add(LinearInstructionKind.Not, Syntax(expression));
                    return true;
                case BoundInvocationExpression call when call.Method.IsStatic && call.Receiver is null or BoundTypeExpression && call.ExtensionReceiver is null:
                    if (!HasSupportedSignature(call.Method)) return Reject("only nongeneric Int32/Boolean parameters and Int32/Boolean/Unit results: " + call.Method.Name, Syntax(expression));
                    var arguments = call.Arguments.ToArray();
                    if (arguments.Length != call.Method.Parameters.Length) return Reject("optional/expanded arguments", Syntax(expression));
                    foreach (var argument in arguments)
                    {
                        if (!LowerValue(argument)) return false;
                    }
                    Add(LinearInstructionKind.Call, Syntax(expression), method: call.Method);
                    return true;
                default: return Reject("lowered expression " + expression.GetType().Name, Syntax(expression));
            }
        }
    }
}
