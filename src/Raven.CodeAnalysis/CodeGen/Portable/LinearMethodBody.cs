using System.Collections.Immutable;

using Raven.CodeAnalysis.Symbols;
using Raven.CodeAnalysis.Syntax;

using OperatorKind = Raven.CodeAnalysis.BinaryOperatorKind;

namespace Raven.CodeAnalysis.CodeGen.Portable;

// Logical instructions carry compiler symbols, never Reflection.Emit or native metadata handles.
internal enum LinearInstructionKind { Constant, Argument, Add, Subtract, Multiply, Call, ConsoleLiteral, Return }

internal readonly record struct LinearInstruction(
    LinearInstructionKind Kind, SyntaxNode Syntax, int Integer = 0, IMethodSymbol? Method = null, string? Text = null);

internal interface ILinearMethodBuilder
{
    void Emit(LinearInstruction instruction);
}

internal sealed record LinearBodyFailure(string Detail, SyntaxNode Syntax);

// Build an instruction plan from the compiler-lowered body before touching a backend.
// Unsupported .NET bodies stay on the general generator; native emission reports the
// source-located boundary. Language rewrites remain owned by the existing Lowerer.
internal sealed class LinearMethodBody(ImmutableArray<LinearInstruction> instructions)
{
    internal void Emit(ILinearMethodBuilder builder)
    {
        foreach (var instruction in instructions) builder.Emit(instruction);
    }

    internal static bool HasSupportedSignature(IMethodSymbol method)
        => Int32CallableSignature.TryCreate(method, out _);

    internal static bool ReturnsValue(IMethodSymbol method) => method.ReturnType.SpecialType == SpecialType.System_Int32;

    internal static bool TryLower(IMethodSymbol source, SemanticModel model, SyntaxNode bodySyntax,
        Func<BoundInvocationExpression, bool> permitsConsoleLiteral, out LinearMethodBody? lowered, out LinearBodyFailure? failure)
    {
        var instructions = ImmutableArray.CreateBuilder<LinearInstruction>();
        LinearBodyFailure? rejected = null;
        var body = model.GetBoundNode(bodySyntax, BoundTreeView.Lowered) as BoundBlockStatement;
        var success = body is not null ? LowerBody(body) : Reject("lowered block body unavailable", bodySyntax);
        lowered = success ? new(instructions.ToImmutable()) : null;
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

        bool LowerBody(BoundBlockStatement body)
        {
            if (!HasSupportedSignature(source)) return Reject("only nongeneric Int32 parameters and Int32/Unit results", bodySyntax);
            if (!body.LocalsToDispose.IsEmpty) return Reject("scope disposal", Syntax(body));
            foreach (var statement in body.Statements)
            {
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
                    return Reject("only value-return statements", Syntax(statement));
                if (!LowerValue(value)) return false;
                Add(LinearInstructionKind.Return, Syntax(statement));
            }
            if (!ReturnsValue(source) && body.Statements.LastOrDefault() is not BoundReturnStatement)
                Add(LinearInstructionKind.Return, Syntax(body));
            return true;
        }

        bool LowerValue(BoundExpression expression)
        {
            switch (expression)
            {
                case BoundLiteralExpression { Value: int value }:
                    Add(LinearInstructionKind.Constant, Syntax(expression), value); return true;
                case BoundParameterAccess parameter:
                    var index = source.Parameters.IndexOf(parameter.Parameter, 0, source.Parameters.Length, SymbolEqualityComparer.Default);
                    if (index < 0) return Reject("captured parameter", Syntax(expression));
                    Add(LinearInstructionKind.Argument, Syntax(expression), index); return true;
                case BoundParenthesizedExpression parenthesized:
                    return LowerValue(parenthesized.Expression);
                case BoundConversionExpression { IsIdentity: true } conversion:
                    return LowerValue(conversion.Expression);
                case BoundBinaryExpression binary when binary.Operator.MethodSymbol is null &&
                    binary.Type.SpecialType == SpecialType.System_Int32:
                    if (binary.Operator.OperatorKind is not (OperatorKind.Addition or OperatorKind.Subtraction or OperatorKind.Multiplication))
                        return Reject("binary operator " + binary.Operator.OperatorKind, Syntax(expression));
                    if (!LowerValue(binary.Left) || !LowerValue(binary.Right)) return false;
                    Add(binary.Operator.OperatorKind switch
                    {
                        OperatorKind.Addition => LinearInstructionKind.Add,
                        OperatorKind.Subtraction => LinearInstructionKind.Subtract,
                        _ => LinearInstructionKind.Multiply
                    }, Syntax(expression));
                    return true;
                case BoundInvocationExpression call when call.Method.IsStatic && call.Receiver is null or BoundTypeExpression && call.ExtensionReceiver is null:
                    if (!HasSupportedSignature(call.Method)) return Reject("only nongeneric Int32 parameters and Int32/Unit results: " + call.Method.Name, Syntax(expression));
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
