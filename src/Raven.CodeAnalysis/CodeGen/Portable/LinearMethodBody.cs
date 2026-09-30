using System.Collections.Immutable;

using Raven.CodeAnalysis.Operations;
using Raven.CodeAnalysis.Symbols;
using Raven.CodeAnalysis.Syntax;

using OperatorKind = Raven.CodeAnalysis.Operations.BinaryOperatorKind;

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

// Lower completely before touching a backend builder. Unsupported .NET bodies can safely
// stay on the general generator; native emission reports the same source-located boundary.
internal sealed class LinearMethodBody(ImmutableArray<LinearInstruction> instructions)
{
    internal void Emit(ILinearMethodBuilder builder)
    {
        foreach (var instruction in instructions) builder.Emit(instruction);
    }

    internal static bool HasSupportedSignature(IMethodSymbol method)
        => Int32CallableSignature.TryCreate(method, out _);

    internal static bool ReturnsValue(IMethodSymbol method) => method.ReturnType.SpecialType == SpecialType.System_Int32;

    internal static bool TryLower(IMethodSymbol source, IBlockOperation body,
        Func<IInvocationOperation, bool> permitsConsoleLiteral, out LinearMethodBody? lowered, out LinearBodyFailure? failure)
    {
        var instructions = ImmutableArray.CreateBuilder<LinearInstruction>();
        LinearBodyFailure? rejected = null;
        var success = LowerBody();
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

        bool LowerBody()
        {
            if (!HasSupportedSignature(source)) return Reject("only nongeneric Int32 parameters and Int32/Unit results", body.Syntax);
            foreach (var statement in body.Operations)
            {
                if (statement is IExpressionStatementOperation { Operation: IInvocationOperation call })
                {
                    if (permitsConsoleLiteral(call) && call.Arguments.Length == 1 &&
                        call.Arguments[0] is IArgumentOperation { IsNamed: false, Value: ILiteralOperation { Value: string text } })
                    {
                        Add(LinearInstructionKind.ConsoleLiteral, call.Syntax, method: call.TargetMethod, text: text);
                        continue;
                    }
                    if (ReturnsValue(call.TargetMethod)) return Reject("discarded value calls", statement.Syntax);
                    if (!LowerValue(call)) return false;
                    continue;
                }
                if (statement is IReturnOperation { ReturnedValue: null } && !ReturnsValue(source))
                {
                    Add(LinearInstructionKind.Return, statement.Syntax);
                    continue;
                }
                if (statement is not IReturnOperation { ReturnedValue: { } value })
                    return Reject("only value-return statements", statement.Syntax);
                if (!LowerValue(value)) return false;
                Add(LinearInstructionKind.Return, statement.Syntax);
            }
            if (!ReturnsValue(source) && body.Operations.LastOrDefault() is not IReturnOperation)
                Add(LinearInstructionKind.Return, body.Syntax);
            return true;
        }

        bool LowerValue(IOperation operation)
        {
            switch (operation)
            {
                case ILiteralOperation { Value: int value }:
                    Add(LinearInstructionKind.Constant, operation.Syntax, value); return true;
                case IParameterReferenceOperation parameter:
                    var index = source.Parameters.IndexOf(parameter.Parameter, 0, source.Parameters.Length, SymbolEqualityComparer.Default);
                    if (index < 0) return Reject("captured parameter", operation.Syntax);
                    Add(LinearInstructionKind.Argument, operation.Syntax, index); return true;
                case IParenthesizedOperation { Operand: { } operand }:
                    return LowerValue(operand);
                case IBinaryOperation binary when !binary.IsChecked && !binary.IsLifted && binary.OperatorMethod is null &&
                    binary.Type?.SpecialType == SpecialType.System_Int32 && binary.Left is not null && binary.Right is not null:
                    if (binary.OperatorKind is not (OperatorKind.Add or OperatorKind.Subtract or OperatorKind.Multiply))
                        return Reject("binary operator " + binary.OperatorKind, operation.Syntax);
                    if (!LowerValue(binary.Left) || !LowerValue(binary.Right)) return false;
                    Add(binary.OperatorKind switch
                    {
                        OperatorKind.Add => LinearInstructionKind.Add,
                        OperatorKind.Subtract => LinearInstructionKind.Subtract,
                        _ => LinearInstructionKind.Multiply
                    }, operation.Syntax);
                    return true;
                case IInvocationOperation call when call.Instance is null:
                    if (!HasSupportedSignature(call.TargetMethod)) return Reject("only nongeneric Int32 parameters and Int32/Unit results: " + call.TargetMethod.Name, operation.Syntax);
                    if (call.Arguments.Length != call.TargetMethod.Parameters.Length) return Reject("optional/expanded arguments", operation.Syntax);
                    foreach (var argument in call.Arguments)
                    {
                        if (argument is not IArgumentOperation { IsNamed: false, Value: { } argumentValue })
                            return Reject("named or unavailable argument", operation.Syntax);
                        if (!LowerValue(argumentValue)) return false;
                    }
                    Add(LinearInstructionKind.Call, operation.Syntax, method: call.TargetMethod);
                    return true;
                default: return Reject("operation " + operation.Kind, operation.Syntax);
            }
        }
    }
}
