using System.Collections.Immutable;

using Raven.CodeAnalysis.Metadata;
using Raven.CodeAnalysis.Symbols;
using Raven.CodeAnalysis.Syntax;

using OperatorKind = Raven.CodeAnalysis.BinaryOperatorKind;

namespace Raven.CodeAnalysis.CodeGen.Portable;

// Logical instructions carry compiler symbols, never Reflection.Emit or native metadata handles.
internal enum LinearInstructionKind { DirectInstanceCall, BaseConstructorCall, ConstrainedCall, EnumFromInt32, EnumToInt32, Constant, Argument, Add, Subtract, Multiply, Call, ConsoleWrite, String, Return, LoadLocal, StoreLocal, Boolean, Not, Equal, Less, Greater, Label, Branch, BranchTrue, BranchFalse, Pop, Constant64, Convert64, Convert32, Negate, Complement, Divide, Remainder, BitwiseAnd, BitwiseOr, BitwiseXor, ShiftLeft, ShiftRight, Receiver, LoadField, StoreField, InstanceCall, InterfaceCall, NewObject, NewArray, LoadElement, StoreElement, ArrayLength, Duplicate, DefaultValue, LocalAddress, LoadIndirect, StoreIndirect, ValueInstanceCall, CompilerFailure, FunctionBind, FunctionInvoke, ReferenceConvert, ConvertByte, BoxToObject, FieldAddress, ReferenceIsNull, TypeTest, UnboxAny, LoadCapture, ArgumentAddress, ConstantSingle, ConstantDouble, ConvertSingle, ConvertDouble, LessOrUnordered, GreaterOrUnordered, ConvertSByte, ConvertInt16, ConvertUInt16, ConvertUInt32, ConvertUInt64, UnsignedDivide, UnsignedRemainder, UnsignedShiftRight, UnsignedLess, UnsignedGreater, UnsignedConvertDouble, LoadTypeToken }

internal readonly record struct LinearInstruction(
    LinearInstructionKind Kind, SyntaxNode Syntax, int Integer = 0, IMethodSymbol? Method = null, string? Text = null, long Long = 0, IFieldSymbol? Field = null, ITypeSymbol? Type = null);

internal interface ILinearMethodBuilder
{
    void DeclareLocal(EmissionType type);
    void DefineLabel();
    void Emit(LinearInstruction instruction);
}

internal sealed record LinearBodyFailure(string Detail, SyntaxNode Syntax);

// Logical value types carry compiler identity, never backend handles.
internal readonly record struct EmissionType(EmissionPrimitiveType? Primitive = null, INamedTypeSymbol? Nominal = null, IArrayTypeSymbol? Array = null, ITypeParameterSymbol? MethodParameter = null, ITypeParameterSymbol? OwnerParameter = null, bool IsByReference = false, IPointerTypeSymbol? Pointer = null);

// Build an instruction plan from the compiler-lowered body before touching a backend.
// Unsupported .NET bodies stay on the general generator; native emission reports the
// source-located boundary. Language rewrites remain owned by the existing Lowerer.
internal sealed class LinearMethodBody(ImmutableArray<LinearInstruction> instructions, ImmutableArray<EmissionType> localTypes, int labelCount, ImmutableArray<(BoundFunctionExpression Expression, SyntaxNode Syntax)> functions)
{
    internal ImmutableArray<(BoundFunctionExpression Expression, SyntaxNode Syntax)> Functions { get; } = functions;
    internal void Emit(ILinearMethodBuilder builder)
    {
        foreach (var type in localTypes)
            builder.DeclareLocal(type);
        for (var i = 0; i < labelCount; i++) builder.DefineLabel();
        foreach (var instruction in instructions) builder.Emit(instruction);
    }

    internal static bool HasSupportedSignature(IMethodSymbol method)
        => CallableSignature.TryCreate(method, out _);

    internal static bool ReturnsValue(IMethodSymbol method)
        => (method.ContainingType?.TypeKind != TypeKind.Delegate || method.ContainingType.Name == "Func") && (method.OriginalDefinition ?? method).ReturnType is ITypeParameterSymbol ||
            CallableSignature.TryType(method.ReturnType, true, out var result) && result.Primitive != EmissionPrimitiveType.NoResult;

    // Lowered match/conditional bodies can retain jumps after a terminating arm.
    // Keep label identities stable, but do not emit instructions with no incoming path.
    private static ImmutableArray<LinearInstruction> ReachableInstructions(ImmutableArray<LinearInstruction> instructions)
    {
        if (instructions.IsEmpty) return instructions;
        var labels = new Dictionary<int, int>();
        for (var i = 0; i < instructions.Length; i++)
            if (instructions[i].Kind == LinearInstructionKind.Label) labels.Add(instructions[i].Integer, i);
        var reachable = new bool[instructions.Length];
        var pending = new Stack<int>();
        pending.Push(0);
        while (pending.TryPop(out var index))
        {
            if (index >= instructions.Length || reachable[index]) continue;
            reachable[index] = true;
            var instruction = instructions[index];
            if (instruction.Kind is LinearInstructionKind.Branch or LinearInstructionKind.BranchTrue or LinearInstructionKind.BranchFalse)
                pending.Push(labels[instruction.Integer]);
            if (instruction.Kind is not (LinearInstructionKind.Branch or LinearInstructionKind.Return or LinearInstructionKind.CompilerFailure))
                pending.Push(index + 1);
        }
        return [.. instructions.Where((instruction, index) => reachable[index] || instruction.Kind == LinearInstructionKind.Label)];
    }

    internal static bool TryLower(IMethodSymbol source, SemanticModel model, SyntaxNode bodySyntax,
        Func<BoundInvocationExpression, bool> permitsConsoleWrite, out LinearMethodBody? lowered, out LinearBodyFailure? failure, EmissionCapabilities? capabilities = null, BoundFunctionExpression? functionBody = null, BoundBlockStatement? preparedBody = null)
    {
        var captures = functionBody?.CapturedVariables.ToArray() ?? [];
        var isStaticBody = functionBody is not null ? captures.Length == 0 : source.IsStatic;
        bool ReturnsValue(IMethodSymbol method) =>
            method.ContainingType is { TypeKind: TypeKind.Delegate } callable && capabilities is not null &&
                CallableSignature.TryFunction(callable, out var callableShape, capabilities) ? callableShape.ReturnsValue :
            (method.ContainingType?.TypeKind != TypeKind.Delegate || method.ContainingType.Name == "Func") && (method.OriginalDefinition ?? method).ReturnType is ITypeParameterSymbol ||
            TryType(method.ReturnType, true, out var result) && result.Primitive != EmissionPrimitiveType.NoResult;
        bool TryType(ITypeSymbol type, bool result, out EmissionType value) => CallableSignature.TryType(type, result, out value, capabilities);
        bool TrySignature(IMethodSymbol method, out CallableSignature signature) => CallableSignature.TryCreate(method, out signature, capabilities);
        var functions = ImmutableArray.CreateBuilder<(BoundFunctionExpression, SyntaxNode)>();
        var instructions = ImmutableArray.CreateBuilder<LinearInstruction>();
        var localTypes = ImmutableArray.CreateBuilder<EmissionType>();
        var nextLabel = 0;
        var labels = new Dictionary<ILabelSymbol, int>(SymbolEqualityComparer.Default);
        // Lowering can create distinct temporaries with the same name and no source
        // declaration. Their storage identity is the bound symbol instance.
        var locals = new Dictionary<ILocalSymbol, int>(ReferenceEqualityComparer.Instance);
        LinearBodyFailure? rejected = null;
        // Arrow clauses expose their bound statement block in the original view, as
        // consumed by the general generator. Reuse compiler lowering for conversions
        // and Unit expression statements instead of synthesizing backend returns.
        var body = preparedBody ?? (functionBody is not null ? FunctionBlock(functionBody) : source.MethodKind == MethodKind.Constructor && bodySyntax is ClassDeclarationSyntax or StructDeclarationSyntax
            ? new BoundBlockStatement([]) : model.Compilation.TryGetSynthesizedMethodBody(source, BoundTreeView.Lowered, out var synthesized) && synthesized is not null
            ? synthesized : bodySyntax is ArrowExpressionClauseSyntax
            ? model.GetBoundNode(bodySyntax, BoundTreeView.Original) is BoundBlockStatement arrowBody
                ? Lowerer.LowerBlock(source, arrowBody) : null
            : model.GetBoundNode(bodySyntax, BoundTreeView.Lowered) as BoundBlockStatement);
        if (body is not null && source.MethodKind == MethodKind.Constructor)
        {
            var initializers = FieldInitializationPlan.Create(model.Compilation, source);
            // A synthesized struct default constructor zero-initializes storage. Keep this
            // semantic initialization in the shared body plan, before declared initializers.
            if (bodySyntax is StructDeclarationSyntax)
                initializers = source.ContainingType!.GetMembers().OfType<IFieldSymbol>().Where(f => !f.IsStatic)
                    .Select(field => new BoundAssignmentStatement(new BoundFieldAssignmentExpression(
                        new BoundSelfExpression(source.ContainingType), field, new BoundDefaultValueExpression(field.Type),
                        model.Compilation.GetSpecialType(SpecialType.System_Unit))))
                    .Concat(initializers);
            var initialization = Lowerer.LowerBlock(source, new BoundBlockStatement(initializers.ToArray()));
            body = new BoundBlockStatement([initialization, body]);
        }
        if (body is not null && (capabilities?.AllowsArrays == true || capabilities?.AllowsRangeEnumeration == true))
            body = Lowerer.LowerPortableLoops(source, body, capabilities!.AllowsArrays, capabilities.AllowsRangeEnumeration);
        var success = body is not null ? LowerBaseInitializer() && LowerBody(body) : Reject("lowered block body unavailable", bodySyntax);
        bool LowerBaseInitializer()
        {
            if (source.MethodKind != MethodKind.Constructor || source.ContainingType is not { IsReferenceType: true, BaseType: { } parent } || parent.SpecialType == SpecialType.System_Object && !SourceTypePlan.IsSourceObjectRoot(parent)) return true;
            if (capabilities?.AllowsLocalClassInheritance != true) return Reject("class base initialization", bodySyntax);
            var initializer = (source as SourceMethodSymbol)?.ConstructorInitializer;
            var target = initializer?.Constructor ?? parent.GetMembers().OfType<IMethodSymbol>()
                .SingleOrDefault(m => m.MethodKind == MethodKind.Constructor && m.Parameters.Length == 0 && !m.IsStatic);
            if (target is null || !SymbolEqualityComparer.Default.Equals(target.ContainingType, parent)) return Reject("direct base constructor unavailable", bodySyntax);
            var arguments = initializer?.Arguments.ToArray() ?? [];
            if (arguments.Length != target.Parameters.Length) return Reject("base constructor argument shape", bodySyntax);
            Add(LinearInstructionKind.Receiver, bodySyntax);
            for (int i = 0; i < arguments.Length; i++)
                if (!LowerValue(arguments[i], target.Parameters[i].Type)) return false;
            Add(LinearInstructionKind.BaseConstructorCall, bodySyntax, method: target);
            return true;
        }
        if (success && capabilities is not null)
        {
            foreach (var instruction in instructions)
            {
                if (!capabilities.Allows(instruction.Kind))
                {
                    success = Reject("target does not support instruction " + instruction.Kind, instruction.Syntax);
                    break;
                }
            }
        }
        lowered = success ? new(ReachableInstructions(instructions.ToImmutable()), localTypes.ToImmutable(), nextLabel, functions.ToImmutable()) : null;
        failure = rejected;
        return success;

        static BoundBlockStatement FunctionBlock(BoundFunctionExpression function)
        {
            if (function.Body is BoundBlockExpression block)
            {
                var statements = block.Statements.ToImmutableArray();
                if (statements.Length > 0 && statements[^1] is BoundExpressionStatement last &&
                    function.ReturnType.SpecialType != SpecialType.System_Unit)
                    statements = statements.SetItem(statements.Length - 1, new BoundReturnStatement(last.Expression));
                return new BoundBlockStatement(statements, block.LocalsToDispose);
            }
            return new BoundBlockStatement(function.ReturnType.SpecialType == SpecialType.System_Unit
                ? [new BoundExpressionStatement(function.Body)] : [new BoundReturnStatement(function.Body)]);
        }

        bool Reject(string detail, SyntaxNode syntax)
        {
            rejected = new(detail, syntax);
            return false;
        }
        void Add(LinearInstructionKind kind, SyntaxNode syntax, int integer = 0, IMethodSymbol? method = null, string? text = null)
        {
            if (kind == LinearInstructionKind.Call && capabilities?.Allows(LinearInstructionKind.ConstrainedCall) == true &&
                method is NativeSelfMethodSymbol { IsStatic: true, ImplementingType: ITypeParameterSymbol } self)
                instructions.Add(new(LinearInstructionKind.ConstrainedCall, syntax, Method: self.AdapterMethod, Type: self.ImplementingType));
            else instructions.Add(new(kind, syntax, integer, method, text));
        }

        SyntaxNode Syntax(BoundNode node) => model.GetSyntax(node) ?? bodySyntax;

        int Label(ILabelSymbol symbol)
        {
            if (!labels.TryGetValue(symbol, out var index)) labels.Add(symbol, index = nextLabel++);
            return index;
        }

        static BoundStatement NormalizeStatementExpression(BoundStatement statement)
        {
            if (statement is BoundExpressionStatement expressionStatement)
            {
                var expression = expressionStatement.Expression;
                while (expression is BoundRequiredResultExpression required)
                    expression = required.Operand;
                // A conversion around a terminal expression is unreachable. The
                // return's own operand already carries the method return conversion.
                var terminal = expression;
                while (terminal is BoundConversionExpression { IsUserDefined: false } || terminal is BoundRequiredResultExpression)
                    terminal = terminal is BoundConversionExpression c ? c.Expression : ((BoundRequiredResultExpression)terminal).Operand;
                if (terminal is BoundReturnExpression returned)
                    return new BoundReturnStatement(returned.Expression);
                // At a statement boundary the expression is discarded; wrappers used
                // by match lowering must not hide blocks, calls or assignments.
                if (!ReferenceEquals(expression, expressionStatement.Expression))
                    return new BoundExpressionStatement(expression);
            }
            return statement;
        }

        IEnumerable<BoundStatement> Flatten(BoundStatement statement)
        {
            statement = NormalizeStatementExpression(statement);
            if (statement is BoundBlockStatement block && block.LocalsToDispose.IsEmpty)
            {
                foreach (var child in block.Statements)
                    foreach (var nested in Flatten(child)) yield return nested;
            }
            else if (statement is BoundLocalDeclarationStatement { IsUsing: false } declaration &&
                declaration.Declarators.Any(variable => variable.Initializer is BoundBlockExpression))
            {
                foreach (var variable in declaration.Declarators)
                {
                    if (variable.FixedAddressInitializer is null && variable.FixedPinnedLocal is null &&
                        variable.Initializer is BoundBlockExpression { LocalsToDispose.IsEmpty: true } initializer &&
                        initializer.Statements.ToImmutableArray() is var parts && !parts.IsEmpty &&
                        parts[^1] is BoundExpressionStatement trailing)
                    {
                        // An initializer is a statement evaluation boundary. Hoist its
                        // lowered prefix so early returns do not live inside a value block.
                        foreach (var prefix in parts.AsSpan()[..^1].ToArray())
                            foreach (var nested in Flatten(prefix)) yield return nested;
                        yield return new BoundLocalDeclarationStatement([
                            new BoundVariableDeclarator(variable.Local, trailing.Expression)]);
                    }
                    else yield return new BoundLocalDeclarationStatement([variable]);
                }
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
            if (!TrySignature(source, out var signature)) return Reject("only supported value signatures and unconstrained generics (Unit only as result)", bodySyntax);
            if (functionBody is not null) signature = signature with { IsInstance = false };
            if (capabilities is not null && !capabilities.Allows(signature))
                return Reject("target does not support callable signature types", bodySyntax);
            if (!body.LocalsToDispose.IsEmpty) return Reject("scope disposal", Syntax(body));
            if (!LowerStatements(body)) return false;
            if (!ReturnsValue(source) && instructions.LastOrDefault().Kind is not (LinearInstructionKind.Return or LinearInstructionKind.CompilerFailure))
                Add(LinearInstructionKind.Return, Syntax(body));
            return true;
        }

        bool LowerPattern(BoundExpression input, BoundPattern pattern, int fail, SyntaxNode syntax)
        {
            if (!LowerValue(input)) return false;
            return PatternValue(input.Type, pattern, fail, syntax);
        }

        static string? GraphemeText(object? value) => value switch
        {
            GraphemeLiteralValue grapheme => grapheme.Text,
            char character => character.ToString(),
            System.Text.Rune scalar => scalar.ToString(),
            _ => null
        };

        bool LowerGrapheme(string text, SyntaxNode syntax)
        {
            var owner = model.Compilation.GetSpecialType(SpecialType.System_Char);
            var factories = owner.GetMembers("FromString").OfType<IMethodSymbol>().Where(m =>
                m.IsStatic && m.Arity == 0 && m.Parameters is [{ RefKind: RefKind.None, Type.SpecialType: SpecialType.System_String }] &&
                m.ReturnType.SpecialType == SpecialType.System_Char && m.DeclaredAccessibility == Accessibility.Public).ToArray();
            if (!model.Compilation.Options.UseGraphemeChar || factories.Length != 1 || !TrySignature(factories[0], out var signature) ||
                capabilities?.Allows(signature) != true) return Reject("grapheme construction contract unavailable", syntax);
            Add(LinearInstructionKind.String, syntax, text: text);
            Add(LinearInstructionKind.Call, syntax, method: factories[0]);
            return true;
        }

        bool LowerBranch(BoundExpression condition, int target, bool jumpIfTrue, SyntaxNode syntax)
        {
            if (condition is BoundParenthesizedExpression parenthesized)
                return LowerBranch(parenthesized.Expression, target, jumpIfTrue, syntax);
            if (condition is BoundUnaryExpression { Operator.OperatorKind: BoundUnaryOperatorKind.LogicalNot } negated)
                return LowerBranch(negated.Operand, target, !jumpIfTrue, syntax);
            if (condition is BoundBinaryExpression { Operator.MethodSymbol: null } logical &&
                logical.Operator.OperatorKind is OperatorKind.LogicalAnd or OperatorKind.LogicalOr)
            {
                bool shortCircuitOnTrue = logical.Operator.OperatorKind == OperatorKind.LogicalOr;
                if (shortCircuitOnTrue == jumpIfTrue)
                    return LowerBranch(logical.Left, target, jumpIfTrue, syntax) && LowerBranch(logical.Right, target, jumpIfTrue, syntax);
                var skipped = nextLabel++;
                if (!LowerBranch(logical.Left, skipped, shortCircuitOnTrue, syntax) ||
                    !LowerBranch(logical.Right, target, jumpIfTrue, syntax)) return false;
                Add(LinearInstructionKind.Label, syntax, skipped);
                return true;
            }
            if (condition is BoundIsPatternExpression pattern && capabilities?.AllowsCasePatterns == true)
            {
                if (!jumpIfTrue) return LowerPattern(pattern.Expression, pattern.Pattern, target, syntax);
                var failed = nextLabel++;
                if (!LowerPattern(pattern.Expression, pattern.Pattern, failed, syntax)) return false;
                Add(LinearInstructionKind.Branch, syntax, target);
                Add(LinearInstructionKind.Label, syntax, failed);
                return true;
            }
            if (!LowerValue(condition)) return false;
            Add(jumpIfTrue ? LinearInstructionKind.BranchTrue : LinearInstructionKind.BranchFalse, syntax, target);
            return true;
        }

        bool PatternValue(ITypeSymbol input, BoundPattern pattern, int fail, SyntaxNode syntax)
        {
            if (pattern is BoundDiscardPattern) { Add(LinearInstructionKind.Pop, syntax); return true; }
            if (pattern is BoundNotPattern negated && !negated.GetDesignators().Any())
            {
                var accepted = nextLabel++;
                if (!PatternValue(input, negated.Pattern, accepted, syntax)) return false;
                Add(LinearInstructionKind.Branch, syntax, fail);
                Add(LinearInstructionKind.Label, syntax, accepted);
                return true;
            }
            if (pattern is BoundDeclarationPattern { Designator: BoundSingleVariableDesignator } referencePattern &&
                input.IsReferenceType && referencePattern.DeclaredType.IsReferenceType && referencePattern.DeclaredType.TypeKind != TypeKind.Delegate &&
                capabilities?.Allows(LinearInstructionKind.TypeTest) == true && capabilities.Allows(LinearInstructionKind.ReferenceConvert) &&
                TryType(input, false, out var inputReferenceType) && capabilities.Allows(inputReferenceType) &&
                TryType(referencePattern.DeclaredType, false, out var referenceType) && capabilities.Allows(referenceType))
            {
                var extractedReference = localTypes.Count; localTypes.Add(inputReferenceType);
                Add(LinearInstructionKind.StoreLocal, syntax, extractedReference);
                Add(LinearInstructionKind.LoadLocal, syntax, extractedReference);
                instructions.Add(new(LinearInstructionKind.TypeTest, syntax, Type: referencePattern.DeclaredType));
                Add(LinearInstructionKind.BranchFalse, syntax, fail);
                Add(LinearInstructionKind.LoadLocal, syntax, extractedReference);
                instructions.Add(new(LinearInstructionKind.ReferenceConvert, syntax, Type: referencePattern.DeclaredType));
                return PatternDesignator(referencePattern.Designator, referencePattern.DeclaredType, syntax);
            }
            if (pattern is BoundDeclarationPattern { Designator: BoundSingleVariableDesignator } valuePattern &&
                input.IsReferenceType && valuePattern.DeclaredType.IsValueType &&
                capabilities?.Allows(LinearInstructionKind.TypeTest) == true && capabilities.Allows(LinearInstructionKind.UnboxAny) &&
                TryType(input, false, out var boxedInputType) && capabilities.Allows(boxedInputType) &&
                TryType(valuePattern.DeclaredType, false, out var unboxedPatternType) && capabilities.Allows(unboxedPatternType))
            {
                var boxedInput = localTypes.Count; localTypes.Add(boxedInputType);
                Add(LinearInstructionKind.StoreLocal, syntax, boxedInput);
                Add(LinearInstructionKind.LoadLocal, syntax, boxedInput);
                instructions.Add(new(LinearInstructionKind.TypeTest, syntax, Type: valuePattern.DeclaredType));
                Add(LinearInstructionKind.BranchFalse, syntax, fail);
                Add(LinearInstructionKind.LoadLocal, syntax, boxedInput);
                instructions.Add(new(LinearInstructionKind.UnboxAny, syntax, Type: valuePattern.DeclaredType));
                return PatternDesignator(valuePattern.Designator, valuePattern.DeclaredType, syntax);
            }
            if (pattern is BoundConstantPattern constant && input.IsReferenceType &&
                (constant.LiteralType is { ConstantValue: null } || constant.Expression is { } value && IsNullLiteral(value)) &&
                constant.Designator is null or BoundDiscardDesignator && capabilities?.Allows(LinearInstructionKind.ReferenceIsNull) == true)
            {
                Add(LinearInstructionKind.ReferenceIsNull, syntax);
                Add(LinearInstructionKind.BranchFalse, syntax, fail);
                return true;
            }
            if (pattern is BoundConstantPattern character && input.SpecialType == SpecialType.System_Char &&
                character.Designator is null or BoundDiscardDesignator &&
                model.Compilation.Options.UseGraphemeChar &&
                GraphemeText(character.LiteralType?.ConstantValue ?? (character.Expression as BoundLiteralExpression)?.Value) is { } graphemeText)
            {
                var equals = ((INamedTypeSymbol)input).GetMembers("Equals").OfType<IMethodSymbol>().SingleOrDefault(m =>
                    !m.IsStatic && m.Arity == 0 && m.Parameters is [{ RefKind: RefKind.None, Type.SpecialType: SpecialType.System_Char }] &&
                    m.ReturnType.SpecialType == SpecialType.System_Boolean);
                if (equals is null || !SupportedInstanceCall(equals) || !TryType(input, false, out var characterType))
                    return Reject("grapheme equality contract unavailable", syntax);
                var characterReceiver = localTypes.Count;
                localTypes.Add(characterType);
                Add(LinearInstructionKind.StoreLocal, syntax, characterReceiver);
                instructions.Add(new(LinearInstructionKind.LocalAddress, syntax, characterReceiver, Type: input));
                if (!LowerGrapheme(graphemeText, syntax)) return false;
                Add(LinearInstructionKind.ValueInstanceCall, syntax, method: equals);
                Add(LinearInstructionKind.BranchFalse, syntax, fail);
                return true;
            }
            if (pattern is BoundDeclarationPattern { Designator: BoundDiscardDesignator } tested &&
                capabilities?.Allows(LinearInstructionKind.TypeTest) == true && input.IsReferenceType &&
                TryType(tested.DeclaredType, false, out var testedType) && capabilities.Allows(testedType))
            {
                instructions.Add(new(LinearInstructionKind.TypeTest, syntax, Type: tested.DeclaredType));
                Add(LinearInstructionKind.BranchFalse, syntax, fail);
                return true;
            }
            if (pattern is BoundDeclarationPattern declaration && CallableSignature.SameStorageType(input, declaration.DeclaredType))
                return PatternDesignator(declaration.Designator, input, syntax);
            if (pattern is BoundPropertyPattern propertyPattern && input.IsReferenceType && propertyPattern.ReceiverType.IsReferenceType &&
                TryType(input, false, out var propertyInputType) && capabilities?.Allows(propertyInputType) == true &&
                TryType(propertyPattern.ReceiverType, false, out var propertyReceiverType) && capabilities.Allows(propertyReceiverType) &&
                capabilities.Allows(LinearInstructionKind.TypeTest) && capabilities.Allows(LinearInstructionKind.ReferenceConvert))
            {
                var original = localTypes.Count; localTypes.Add(propertyInputType);
                Add(LinearInstructionKind.StoreLocal, syntax, original);
                Add(LinearInstructionKind.LoadLocal, syntax, original);
                instructions.Add(new(LinearInstructionKind.TypeTest, syntax, Type: propertyPattern.ReceiverType));
                Add(LinearInstructionKind.BranchFalse, syntax, fail);
                var receiverSlot = localTypes.Count; localTypes.Add(propertyReceiverType);
                Add(LinearInstructionKind.LoadLocal, syntax, original);
                instructions.Add(new(LinearInstructionKind.ReferenceConvert, syntax, Type: propertyPattern.ReceiverType));
                Add(LinearInstructionKind.StoreLocal, syntax, receiverSlot);
                foreach (var propertyMember in propertyPattern.Properties)
                {
                    if (propertyMember.Member is not IPropertySymbol { GetMethod: { IsStatic: false } accessor } || !SupportedInstanceCall(accessor))
                        return Reject("unsupported property pattern member", syntax);
                    Add(LinearInstructionKind.LoadLocal, syntax, receiverSlot);
                    Add(InstanceCallKind(accessor), syntax, method: accessor);
                    if (!PatternValue(propertyMember.Type, propertyMember.Pattern, fail, syntax)) return false;
                }
                if (propertyPattern.Designator is { } designator)
                {
                    Add(LinearInstructionKind.LoadLocal, syntax, receiverSlot);
                    return PatternDesignator(designator, propertyPattern.ReceiverType, syntax);
                }
                return true;
            }
            if (pattern is BoundConstantPattern scalar && scalar.Expression is { } scalarExpression &&
                (input.SpecialType is SpecialType.System_Int32 or SpecialType.System_Boolean || input.TypeKind == TypeKind.Enum) &&
                CallableSignature.SameStorageType(input, scalarExpression.Type) && scalar.Designator is null or BoundDiscardDesignator)
            {
                if (input.TypeKind == TypeKind.Enum) instructions.Add(new(LinearInstructionKind.EnumToInt32, syntax, Type: input));
                if (!LowerValue(scalarExpression)) return false;
                if (input.TypeKind == TypeKind.Enum) instructions.Add(new(LinearInstructionKind.EnumToInt32, syntax, Type: input));
                Add(LinearInstructionKind.Equal, syntax);
                Add(LinearInstructionKind.BranchFalse, syntax, fail);
                return true;
            }
            var tryGet = pattern switch
            {
                BoundCasePattern c => c.TryGetMethod,
                BoundUnionMemberPattern m => m.TryGetMethod,
                BoundDeclarationPattern d when input.TryGetUnion() is not null && input is INamedTypeSymbol union =>
                    union.GetMembers("TryGetValue").OfType<IMethodSymbol>().SingleOrDefault(m => !m.IsStatic && m.Parameters is [{ RefKind: RefKind.Out } p] &&
                        m.ReturnType.SpecialType == SpecialType.System_Boolean && CallableSignature.SameStorageType(p.GetByRefElementType(), d.DeclaredType)),
                _ => null
            };
            if (tryGet is null || !SymbolEqualityComparer.Default.Equals(input, tryGet.ContainingType) ||
                !SupportedInstanceCall(tryGet) || tryGet.Parameters is not [{ RefKind: RefKind.Out } output] ||
                !TryType(input, false, out var inputType) || !TryType(output.Type, false, out var caseType) ||
                capabilities?.Allows(inputType) != true || !capabilities.Allows(caseType))
                return Reject("unsupported pattern " + pattern.GetType().Name + " from " + input.ToDisplayString() + " to " + pattern.Type.ToDisplayString(), syntax);
            var receiver = localTypes.Count; localTypes.Add(inputType);
            Add(LinearInstructionKind.StoreLocal, syntax, receiver);
            var payload = localTypes.Count; localTypes.Add(caseType);
            Add(input.IsValueType ? LinearInstructionKind.LocalAddress : LinearInstructionKind.LoadLocal, syntax, receiver);
            Add(LinearInstructionKind.LocalAddress, syntax, payload);
            Add(InstanceCallKind(tryGet), syntax, method: tryGet);
            Add(LinearInstructionKind.BranchFalse, syntax, fail);
            if (pattern is BoundUnionMemberPattern member)
            {
                Add(LinearInstructionKind.LoadLocal, syntax, payload);
                return PatternValue(output.Type, member.Pattern, fail, syntax);
            }
            if (pattern is BoundDeclarationPattern extracted)
            {
                Add(LinearInstructionKind.LoadLocal, syntax, payload);
                return PatternDesignator(extracted.Designator, output.Type, syntax);
            }
            var casePattern = (BoundCasePattern)pattern;
            if (casePattern.Arguments.Length != casePattern.CaseSymbol.ConstructorParameters.Length)
                return Reject("case payload arity mismatch", syntax);
            for (var i = 0; i < casePattern.Arguments.Length; i++)
            {
                var name = casePattern.CaseSymbol.ConstructorParameters[i].Name;
                var propertyName = name.Length == 0 ? name : char.ToUpperInvariant(name[0]) + name[1..];
                var property = casePattern.CaseSymbol.GetMembers(propertyName).OfType<IPropertySymbol>().SingleOrDefault();
                if (property?.GetMethod is not { } getter || !SupportedInstanceCall(getter)) return Reject("unsupported case payload accessor", syntax);
                Add(output.Type.IsValueType ? LinearInstructionKind.LocalAddress : LinearInstructionKind.LoadLocal, syntax, payload);
                Add(InstanceCallKind(getter), syntax, method: getter);
                if (!PatternValue(property.Type, casePattern.Arguments[i], fail, syntax)) return false;
            }
            if (casePattern.Designator is not null)
            {
                Add(LinearInstructionKind.LoadLocal, syntax, payload);
                if (!PatternDesignator(casePattern.Designator, output.Type, syntax)) return false;
            }
            return true;
        }

        bool PatternDesignator(BoundDesignator designator, ITypeSymbol input, SyntaxNode syntax)
        {
            if (designator is BoundDiscardDesignator) { Add(LinearInstructionKind.Pop, syntax); return true; }
            if (designator is not BoundSingleVariableDesignator variable ||
                !CallableSignature.SameStorageType(input, variable.Local.Type) || !TryType(input, false, out var type) || capabilities?.Allows(type) != true)
                return Reject("unsupported pattern binding", syntax);
            if (!locals.TryGetValue(variable.Local, out var slot))
            {
                slot = localTypes.Count; localTypes.Add(type); locals.Add(variable.Local, slot);
            }
            Add(LinearInstructionKind.StoreLocal, syntax, slot);
            return true;
        }

        bool LowerStatements(BoundStatement body, bool atStatementBoundary = true)
        {
            foreach (var statement in Flatten(body))
            {
                if (statement is BoundForStatement loop && capabilities?.AllowsReferenceEnumeration == true &&
                    Lowerer.TryLowerPortableEnumeration(source, loop, out var enumerated))
                {
                    if (!LowerStatements(enumerated!, atStatementBoundary)) return false;
                    continue;
                }
                if (statement is BoundExpressionStatement { Expression: BoundBlockExpression discardedBlock })
                {
                    if (!discardedBlock.LocalsToDispose.IsEmpty) return Reject("scope disposal", Syntax(discardedBlock));
                    foreach (var child in discardedBlock.Statements)
                        if (!LowerStatements(child, atStatementBoundary)) return false;
                    continue;
                }
                if (statement is BoundExpressionStatement { Expression: BoundUnitExpression }) continue;
                if (statement is BoundThrowStatement { CompilerFailure: { } message })
                {
                    Add(LinearInstructionKind.CompilerFailure, Syntax(statement), text: message);
                    continue;
                }
                if (statement is BoundIfStatement conditionalIf)
                {
                    var otherwise = nextLabel++; var end = nextLabel++;
                    if (!LowerBranch(conditionalIf.Condition, otherwise, false, Syntax(statement))) return false;
                    if (!LowerStatements(conditionalIf.ThenNode, atStatementBoundary)) return false;
                    if (instructions.LastOrDefault().Kind is not (LinearInstructionKind.Return or LinearInstructionKind.Branch or LinearInstructionKind.CompilerFailure))
                        Add(LinearInstructionKind.Branch, Syntax(statement), end);
                    Add(LinearInstructionKind.Label, Syntax(statement), otherwise);
                    if (conditionalIf.ElseNode is { } alternative && !LowerStatements(alternative, atStatementBoundary)) return false;
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
                    if (!LowerBranch(conditional.Condition, Label(conditional.Target), conditional.JumpIfTrue, Syntax(statement))) return false;
                    continue;
                }
                if (statement is BoundLocalDeclarationStatement declaration)
                {
                    if (declaration.IsUsing) return Reject("using local", Syntax(statement));
                    foreach (var variable in declaration.Declarators)
                    {
                        if (variable.Initializer is null && capabilities?.AllowsManagedReferences != true || variable.FixedAddressInitializer is not null || variable.FixedPinnedLocal is not null)
                            return Reject("only initialized value locals", Syntax(variable));
                        EmissionType localType;
                        if (EmissionPrimitiveTypes.TryGetValueType(variable.Local.Type, out var primitive))
                        {
                            if (capabilities is not null && !capabilities.Allows(primitive)) return Reject("target does not support local type " + primitive, Syntax(variable));
                            localType = new(Primitive: primitive);
                        }
                        else if (variable.Local.Type is UnitTypeSymbol { RuntimeRepresentation: not null } &&
                            TryType(variable.Local.Type, false, out var unitType) && capabilities?.Allows(unitType) == true)
                            localType = unitType;
                        else if (variable.Local.Type is INamedTypeSymbol { TypeKind: TypeKind.Delegate } &&
                            TryType(variable.Local.Type, false, out var callableType) && capabilities?.Allows(callableType) == true)
                            localType = callableType;
                        else if (variable.Local.Type is INamedTypeSymbol externalValue && capabilities?.AllowsExternalValueSignatures == true &&
                            CallableSignature.IsExternalValue(externalValue, capabilities?.AllowsNestedExternalTypes == true) && TryType(externalValue, false, out var importedValueType) && capabilities.Allows(importedValueType))
                            localType = importedValueType;
                        else if (variable.Local.Type.GetNonNullableType() is INamedTypeSymbol external && capabilities?.AllowsExternalReferenceSignatures == true &&
                            CallableSignature.IsExternalReference(external, capabilities?.AllowsNestedExternalTypes == true) && TryType(external, false, out var externalType) && capabilities.Allows(externalType))
                            localType = externalType;
                        else if (variable.Local.Type.GetNonNullableType() is INamedTypeSymbol nominal && (!variable.Local.Type.IsNullable || nominal.IsReferenceType) && SourceTypePlan.TryCreate(nominal, out var typePlan, capabilities) && !typePlan!.IsStatic && capabilities?.AllowsRootClassLocals == true)
                            localType = new(Nominal: nominal);
                        else if (variable.Local.Type.GetNonNullableType() is INamedTypeSymbol { TypeKind: TypeKind.Interface } &&
                            TryType(variable.Local.Type, false, out var contractType) && capabilities?.Allows(contractType) == true)
                            localType = contractType;
                        else if (variable.Local.Type is (IArrayTypeSymbol or ITypeParameterSymbol or IPointerTypeSymbol) && TryType(variable.Local.Type, false, out var arrayType) && capabilities?.Allows(arrayType) == true)
                            localType = arrayType;
                        else return Reject("target does not support local type " + variable.Local.Type.Name, Syntax(variable));
                        if (variable.Initializer is not null && !LowerValue(variable.Initializer, variable.Local.Type, atStatementBoundary)) return false;
                        var slot = localTypes.Count;
                        locals.Add(variable.Local, slot);
                        localTypes.Add(localType);
                        if (variable.Initializer is not null) Add(LinearInstructionKind.StoreLocal, Syntax(variable), slot);
                    }
                    continue;
                }
                var memberAssignment = statement switch
                {
                    BoundAssignmentStatement { Expression: var expression } => expression,
                    BoundExpressionStatement { Expression: BoundAssignmentExpression expression } => expression,
                    _ => null
                };
                if (memberAssignment is BoundPatternAssignmentExpression { Pattern: BoundDiscardPattern } discard)
                {
                    if (discard.Right is BoundUnitExpression) continue;
                    // A discard consumes no earlier operand. Preserve the enclosing
                    // statement boundary so a lowered await may suspend here, while
                    // discards nested inside a value expression retain its restrictions.
                    if (!LowerValue(discard.Right, atStatementBoundary: atStatementBoundary)) return false;
                    if (discard.Right is BoundInvocationExpression discardedCall)
                    {
                        if (ReturnsValue(discardedCall.Method)) Add(LinearInstructionKind.Pop, Syntax(statement));
                    }
                    else if (TryType(discard.Right.Type, false, out _))
                        Add(LinearInstructionKind.Pop, Syntax(statement));
                    else return Reject("unsupported discarded result", Syntax(statement));
                    continue;
                }
                if (memberAssignment is BoundIndexerAssignmentExpression indexerAssignment)
                {
                    var access = indexerAssignment.Left;
                    if (access.Indexer.SetMethod is not { } setter || !LowerIndexerReceiverAndArguments(access, setter, true) || !LowerValue(indexerAssignment.Right, access.Indexer.Type))
                        return Reject("unsupported indexed property assignment", Syntax(statement));
                    Add(InstanceCallKind(setter), Syntax(statement), method: setter);
                    continue;
                }
                if (memberAssignment is BoundArrayAssignmentExpression arrayAssignment)
                {
                    if (!ArrayReceiverAndIndex(arrayAssignment.Left) || !LowerValue(arrayAssignment.Right, arrayAssignment.Left.ElementType)) return false;
                    instructions.Add(new(LinearInstructionKind.StoreElement, Syntax(statement), Type: arrayAssignment.Left.ElementType));
                    continue;
                }
                // An owned auto-property has no setter behavior to preserve. Initialize
                // its backing field directly while a value receiver is under construction;
                // calling a method here would escape the uninitialized receiver.
                if (source.MethodKind == MethodKind.Constructor && source.ContainingType?.IsValueType == true &&
                    memberAssignment is BoundPropertyAssignmentExpression
                    {
                        Property: SourcePropertySymbol { IsAutoProperty: true, BackingField: { } backingField },
                        Receiver: null or BoundSelfExpression
                    } initializer &&
                    SymbolEqualityComparer.Default.Equals(backingField.ContainingType, source.ContainingType))
                    memberAssignment = new BoundFieldAssignmentExpression(initializer.Receiver, backingField,
                        initializer.Right, model.Compilation.GetSpecialType(SpecialType.System_Unit));
                if (memberAssignment is BoundFieldAssignmentExpression fieldAssignment)
                {
                    var owner = fieldAssignment.Field.ContainingType!;
                    if (!SupportedField(fieldAssignment.Field))
                        return Reject("unsupported instance field assignment", Syntax(statement));
                    if (owner.IsReferenceType)
                    {
                        // Async dispatch can resume inside the RHS. Reload the generated
                        // self receiver afterwards instead of relying on a pre-await local.
                        // Ordinary receivers must still be evaluated before the value.
                        var reloadSelf = owner.OriginalDefinition is SynthesizedAsyncStateMachineTypeSymbol &&
                            fieldAssignment.Receiver is null or BoundSelfExpression;
                        if (!TryType(owner, false, out var receiverType) ||
                            !TryType(fieldAssignment.Field.Type, false, out var valueType))
                            return Reject("unsupported field assignment storage", Syntax(statement));
                        var receiverSlot = -1;
                        if (!reloadSelf)
                        {
                            if (!Receiver(fieldAssignment.Receiver, owner, Syntax(statement))) return false;
                            receiverSlot = localTypes.Count;
                            localTypes.Add(receiverType);
                            Add(LinearInstructionKind.StoreLocal, Syntax(statement), receiverSlot);
                        }
                        if (!LowerValue(fieldAssignment.Right, fieldAssignment.Field.Type, atStatementBoundary)) return false;
                        var valueSlot = localTypes.Count;
                        localTypes.Add(valueType);
                        Add(LinearInstructionKind.StoreLocal, Syntax(statement), valueSlot);
                        if (reloadSelf)
                        {
                            if (!Receiver(fieldAssignment.Receiver, owner, Syntax(statement))) return false;
                        }
                        else Add(LinearInstructionKind.LoadLocal, Syntax(statement), receiverSlot);
                        Add(LinearInstructionKind.LoadLocal, Syntax(statement), valueSlot);
                    }
                    else if (!Receiver(fieldAssignment.Receiver, owner, Syntax(statement)) ||
                        !LowerValue(fieldAssignment.Right, fieldAssignment.Field.Type)) return false;
                    instructions.Add(new(LinearInstructionKind.StoreField, Syntax(statement), Field: fieldAssignment.Field));
                    continue;
                }
                if (memberAssignment is BoundPropertyAssignmentExpression propertyAssignment)
                {
                    if (propertyAssignment.Property.SetMethod is not { } setter || !SupportedPropertyCall(setter) ||
                        !PropertyReceiver(propertyAssignment.Receiver, setter, Syntax(statement)) || !LowerValue(propertyAssignment.Right, propertyAssignment.Property.Type))
                        return Reject("unsupported property assignment", Syntax(statement));
                    Add(setter.IsStatic ? LinearInstructionKind.Call : InstanceCallKind(setter), Syntax(statement), method: setter);
                    continue;
                }
                var referenceAssignment = statement switch
                {
                    BoundAssignmentStatement { Expression: BoundByRefAssignmentExpression write } => write,
                    BoundExpressionStatement { Expression: BoundByRefAssignmentExpression write } => write,
                    _ => null
                };
                if (referenceAssignment is not null)
                {
                    if (!LowerReference(referenceAssignment.Reference) || !LowerValue(referenceAssignment.Right)) return false;
                    instructions.Add(new(LinearInstructionKind.StoreIndirect, Syntax(statement), Type: referenceAssignment.ElementType));
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
                    if (!LowerValue(assignment.Right, assignment.Local.Type, atStatementBoundary)) return false;
                    Add(LinearInstructionKind.StoreLocal, Syntax(assignment), slot);
                    continue;
                }
                if (statement is BoundExpressionStatement { Expression: BoundInvocationExpression call })
                {
                    if (permitsConsoleWrite(call) && call.Arguments.ToArray() is [var text])
                    {
                        if (!LowerValue(text)) return false;
                        Add(LinearInstructionKind.ConsoleWrite, Syntax(call), method: call.Method);
                        continue;
                    }
                    if (!LowerValue(call)) return false;
                    if (ReturnsValue(call.Method)) Add(LinearInstructionKind.Pop, Syntax(statement));
                    // CLI void signatures cannot express a terminal call. Preserve the
                    // call (and its dynamic message), then guard its impossible return.
                    if (BoundNodeFacts.IsTerminalRuntimeFault(call.Method))
                        Add(LinearInstructionKind.CompilerFailure, Syntax(statement),
                            text: "A terminal runtime failure unexpectedly returned.");
                    continue;
                }
                if (statement is BoundReturnStatement { Expression: null or BoundUnitExpression } && !ReturnsValue(source))
                {
                    Add(LinearInstructionKind.Return, Syntax(statement));
                    continue;
                }
                if (statement is not BoundReturnStatement { Expression: { } value })
                    return Reject("unsupported lowered statement " + statement.GetType().Name +
                        (statement is BoundExpressionStatement unsupported ? " (" + unsupported.Expression.GetType().Name +
                            (unsupported.Expression is BoundRequiredResultExpression required ? "/" + required.Operand.GetType().Name : unsupported.Expression is BoundConversionExpression converted ? "/" + converted.Expression.GetType().Name + " " + converted.Expression.Type.ToDisplayString() + " -> " + converted.Type.ToDisplayString() : "") + ") in " + source.Name : statement is BoundAssignmentStatement assigned ? " (" + assigned.Expression.GetType().Name + ")" : ""), Syntax(statement));
                if (!LowerValue(value, source.ReturnType, atStatementBoundary)) return false;
                Add(LinearInstructionKind.Return, Syntax(statement));
            }
            return true;
        }

        static IEnumerable<BoundStatement> WalkStatements(BoundStatement statement)
        {
            statement = NormalizeStatementExpression(statement);
            yield return statement;
            IEnumerable<BoundStatement> children = statement switch
            {
                BoundBlockStatement block => block.Statements,
                BoundLocalDeclarationStatement declaration => declaration.Declarators
                    .Select(variable => variable.Initializer).OfType<BoundBlockExpression>()
                    .SelectMany(initializer => initializer.Statements),
                BoundExpressionStatement { Expression: BoundBlockExpression block } => block.Statements,
                BoundIfStatement conditional => conditional.ElseNode is { } alternative
                    ? [conditional.ThenNode, alternative] : [conditional.ThenNode],
                BoundLabeledStatement labeled => [labeled.Statement],
                _ => []
            };
            foreach (var child in children)
                foreach (var nested in WalkStatements(child)) yield return nested;
        }

        bool SupportedField(IFieldSymbol field) => !field.IsStatic &&
            (field.ContainingType?.Arity is not > 0 || capabilities is null || capabilities.AllowsConstructedFieldReferences ||
                SymbolEqualityComparer.Default.Equals(field.ContainingType, source.ContainingType)) &&
            TryType(field.Type, false, out var type) && (capabilities is null || capabilities.Allows(type));
        bool SupportedTypeArguments(IMethodSymbol method) => capabilities is null ||
            method.TypeArguments.Concat(method.ContainingType?.TypeArguments ?? []).All(t => TryType(t, false, out var type) && capabilities.Allows(type));
        bool SupportedInterfaceCall(IMethodSymbol method) => !method.IsStatic && !method.IsGenericMethod && method.IsAbstract &&
            method.ContainingType is { TypeKind: TypeKind.Interface } owner &&
            ((owner.Arity == 0 || capabilities?.AllowsConstructedInterfaceInheritance == true && SupportedTypeArguments(method)) && SourceInterfacePlan.HasSupportedIdentity(owner) ||
             capabilities?.AllowsExternalInstanceCalls == true && CallableSignature.IsExternalReference(owner, capabilities?.AllowsNestedExternalTypes == true)) &&
            capabilities?.AllowsInterfaceDispatch == true && TrySignature(method, out var signature) && capabilities.Allows(signature);
        LinearInstructionKind InstanceCallKind(IMethodSymbol method) => method.ContainingType?.IsValueType == true
            ? LinearInstructionKind.ValueInstanceCall : method.ContainingType?.TypeKind == TypeKind.Interface
                ? LinearInstructionKind.InterfaceCall : LinearInstructionKind.InstanceCall;
        bool SupportedValueInstanceCall(IMethodSymbol method) => capabilities?.AllowsExternalValueInstanceCalls == true &&
            capabilities.AllowsManagedReferences && !method.IsStatic && !method.IsGenericMethod && !method.IsAbstract &&
            method.DeclaredAccessibility == Accessibility.Public &&
            method.ContainingType is { } owner && CallableSignature.IsExternalValue(owner, capabilities?.AllowsNestedExternalTypes == true) &&
            TrySignature(method, out var signature) && SupportedTypeArguments(method) && capabilities.Allows(signature);
        bool SupportedObjectDisplayCall(IMethodSymbol method) => capabilities?.AllowsObjectDisplayDispatch == true &&
            method is { Name: "ToString", IsStatic: false, IsVirtual: true, IsAbstract: false, IsGenericMethod: false, DeclaredAccessibility: Accessibility.Public } &&
            method.ContainingType?.SpecialType == SpecialType.System_Object && method.Parameters.Length == 0 &&
            method.ReturnType.GetNonNullableType().SpecialType == SpecialType.System_String;
        bool SupportedObjectHashCall(IMethodSymbol method) => capabilities?.AllowsObjectHashDispatch == true &&
            method is { Name: "GetHashCode", IsStatic: false, IsVirtual: true, IsAbstract: false, IsGenericMethod: false, DeclaredAccessibility: Accessibility.Public } &&
            method.ContainingType?.SpecialType == SpecialType.System_Object && method.Parameters.Length == 0 &&
            method.ReturnType.SpecialType == SpecialType.System_Int32;
        bool SupportedObjectEqualsCall(IMethodSymbol method) => capabilities?.Allows(EmissionDeclarationKind.ReferenceObjectOverride) == true &&
            method is { Name: "Equals", IsStatic: false, IsVirtual: true, IsAbstract: false, IsGenericMethod: false, DeclaredAccessibility: Accessibility.Public } &&
            method.ContainingType?.SpecialType == SpecialType.System_Object && method.Parameters is [{ RefKind: RefKind.None, Type: var argument }] &&
            argument.GetNonNullableType().SpecialType == SpecialType.System_Object && method.ReturnType.SpecialType == SpecialType.System_Boolean;
        bool SupportedObjectOverrideCall(IMethodSymbol method) =>
            capabilities?.Allows(method.ContainingType?.IsValueType == true ? EmissionDeclarationKind.ValueObjectOverride : EmissionDeclarationKind.ReferenceObjectOverride) == true &&
            (SourceCallablePlan.ClassifyOverride(method) != EmissionOverrideKind.None ||
             method.ContainingAssembly is IImportedAssemblySymbol { ResolvedArtifact: not null } &&
             method is { IsOverride: true, IsStatic: false, IsAbstract: false, IsGenericMethod: false, DeclaredAccessibility: Accessibility.Public } &&
             (method.Name == "ToString" && method.Parameters.IsEmpty && method.ReturnType.SpecialType == SpecialType.System_String ||
              method.Name == "GetHashCode" && method.Parameters.IsEmpty && method.ReturnType.SpecialType == SpecialType.System_Int32 ||
              method.Name == "Equals" && method.Parameters is [{ RefKind: RefKind.None, Type: var parameter }] &&
              parameter.GetNonNullableType().SpecialType == SpecialType.System_Object && method.ReturnType.SpecialType == SpecialType.System_Boolean));
        bool SupportedInstanceCall(IMethodSymbol method) => SupportedInterfaceCall(method) || SupportedValueInstanceCall(method) || !method.IsStatic && (!method.IsVirtual && !method.IsOverride || SourceCallablePlan.IsClassVirtualSlot(method, capabilities) || SupportedObjectDisplayCall(method) || SupportedObjectHashCall(method) || SupportedObjectEqualsCall(method) || SupportedObjectOverrideCall(method) || method.IsFinal && capabilities?.AllowsExternalInstanceCalls == true && method.ContainingType is { } externalOwner && CallableSignature.IsExternalReference(externalOwner, capabilities?.AllowsNestedExternalTypes == true)) &&
            method.ContainingType is { } owner && (SourceTypePlan.TryCreate(owner, out _, capabilities) ||
                capabilities?.AllowsExternalInstanceCalls == true && CallableSignature.IsExternalReference(owner, capabilities?.AllowsNestedExternalTypes == true)) &&
            TrySignature(method, out var signature) && SupportedTypeArguments(method) && (capabilities is null || capabilities.Allows(signature));
        bool SupportedPropertyCall(IMethodSymbol method) =>
            (capabilities is null || capabilities.Allows(EmissionDeclarationKind.PropertyAccessor)) &&
            (method.IsStatic
                ? method.ContainingType is { } owner && (SourceTypePlan.TryCreate(owner, out _, capabilities) ||
                  capabilities?.AllowsExternalReferenceSignatures == true &&
                  CallableSignature.IsExternalReference(owner, capabilities.AllowsNestedExternalTypes) ||
                  capabilities?.AllowsExternalValueSignatures == true &&
                  CallableSignature.IsExternalValue(owner, capabilities.AllowsNestedExternalTypes)) &&
                  TrySignature(method, out var signature) && SupportedTypeArguments(method) &&
                  (capabilities is null || capabilities.Allows(signature))
                : SupportedInstanceCall(method));
        bool PropertyReceiver(BoundExpression? receiver, IMethodSymbol accessor, SyntaxNode syntax) =>
            accessor.IsStatic
                ? receiver is null or BoundTypeExpression || Reject("static property receiver must be a type", syntax)
                : Receiver(receiver, accessor.ContainingType!, syntax);

        bool TemporaryReceiver(BoundExpression receiver, SyntaxNode syntax)
        {
            // Value-returning getters and calls produce copies. Evaluate once before
            // the call arguments and give only that copy an address; never write it back.
            if (!TryType(receiver.Type, false, out var type) || type.IsByReference ||
                capabilities?.Allows(type) != true || !capabilities.Allows(LinearInstructionKind.LocalAddress))
                return Reject("target does not support temporary value receiver storage", syntax);
            if (!LowerValue(receiver)) return false;
            var slot = localTypes.Count;
            localTypes.Add(type);
            Add(LinearInstructionKind.StoreLocal, syntax, slot);
            Add(LinearInstructionKind.LocalAddress, syntax, slot);
            return true;
        }

        bool Receiver(BoundExpression? receiver, INamedTypeSymbol owner, SyntaxNode syntax)
        {
            if (owner.IsValueType)
            {
                if (capabilities?.AllowsManagedReferences != true ||
                    !(capabilities.Allows(EmissionDeclarationKind.ValueType) && SourceTypePlan.TryCreate(owner, out _, capabilities) || capabilities.AllowsExternalValueInstanceCalls && CallableSignature.IsExternalValue(owner, capabilities.AllowsNestedExternalTypes)))
                    return Reject("target does not support value receivers", syntax);
                if (!isStaticBody && SymbolEqualityComparer.Default.Equals(source.ContainingType, owner) &&
                    receiver is null or BoundSelfExpression)
                {
                    Add(LinearInstructionKind.Receiver, syntax); return true;
                }
                return receiver switch
                {
                    BoundParenthesizedExpression parenthesized => Receiver(parenthesized.Expression, owner, syntax),
                    BoundLocalAccess local => LowerReference(new BoundAddressOfExpression(local)),
                    BoundParameterAccess parameter => LowerReference(parameter),
                    BoundDereferenceExpression dereference => LowerReference(dereference.Reference),
                    BoundFieldAccess field => FieldAddress(field.Receiver, field.Field, syntax),
                    BoundMemberAccessExpression { Member: IFieldSymbol field } access => FieldAddress(access.Receiver, field, syntax),
                    BoundIndexerAccessExpression or BoundPropertyAccess or BoundInvocationExpression or BoundObjectCreationExpression or BoundConversionExpression or BoundDefaultValueExpression => TemporaryReceiver(receiver, syntax),
                    BoundMemberAccessExpression { Member: IPropertySymbol } => TemporaryReceiver(receiver, syntax),
                    _ => Reject("value receiver requires addressable storage or a supported value result", syntax)
                };
            }
            if (receiver is not null)
            {
                if (!LowerValue(receiver)) return false;
                if (receiver.Type.IsValueType && owner.SpecialType == SpecialType.System_Object)
                {
                    if (capabilities?.Allows(LinearInstructionKind.BoxToObject) != true) return Reject("target does not support boxed Object receivers", syntax);
                    instructions.Add(new(LinearInstructionKind.BoxToObject, syntax, Type: receiver.Type));
                }
                if (receiver.Type.SpecialType == SpecialType.System_String && owner.SpecialType == SpecialType.System_Object &&
                    capabilities?.Allows(LinearInstructionKind.ReferenceConvert) == true)
                    instructions.Add(new(LinearInstructionKind.ReferenceConvert, syntax, Type: owner));
                // Projected members use the configured nominal backing or one of
                // its interfaces; the bound receiver can still be a vector.
                if (receiver.Type is IArrayTypeSymbol array &&
                    capabilities?.Allows(LinearInstructionKind.ReferenceConvert) == true &&
                    (owner.TypeKind == TypeKind.Interface &&
                     model.Compilation.ClassifyConversion(receiver.Type, owner, includeUserDefined: false) is { IsImplicit: true, IsReference: true } ||
                     model.Compilation.IsRuntimeArrayShape(owner) && owner.TypeArguments.Length == 1 &&
                     SymbolEqualityComparer.Default.Equals(array.ElementType, owner.TypeArguments[0])))
                    instructions.Add(new(LinearInstructionKind.ReferenceConvert, syntax, Type: owner));
                return true;
            }
            var selfCapture = Array.FindIndex(captures, capture => capture is INamedTypeSymbol type && SymbolEqualityComparer.Default.Equals(type, owner));
            if (selfCapture >= 0)
            {
                Add(LinearInstructionKind.LoadCapture, syntax, selfCapture);
                return true;
            }
            if (isStaticBody || !SymbolEqualityComparer.Default.Equals(source.ContainingType, owner)) return Reject("implicit receiver unavailable", syntax);
            Add(LinearInstructionKind.Receiver, syntax); return true;
        }

        bool LowerIndexerReceiverAndArguments(BoundIndexerAccessExpression access, IMethodSymbol accessor, bool setter)
        {
            var arguments = access.Arguments.ToArray();
            if (!SupportedInstanceCall(accessor) || capabilities is not null && !capabilities.Allows(EmissionDeclarationKind.IndexerAccessor) ||
                arguments.Length != accessor.Parameters.Length - (setter ? 1 : 0))
                return Reject("unsupported indexed property accessor or arguments", Syntax(access));
            if (!LowerValue(access.Receiver)) return false;
            foreach (var argument in arguments) if (!LowerValue(argument)) return false;
            return true;
        }
        bool SupportedArray(ITypeSymbol type) => type is IArrayTypeSymbol && TryType(type, false, out var array) &&
            (capabilities is null || capabilities.Allows(array));
        bool ArrayReceiverAndIndex(BoundArrayAccessExpression access)
        {
            var indices = access.Indices.ToArray();
            if (!SupportedArray(access.Receiver.Type) || indices.Length != 1 || indices[0].Type.SpecialType != SpecialType.System_Int32)
                return Reject("only supported vectors with an Int32 index", Syntax(access));
            return LowerValue(access.Receiver) && LowerValue(indices[0]);
        }
        bool ArrayLiteral(IArrayTypeSymbol type, IEnumerable<BoundExpression> values, SyntaxNode syntax)
        {
            var elements = values.ToArray();
            if (elements.Any(e => e is BoundSpreadElement or BoundCollectionComprehensionExpression))
                return Reject("array spreads/comprehensions", syntax);
            Add(LinearInstructionKind.Constant, syntax, elements.Length);
            instructions.Add(new(LinearInstructionKind.NewArray, syntax, Type: type.ElementType));
            for (int i = 0; i < elements.Length; i++)
            {
                Add(LinearInstructionKind.Duplicate, syntax);
                Add(LinearInstructionKind.Constant, syntax, i);
                if (!LowerValue(elements[i], type.ElementType)) return false;
                instructions.Add(new(LinearInstructionKind.StoreElement, syntax, Type: type.ElementType));
            }
            return true;
        }
        bool FieldAddress(BoundExpression? receiver, IFieldSymbol field, SyntaxNode syntax)
        {
            if (capabilities?.Allows(LinearInstructionKind.FieldAddress) != true || field.IsReadOnly ||
                field.ContainingType?.OriginalDefinition is not SourceNamedTypeSymbol || !SupportedField(field))
                return Reject("unsupported mutable source field address", syntax);
            if (!Receiver(receiver, field.ContainingType, syntax)) return false;
            instructions.Add(new(LinearInstructionKind.FieldAddress, syntax, Field: field));
            return true;
        }

        static bool IsNullLiteral(BoundExpression expression) => expression is BoundLiteralExpression { Kind: BoundLiteralExpressionKind.NullLiteral } ||
            expression is BoundConversionExpression { IsUserDefined: false } conversion && IsNullLiteral(conversion.Expression);

        bool LowerReference(BoundExpression expression)
        {
            if (capabilities?.AllowsManagedReferences != true) return Reject("target does not support managed references", Syntax(expression));
            var symbol = expression is BoundAddressOfExpression address ? address.Symbol : expression is BoundParameterAccess parameter ? parameter.Parameter : null;
            if (symbol is IFieldSymbol field && expression is BoundAddressOfExpression fieldAddress)
                return FieldAddress(fieldAddress.Storage is BoundFieldAccess access ? access.Receiver : fieldAddress.Receiver, field, Syntax(expression));
            if (symbol is ILocalSymbol local)
            {
                if (!locals.TryGetValue(local, out var slot))
                {
                    // Inline out declarations have a local symbol but no declaration statement.
                    if (!SymbolEqualityComparer.Default.Equals(local.ContainingSymbol, source) ||
                        !TryType(local.Type, false, out var localType) || !capabilities.Allows(localType))
                        return Reject("unsupported or captured addressed local", Syntax(expression));
                    slot = localTypes.Count;
                    locals.Add(local, slot);
                    localTypes.Add(localType);
                }
                Add(LinearInstructionKind.LocalAddress, Syntax(expression), slot); return true;
            }
            if (symbol is IParameterSymbol value && value.RefKind == RefKind.None && capabilities.Allows(LinearInstructionKind.ArgumentAddress))
            {
                int ordinal = source.Parameters.IndexOf(value, 0, source.Parameters.Length, SymbolEqualityComparer.Default);
                if (ordinal < 0) return Reject("captured value parameter address", Syntax(expression));
                Add(LinearInstructionKind.ArgumentAddress, Syntax(expression), ordinal + (isStaticBody ? 0 : 1)); return true;
            }
            if (symbol is IParameterSymbol reference && reference.RefKind is RefKind.Ref or RefKind.Out)
            {
                int ordinal = source.Parameters.IndexOf(reference, 0, source.Parameters.Length, SymbolEqualityComparer.Default);
                if (ordinal < 0) return Reject("captured reference parameter", Syntax(expression));
                Add(LinearInstructionKind.Argument, Syntax(expression), ordinal + (isStaticBody ? 0 : 1)); return true;
            }
            return Reject("only owned locals or ref/out parameter addresses: " + expression.GetType().Name + " (" + symbol?.ToDisplayString() + ")", Syntax(expression));
        }

        bool LowerTypedNull(ITypeSymbol type, SyntaxNode syntax)
        {
            if (!type.IsReferenceType || !TryType(type, false, out var target) || capabilities?.Allows(target) == false ||
                capabilities?.Allows(LinearInstructionKind.DefaultValue) == false)
                return Reject("unsupported null target " + type.ToDisplayString(), syntax);
            instructions.Add(new(LinearInstructionKind.DefaultValue, syntax, Type: type));
            return true;
        }

        static bool IsUnsigned(SpecialType type) => type is SpecialType.System_UInt32 or SpecialType.System_UInt64;
        static bool IsNumeric(SpecialType type) => type is SpecialType.System_SByte or SpecialType.System_Byte or
            SpecialType.System_Int16 or SpecialType.System_UInt16 or SpecialType.System_Int32 or SpecialType.System_UInt32 or
            SpecialType.System_Int64 or SpecialType.System_UInt64 or SpecialType.System_Single or SpecialType.System_Double;

        void ConvertNumeric(SpecialType source, SpecialType target, SyntaxNode syntax)
        {
            if (IsUnsigned(source) && target is SpecialType.System_Single or SpecialType.System_Double)
            {
                Add(LinearInstructionKind.UnsignedConvertDouble, syntax);
                if (target == SpecialType.System_Single) Add(LinearInstructionKind.ConvertSingle, syntax);
                return;
            }
            Add(target switch
            {
                SpecialType.System_SByte => LinearInstructionKind.ConvertSByte,
                SpecialType.System_Byte => LinearInstructionKind.ConvertByte,
                SpecialType.System_Int16 => LinearInstructionKind.ConvertInt16,
                SpecialType.System_UInt16 => LinearInstructionKind.ConvertUInt16,
                SpecialType.System_UInt32 => LinearInstructionKind.ConvertUInt32,
                SpecialType.System_UInt64 when source is SpecialType.System_SByte or SpecialType.System_Int16 or SpecialType.System_Int32 => LinearInstructionKind.Convert64,
                SpecialType.System_UInt64 => LinearInstructionKind.ConvertUInt64,
                SpecialType.System_Int64 when source == SpecialType.System_UInt32 => LinearInstructionKind.ConvertUInt64,
                SpecialType.System_Int64 => LinearInstructionKind.Convert64,
                SpecialType.System_Single => LinearInstructionKind.ConvertSingle,
                SpecialType.System_Double => LinearInstructionKind.ConvertDouble,
                _ => LinearInstructionKind.Convert32
            }, syntax);
        }

        bool LowerBinaryOperand(BoundExpression operand, ITypeSymbol expectedType)
        {
            if (!LowerValue(operand)) return false;
            // Built-in operators retain their promoted operand types even when the
            // bound operand has no explicit conversion node.
            if (expectedType.SpecialType != operand.Type.SpecialType && IsNumeric(expectedType.SpecialType) && IsNumeric(operand.Type.SpecialType))
                ConvertNumeric(operand.Type.SpecialType, expectedType.SpecialType, Syntax(operand));
            return true;
        }

        bool LowerValue(BoundExpression expression, ITypeSymbol? nullTarget = null, bool atStatementBoundary = false)
        {
            if (nullTarget is not null && IsNullLiteral(expression)) return LowerTypedNull(nullTarget, Syntax(expression));
            if (capabilities is not null && EmissionPrimitiveTypes.TryGetValueType(expression.Type, out var valueType) && !capabilities.Allows(valueType))
                return Reject("target does not support value type " + valueType, Syntax(expression));
            switch (expression)
            {
                case BoundNullCoalesceExpression coalesce when coalesce.Left.Type.IsReferenceType &&
                    capabilities?.Allows(LinearInstructionKind.ReferenceIsNull) == true &&
                    TryType(coalesce.Type, false, out var coalesceType) && capabilities.Allows(coalesceType):
                    // Keep the original operand on the non-null path; evaluate it once.
                    // A return fallback is admitted only with no enclosing operands on the stack.
                    if (coalesce.Right is BoundReturnExpression && !atStatementBoundary)
                        return Reject("coalescing return requires a statement boundary", Syntax(expression));
                    var coalesceJoin = nextLabel++;
                    if (!LowerValue(coalesce.Left)) return false;
                    Add(LinearInstructionKind.Duplicate, Syntax(expression));
                    Add(LinearInstructionKind.ReferenceIsNull, Syntax(expression));
                    Add(LinearInstructionKind.BranchFalse, Syntax(expression), coalesceJoin);
                    Add(LinearInstructionKind.Pop, Syntax(expression));
                    if (coalesce.Right is BoundReturnExpression fallbackReturn)
                    {
                        if (!LowerStatements(new BoundReturnStatement(fallbackReturn.Expression))) return false;
                    }
                    else if (!LowerValue(coalesce.Right, coalesce.Type)) return false;
                    Add(LinearInstructionKind.Label, Syntax(expression), coalesceJoin);
                    return true;
                case BoundUnitExpression { Type: UnitTypeSymbol { RuntimeRepresentation: { } representation } } when TryType(representation, false, out var unitType) &&
                    unitType.Nominal is not null && capabilities?.Allows(unitType) == true &&
                    capabilities.Allows(LinearInstructionKind.DefaultValue):
                    instructions.Add(new(LinearInstructionKind.DefaultValue, Syntax(expression), Type: representation)); return true;
                case BoundDefaultValueExpression value when TryType(value.Type, false, out var defaultType) &&
                    (capabilities is null || capabilities.Allows(defaultType)):
                    instructions.Add(new(LinearInstructionKind.DefaultValue, Syntax(expression), Type: value.Type)); return true;
                case BoundIndexerAccessExpression indexer when indexer.Indexer.GetMethod is { } indexGetter:
                    if (!LowerIndexerReceiverAndArguments(indexer, indexGetter, false)) return false;
                    Add(InstanceCallKind(indexGetter), Syntax(expression), method: indexGetter); return true;
                case BoundCollectionExpression collection when SupportedArray(collection.Type):
                    return ArrayLiteral((IArrayTypeSymbol)collection.Type, collection.Elements, Syntax(expression));
                case BoundEmptyCollectionExpression empty when SupportedArray(empty.Type):
                    return ArrayLiteral((IArrayTypeSymbol)empty.Type, [], Syntax(expression));
                case BoundArrayAccessExpression access:
                    if (!ArrayReceiverAndIndex(access)) return false;
                    instructions.Add(new(LinearInstructionKind.LoadElement, Syntax(expression), Type: access.ElementType)); return true;
                case BoundMemberAccessExpression { Member: IPropertySymbol { Name: "Length", IsStatic: false, Type.SpecialType: SpecialType.System_Int32, GetMethod.Parameters.Length: 0 } lengthProperty } length
                    when SupportedArray(length.Receiver.Type) && (lengthProperty.ContainingType?.SpecialType == SpecialType.System_Array ||
                        lengthProperty.ContainingType is { } arrayShape && model.Compilation.IsRuntimeArrayShape(arrayShape)):
                    if (!LowerValue(length.Receiver)) return false;
                    Add(LinearInstructionKind.ArrayLength, Syntax(expression)); return true;
                case BoundBlockExpression block:
                    if (!block.LocalsToDispose.IsEmpty) return Reject("value block scope disposal", Syntax(block));
                    var statements = block.Statements.ToImmutableArray();
                    if (statements.IsEmpty || statements[^1] is not BoundExpressionStatement { Expression: var result } ||
                        !TryType(result.Type, false, out var blockType) ||
                        capabilities is not null && !capabilities.Allows(blockType))
                        return Reject("value block requires a supported trailing value expression", Syntax(block));
                    // A value block may be evaluated with earlier operands still on the
                    // stack. Only an empty-stack statement boundary admits a method return
                    // or an async-lowered branch to the enclosing method's completion label.
                    var controlFlow = statements.Take(statements.Length - 1).SelectMany(WalkStatements).ToArray();
                    var localLabels = controlFlow.OfType<BoundLabeledStatement>()
                        .Select(label => label.Label).ToHashSet<ILabelSymbol>(SymbolEqualityComparer.Default);
                    foreach (var statement in controlFlow)
                    {
                        if (!atStatementBoundary && (statement is (BoundReturnStatement or BoundExpressionStatement { Expression: BoundReturnExpression }) ||
                            statement is BoundGotoStatement jump && !localLabels.Contains(jump.Target) ||
                            statement is BoundConditionalGotoStatement branch && !localLabels.Contains(branch.Target)))
                            return Reject("value block cannot exit its enclosing expression", Syntax(statement));
                    }
                    foreach (var prefix in statements.AsSpan()[..^1])
                        if (!LowerStatements(prefix, atStatementBoundary)) return false;
                    return LowerValue(result, atStatementBoundary: atStatementBoundary);
                case BoundIfExpression conditional when conditional.ElseBranch is not null &&
                    conditional.Condition.Type.SpecialType == SpecialType.System_Boolean &&
                    TryType(conditional.Type, false, out var conditionalType) &&
                    (capabilities is null || capabilities.Allows(conditionalType)) &&
                    SymbolEqualityComparer.Default.Equals(conditional.ThenBranch.Type, conditional.Type) &&
                    SymbolEqualityComparer.Default.Equals(conditional.ElseBranch.Type, conditional.Type):
                    var alternative = nextLabel++;
                    var joined = nextLabel++;
                    if (!LowerBranch(conditional.Condition, alternative, false, Syntax(expression))) return false;
                    if (!LowerValue(conditional.ThenBranch, conditional.Type, atStatementBoundary)) return false;
                    Add(LinearInstructionKind.Branch, Syntax(expression), joined);
                    Add(LinearInstructionKind.Label, Syntax(expression), alternative);
                    if (!LowerValue(conditional.ElseBranch, conditional.Type, atStatementBoundary)) return false;
                    Add(LinearInstructionKind.Label, Syntax(expression), joined);
                    return true;
                case BoundObjectCreationExpression creation when creation.Initializer is null && creation.Receiver is null &&
                    (SourceTypePlan.TryCreate(creation.Constructor.ContainingType!, out var createdType, capabilities) && !createdType!.IsStatic ||
                     capabilities?.AllowsExternalConstructors == true && creation.Constructor.DeclaredAccessibility == Accessibility.Public &&
                     creation.Constructor.ContainingType is { } externalOwner && (CallableSignature.IsExternalValue(externalOwner, capabilities?.AllowsNestedExternalTypes == true) || CallableSignature.IsExternalReference(externalOwner, capabilities?.AllowsNestedExternalTypes == true))) &&
                    TrySignature(creation.Constructor, out var constructorSignature) && SupportedTypeArguments(creation.Constructor) &&
                    (capabilities is null || capabilities.Allows(constructorSignature)):
                    var constructorArguments = creation.Arguments.ToArray();
                    if (constructorArguments.Length != creation.Constructor.Parameters.Length) return Reject("optional/expanded constructor arguments", Syntax(expression));
                    for (var i = 0; i < constructorArguments.Length; i++)
                        if (!LowerValue(constructorArguments[i], creation.Constructor.Parameters[i].Type)) return false;
                    Add(LinearInstructionKind.NewObject, Syntax(expression), method: creation.Constructor); return true;
                case BoundObjectCreationExpression unsupportedCreation:
                    var nestedParameter = unsupportedCreation.Constructor.Parameters.Select(p => p.Type).OfType<INamedTypeSymbol>().FirstOrDefault(t => t.ContainingType is not null);
                    return Reject("constructor " + unsupportedCreation.Constructor.ContainingType?.ToDisplayString() + "." + unsupportedCreation.Constructor.ToDisplayString() +
                        (nestedParameter is null ? "" : " with nested parameter type " + nestedParameter.ContainingType!.ToDisplayString() + "." + nestedParameter.MetadataName), Syntax(expression));
                case BoundSelfExpression capturedSelf when Array.FindIndex(captures, capture => capture is INamedTypeSymbol type && SymbolEqualityComparer.Default.Equals(type, capturedSelf.Type)) is var selfIndex && selfIndex >= 0:
                    Add(LinearInstructionKind.LoadCapture, Syntax(expression), selfIndex);
                    return true;
                case BoundBaseExpression baseReceiver when !isStaticBody && capabilities?.AllowsClassVirtualSlots == true &&
                    SymbolEqualityComparer.Default.Equals(baseReceiver.Type, source.ContainingType?.BaseType):
                    Add(LinearInstructionKind.Receiver, Syntax(expression)); return true;
                case BoundSelfExpression self when !isStaticBody && SymbolEqualityComparer.Default.Equals(self.Type, source.ContainingType):
                    Add(LinearInstructionKind.Receiver, Syntax(expression));
                    if (self.Type.IsValueType) instructions.Add(new(LinearInstructionKind.LoadIndirect, Syntax(expression), Type: self.Type));
                    return true;
                case BoundFieldAccess { Field: { IsConst: true, Type.TypeKind: TypeKind.Enum } enumField } when
                    capabilities?.Allows(EmissionDeclarationKind.Enum) == true && enumField.GetConstantValue() is int enumValue:
                    Add(LinearInstructionKind.Constant, Syntax(expression), enumValue);
                    instructions.Add(new(LinearInstructionKind.EnumFromInt32, Syntax(expression), Type: enumField.Type)); return true;
                case BoundMemberAccessExpression { Member: IFieldSymbol { IsConst: true, Type.TypeKind: TypeKind.Enum } enumMember } when
                    capabilities?.Allows(EmissionDeclarationKind.Enum) == true && enumMember.GetConstantValue() is int enumMemberValue:
                    Add(LinearInstructionKind.Constant, Syntax(expression), enumMemberValue);
                    instructions.Add(new(LinearInstructionKind.EnumFromInt32, Syntax(expression), Type: enumMember.Type)); return true;
                case BoundFieldAccess { Field: { IsConst: true, Type.SpecialType: SpecialType.System_Double } constantField } when constantField.GetConstantValue() is double constantValue:
                    instructions.Add(new(LinearInstructionKind.ConstantDouble, Syntax(expression), Long: BitConverter.DoubleToInt64Bits(constantValue))); return true;
                case BoundMemberAccessExpression { Member: IFieldSymbol { IsConst: true, Type.SpecialType: SpecialType.System_Double } constantMember } when constantMember.GetConstantValue() is double memberValue:
                    instructions.Add(new(LinearInstructionKind.ConstantDouble, Syntax(expression), Long: BitConverter.DoubleToInt64Bits(memberValue))); return true;
                case BoundFieldAccess field when SupportedField(field.Field):
                    if (!Receiver(field.Receiver, field.Field.ContainingType!, Syntax(expression))) return false;
                    instructions.Add(new(LinearInstructionKind.LoadField, Syntax(expression), Field: field.Field)); return true;
                case BoundMemberAccessExpression { Member: IFieldSymbol memberField } fieldAccess when SupportedField(memberField):
                    if (!Receiver(fieldAccess.Receiver, memberField.ContainingType!, Syntax(expression))) return false;
                    instructions.Add(new(LinearInstructionKind.LoadField, Syntax(expression), Field: memberField)); return true;
                case BoundPropertyAccess property when property.Property.GetMethod is { } getter && SupportedPropertyCall(getter):
                    if (!PropertyReceiver(null, getter, Syntax(expression))) return false;
                    Add(getter.IsStatic ? LinearInstructionKind.Call : InstanceCallKind(getter), Syntax(expression), method: getter); return true;
                case BoundMemberAccessExpression { Member: IPropertySymbol memberProperty } access when memberProperty.GetMethod is { } memberGetter && SupportedPropertyCall(memberGetter):
                    if (!PropertyReceiver(access.Receiver, memberGetter, Syntax(expression))) return false;
                    Add(memberGetter.IsStatic ? LinearInstructionKind.Call : InstanceCallKind(memberGetter), Syntax(expression), method: memberGetter); return true;
                case BoundIsPatternExpression { Pattern: BoundDeclarationPattern { Designator: BoundDiscardDesignator } tested } typeTest when
                    capabilities?.Allows(LinearInstructionKind.TypeTest) == true && typeTest.Expression.Type.IsReferenceType &&
                    TryType(tested.DeclaredType, false, out var testedType) && capabilities.Allows(testedType):
                    if (!LowerValue(typeTest.Expression)) return false;
                    instructions.Add(new(LinearInstructionKind.TypeTest, Syntax(expression), Type: tested.DeclaredType));
                    return true;
                case BoundIsPatternExpression caseTest when capabilities?.AllowsCasePatterns == true:
                    var failedPattern = nextLabel++;
                    var completedPattern = nextLabel++;
                    if (!LowerPattern(caseTest.Expression, caseTest.Pattern, failedPattern, Syntax(expression))) return false;
                    Add(LinearInstructionKind.Boolean, Syntax(expression), 1);
                    Add(LinearInstructionKind.Branch, Syntax(expression), completedPattern);
                    Add(LinearInstructionKind.Label, Syntax(expression), failedPattern);
                    Add(LinearInstructionKind.Boolean, Syntax(expression), 0);
                    Add(LinearInstructionKind.Label, Syntax(expression), completedPattern);
                    return true;
                case BoundTypeOfExpression typeOf when typeOf.SystemType.SpecialType != SpecialType.System_Int32 &&
                    capabilities?.Allows(LinearInstructionKind.LoadTypeToken) == true &&
                    model.Compilation.ResolveRuntimeTypeOfContract() is { } typeOfBinding:
                    if (typeOfBinding.Resolver.IsVirtual || typeOfBinding.Resolver.IsOverride ||
                        typeOf.OperandType is INamedTypeSymbol { IsUnboundGenericType: true } ||
                        !TryType(typeOf.OperandType, false, out var tokenType) || !capabilities.Allows(tokenType) ||
                        !TrySignature(typeOfBinding.CurrentGetter, out var currentSignature) || !capabilities.Allows(currentSignature) ||
                        !TrySignature(typeOfBinding.Resolver, out var resolverSignature) || !capabilities.Allows(resolverSignature))
                        return Reject("unsupported runtime typeof contract or operand", Syntax(expression));
                    Add(LinearInstructionKind.Call, Syntax(expression), method: typeOfBinding.CurrentGetter);
                    instructions.Add(new(LinearInstructionKind.LoadTypeToken, Syntax(expression), Type: typeOf.OperandType));
                    Add(InstanceCallKind(typeOfBinding.Resolver), Syntax(expression), method: typeOfBinding.Resolver);
                    return true;
                case BoundLiteralExpression { Value: System.Text.Rune scalar } when model.Compilation.Options.UseGraphemeChar:
                    return LowerGrapheme(scalar.ToString(), Syntax(expression));
                case BoundLiteralExpression { Value: char character } when model.Compilation.Options.UseGraphemeChar:
                    return LowerGrapheme(character.ToString(), Syntax(expression));
                case BoundLiteralExpression { Value: GraphemeLiteralValue grapheme }:
                    return LowerGrapheme(grapheme.Text, Syntax(expression));
                case BoundLiteralExpression { Value: string text }:
                    Add(LinearInstructionKind.String, Syntax(expression), text: text); return true;
                case BoundLiteralExpression { Value: bool boolean }:
                    Add(LinearInstructionKind.Boolean, Syntax(expression), boolean ? 1 : 0); return true;
                case BoundLiteralExpression { Value: float single }:
                    instructions.Add(new(LinearInstructionKind.ConstantSingle, Syntax(expression), Integer: BitConverter.SingleToInt32Bits(single))); return true;
                case BoundLiteralExpression { Value: double floating }:
                    instructions.Add(new(LinearInstructionKind.ConstantDouble, Syntax(expression), Long: BitConverter.DoubleToInt64Bits(floating))); return true;
                case BoundLiteralExpression { Value: long value64 }:
                    instructions.Add(new(LinearInstructionKind.Constant64, Syntax(expression), Long: value64)); return true;
                case BoundLiteralExpression { Value: ulong unsigned64 }:
                    instructions.Add(new(LinearInstructionKind.Constant64, Syntax(expression), Long: unchecked((long)unsigned64))); return true;
                case BoundLiteralExpression { Value: uint unsigned32 }:
                    Add(LinearInstructionKind.Constant, Syntax(expression), unchecked((int)unsigned32)); return true;
                case BoundLiteralExpression { Value: sbyte signed8 }:
                    Add(LinearInstructionKind.Constant, Syntax(expression), signed8); return true;
                case BoundLiteralExpression { Value: short signed16 }:
                    Add(LinearInstructionKind.Constant, Syntax(expression), signed16); return true;
                case BoundLiteralExpression { Value: ushort unsigned16 }:
                    Add(LinearInstructionKind.Constant, Syntax(expression), unsigned16); return true;
                case BoundLiteralExpression { Value: byte valueByte }:
                    Add(LinearInstructionKind.Constant, Syntax(expression), valueByte); return true;
                case BoundLiteralExpression { Value: int enumLiteral, Type.TypeKind: TypeKind.Enum } when capabilities?.Allows(EmissionDeclarationKind.Enum) == true:
                    Add(LinearInstructionKind.Constant, Syntax(expression), enumLiteral);
                    instructions.Add(new(LinearInstructionKind.EnumFromInt32, Syntax(expression), Type: expression.Type)); return true;
                case BoundLiteralExpression { Value: int value }:
                    Add(LinearInstructionKind.Constant, Syntax(expression), value); return true;
                case BoundLocalAccess local:
                    var captureIndex = Array.FindIndex(captures, capture => SymbolEqualityComparer.Default.Equals(capture, local.Local));
                    if (captureIndex >= 0)
                    {
                        Add(LinearInstructionKind.LoadCapture, Syntax(expression), captureIndex);
                        return true;
                    }
                    if (!locals.TryGetValue(local.Local, out var slot)) return Reject("undeclared local", Syntax(expression));
                    Add(LinearInstructionKind.LoadLocal, Syntax(expression), slot); return true;
                case BoundAddressOfExpression address:
                    return LowerReference(address);
                case BoundDereferenceExpression dereference:
                    if (!LowerReference(dereference.Reference)) return false;
                    instructions.Add(new(LinearInstructionKind.LoadIndirect, Syntax(expression), Type: dereference.ElementType));
                    return true;
                case BoundParameterAccess parameter:
                    var parameterCaptureIndex = Array.FindIndex(captures, capture => SymbolEqualityComparer.Default.Equals(capture, parameter.Parameter));
                    if (parameterCaptureIndex >= 0)
                    {
                        Add(LinearInstructionKind.LoadCapture, Syntax(expression), parameterCaptureIndex);
                        return true;
                    }
                    var index = source.Parameters.IndexOf(parameter.Parameter, 0, source.Parameters.Length, SymbolEqualityComparer.Default);
                    if (index < 0) return Reject("captured parameter", Syntax(expression));
                    Add(LinearInstructionKind.Argument, Syntax(expression), index + (isStaticBody ? 0 : 1));
                    if (parameter.Parameter.RefKind is RefKind.Ref or RefKind.Out)
                        instructions.Add(new(LinearInstructionKind.LoadIndirect, Syntax(expression), Type: parameter.Parameter.Type));
                    return true;
                case BoundUnaryExpression { Operator.OperatorKind: BoundUnaryOperatorKind.LogicalNot } unary:
                    if (!LowerValue(unary.Operand)) return false;
                    Add(LinearInstructionKind.Not, Syntax(expression)); return true;
                case BoundUnaryExpression unary when unary.Operator.OperandType.SpecialType is SpecialType.System_Int32 or SpecialType.System_Int64 or SpecialType.System_Single or SpecialType.System_Double &&
                    unary.Operator.OperatorKind is BoundUnaryOperatorKind.UnaryPlus or BoundUnaryOperatorKind.UnaryMinus or BoundUnaryOperatorKind.BitwiseNot:
                    if (!LowerValue(unary.Operand)) return false;
                    if (unary.Operator.OperatorKind != BoundUnaryOperatorKind.UnaryPlus)
                        Add(unary.Operator.OperatorKind == BoundUnaryOperatorKind.UnaryMinus ? LinearInstructionKind.Negate : LinearInstructionKind.Complement, Syntax(expression));
                    return true;
                case BoundRequiredResultExpression required:
                    return LowerValue(required.Operand, atStatementBoundary: atStatementBoundary);
                case BoundParenthesizedExpression parenthesized:
                    return LowerValue(parenthesized.Expression, atStatementBoundary: atStatementBoundary);
                case BoundConversionExpression { IsUserDefined: true, MethodSymbol: { IsStatic: true, Parameters: [{ RefKind: RefKind.None } parameter] } conversionMethod } conversion when
                    CallableSignature.SameStorageType(parameter.Type, conversion.Expression.Type) &&
                    CallableSignature.SameStorageType(conversionMethod.ReturnType, conversion.Type) &&
                    TrySignature(conversionMethod, out var conversionSignature) && SupportedTypeArguments(conversionMethod) &&
                    (capabilities is null || capabilities.Allows(conversionSignature)):
                    if (!LowerValue(conversion.Expression, atStatementBoundary: atStatementBoundary)) return false;
                    Add(LinearInstructionKind.Call, Syntax(expression), method: conversionMethod);
                    return true;
                case BoundConversionExpression conversion when !conversion.IsUserDefined && conversion.Conversion.Exists && IsNullLiteral(conversion.Expression):
                    return LowerTypedNull(conversion.Type, Syntax(expression));
                case BoundConversionExpression conversion when !conversion.IsUserDefined && conversion.Conversion.Exists && conversion.IsBoxing &&
                    conversion.Type.GetNonNullableType().TypeKind == TypeKind.Interface &&
                    capabilities?.Allows(LinearInstructionKind.BoxToObject) == true && capabilities.Allows(LinearInstructionKind.ReferenceConvert) &&
                    TryType(conversion.Expression.Type, false, out var interfaceValue) && capabilities.Allows(interfaceValue) &&
                    TryType(conversion.Type, false, out var interfaceTarget) && capabilities.Allows(interfaceTarget):
                    if (!LowerValue(conversion.Expression, atStatementBoundary: atStatementBoundary)) return false;
                    instructions.Add(new(LinearInstructionKind.BoxToObject, Syntax(expression), Type: conversion.Expression.Type));
                    instructions.Add(new(LinearInstructionKind.ReferenceConvert, Syntax(expression), Type: conversion.Type));
                    return true;
                case BoundConversionExpression conversion when !conversion.IsUserDefined && conversion.Conversion.Exists &&
                    conversion.Type.GetNonNullableType().SpecialType == SpecialType.System_Object &&
                    (conversion.IsBoxing || conversion.Expression.Type is ITypeParameterSymbol) &&
                    capabilities?.Allows(LinearInstructionKind.BoxToObject) == true &&
                    TryType(conversion.Expression.Type, false, out var boxedType) && capabilities.Allows(boxedType):
                    if (!LowerValue(conversion.Expression, atStatementBoundary: atStatementBoundary)) return false;
                    instructions.Add(new(LinearInstructionKind.BoxToObject, Syntax(expression), Type: conversion.Expression.Type));
                    return true;
                case BoundConversionExpression conversion when !conversion.IsUserDefined && conversion.Conversion.Exists &&
                    conversion.Expression.Type.IsReferenceType &&
                    (conversion.Type is ITypeParameterSymbol || conversion.Conversion.IsUnboxing) &&
                    capabilities?.Allows(LinearInstructionKind.UnboxAny) == true &&
                    TryType(conversion.Type, false, out var unboxedType) && capabilities.Allows(unboxedType):
                    if (!LowerValue(conversion.Expression, atStatementBoundary: atStatementBoundary)) return false;
                    instructions.Add(new(LinearInstructionKind.UnboxAny, Syntax(expression), Type: conversion.Type));
                    return true;
                case BoundConversionExpression conversion when conversion.Conversion.IsReference && !conversion.IsUserDefined &&
                    capabilities?.Allows(LinearInstructionKind.ReferenceConvert) == true && conversion.Expression.Type.IsReferenceType &&
                    conversion.Type.IsReferenceType && conversion.Type.TypeKind != TypeKind.Delegate &&
                    TryType(conversion.Type, false, out var targetType) && capabilities.Allows(targetType) &&
                    TryType(conversion.Expression.Type, false, out var sourceType) && capabilities.Allows(sourceType):
                    if (!LowerValue(conversion.Expression, atStatementBoundary: atStatementBoundary)) return false;
                    instructions.Add(new(LinearInstructionKind.ReferenceConvert, Syntax(expression), Type: conversion.Type));
                    return true;
                case BoundConversionExpression conversion when conversion.Conversion.IsReference && conversion.Conversion.IsImplicit &&
                    capabilities?.AllowsInterfaceDispatch == true && conversion.Type.GetNonNullableType() is INamedTypeSymbol { TypeKind: TypeKind.Interface, Arity: 0 } target &&
                    SourceInterfacePlan.HasSupportedIdentity(target) && TryType(conversion.Expression.Type, false, out var from) && capabilities.Allows(from):
                    return LowerValue(conversion.Expression, atStatementBoundary: atStatementBoundary);
                case BoundConversionExpression conversion when !conversion.IsUserDefined && conversion.Conversion.Exists && capabilities?.Allows(EmissionDeclarationKind.Enum) == true &&
                    conversion.Expression.Type.SpecialType == SpecialType.System_Int32 && conversion.Type is INamedTypeSymbol { TypeKind: TypeKind.Enum, EnumUnderlyingType.SpecialType: SpecialType.System_Int32 }:
                    if (!LowerValue(conversion.Expression, atStatementBoundary: atStatementBoundary)) return false;
                    instructions.Add(new(LinearInstructionKind.EnumFromInt32, Syntax(expression), Type: conversion.Type)); return true;
                case BoundConversionExpression conversion when !conversion.IsUserDefined && conversion.Conversion.Exists && capabilities?.Allows(EmissionDeclarationKind.Enum) == true &&
                    conversion.Type.SpecialType == SpecialType.System_Int32 && conversion.Expression.Type is INamedTypeSymbol { TypeKind: TypeKind.Enum, EnumUnderlyingType.SpecialType: SpecialType.System_Int32 }:
                    if (!LowerValue(conversion.Expression, atStatementBoundary: atStatementBoundary)) return false;
                    instructions.Add(new(LinearInstructionKind.EnumToInt32, Syntax(expression), Type: conversion.Expression.Type)); return true;
                case BoundConversionExpression { IsIdentity: true } conversion:
                    return LowerValue(conversion.Expression, atStatementBoundary: atStatementBoundary);
                case BoundConversionExpression conversion when conversion.Conversion.IsNumeric && !conversion.IsUserDefined &&
                    IsNumeric(conversion.Expression.Type.SpecialType) && IsNumeric(conversion.Type.SpecialType):
                    if (!LowerValue(conversion.Expression, atStatementBoundary: atStatementBoundary)) return false;
                    ConvertNumeric(conversion.Expression.Type.SpecialType, conversion.Type.SpecialType, Syntax(expression));
                    return true;
                case BoundBinaryExpression { Operator.MethodSymbol: { IsStatic: true } binaryMethod } binary when
                    binaryMethod.Parameters.Length == 2 && binaryMethod.Parameters.All(p => p.RefKind == RefKind.None) &&
                    TrySignature(binaryMethod, out var binarySignature) && SupportedTypeArguments(binaryMethod) &&
                    (capabilities is null || capabilities.Allows(binarySignature)):
                    if (!LowerValue(binary.Left, binaryMethod.Parameters[0].Type) ||
                        !LowerValue(binary.Right, binaryMethod.Parameters[1].Type)) return false;
                    Add(LinearInstructionKind.Call, Syntax(expression), method: binaryMethod);
                    return true;
                case BoundBinaryExpression characterComparison when model.Compilation.Options.UseGraphemeChar &&
                    characterComparison.Operator.MethodSymbol is null &&
                    characterComparison.Left.Type.SpecialType == SpecialType.System_Char &&
                    characterComparison.Right.Type.SpecialType == SpecialType.System_Char &&
                    characterComparison.Operator.OperatorKind is OperatorKind.Equality or OperatorKind.Inequality:
                    var characterOwner = model.Compilation.GetSpecialType(SpecialType.System_Char);
                    var characterEquals = characterOwner.GetMembers("Equals").OfType<IMethodSymbol>().SingleOrDefault(m =>
                        !m.IsStatic && m.Arity == 0 && m.Parameters is [{ RefKind: RefKind.None, Type.SpecialType: SpecialType.System_Char }] &&
                        m.ReturnType.SpecialType == SpecialType.System_Boolean);
                    if (characterEquals is null || !SupportedInstanceCall(characterEquals) || !TryType(characterOwner, false, out var characterStorage))
                        return Reject("grapheme equality contract unavailable", Syntax(expression));
                    var characterSlot = localTypes.Count; localTypes.Add(characterStorage);
                    if (!LowerValue(characterComparison.Left)) return false;
                    Add(LinearInstructionKind.StoreLocal, Syntax(expression), characterSlot);
                    instructions.Add(new(LinearInstructionKind.LocalAddress, Syntax(expression), characterSlot, Type: characterOwner));
                    if (!LowerValue(characterComparison.Right)) return false;
                    Add(LinearInstructionKind.ValueInstanceCall, Syntax(expression), method: characterEquals);
                    if (characterComparison.Operator.OperatorKind == OperatorKind.Inequality) Add(LinearInstructionKind.Not, Syntax(expression));
                    return true;
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
                case BoundBinaryExpression comparison when capabilities?.Allows(LinearInstructionKind.ReferenceIsNull) == true &&
                    comparison.Operator.MethodSymbol is null && comparison.Operator.OperatorKind is OperatorKind.Equality or OperatorKind.Inequality &&
                    (IsNullLiteral(comparison.Left) || IsNullLiteral(comparison.Right)):
                    var reference = IsNullLiteral(comparison.Left) ? comparison.Right : comparison.Left;
                    if (!reference.Type.IsReferenceType || !TryType(reference.Type, false, out var referenceType) || !capabilities.Allows(referenceType))
                        return Reject("null comparison requires a supported reference", Syntax(expression));
                    if (!LowerValue(reference)) return false;
                    Add(LinearInstructionKind.ReferenceIsNull, Syntax(expression));
                    if (comparison.Operator.OperatorKind == OperatorKind.Inequality) Add(LinearInstructionKind.Not, Syntax(expression));
                    return true;
                case BoundBinaryExpression enumBits when capabilities?.Allows(EmissionDeclarationKind.Enum) == true &&
                    enumBits.Operator.MethodSymbol is null && enumBits.Operator.OperatorKind is OperatorKind.BitwiseAnd or OperatorKind.BitwiseOr or OperatorKind.BitwiseXor &&
                    enumBits.Left.Type is INamedTypeSymbol { TypeKind: TypeKind.Enum, EnumUnderlyingType.SpecialType: SpecialType.System_Int32 } &&
                    SymbolEqualityComparer.Default.Equals(enumBits.Left.Type, enumBits.Right.Type) &&
                    SymbolEqualityComparer.Default.Equals(enumBits.Left.Type, enumBits.Type):
                    if (!LowerValue(enumBits.Left)) return false;
                    instructions.Add(new(LinearInstructionKind.EnumToInt32, Syntax(expression), Type: enumBits.Left.Type));
                    if (!LowerValue(enumBits.Right)) return false;
                    instructions.Add(new(LinearInstructionKind.EnumToInt32, Syntax(expression), Type: enumBits.Right.Type));
                    instructions.Add(new(enumBits.Operator.OperatorKind == OperatorKind.BitwiseAnd ? LinearInstructionKind.BitwiseAnd :
                        enumBits.Operator.OperatorKind == OperatorKind.BitwiseOr ? LinearInstructionKind.BitwiseOr : LinearInstructionKind.BitwiseXor, Syntax(expression)));
                    instructions.Add(new(LinearInstructionKind.EnumFromInt32, Syntax(expression), Type: enumBits.Type));
                    return true;
                case BoundBinaryExpression enumComparison when enumComparison.Operator.MethodSymbol is null &&
                    capabilities?.Allows(EmissionDeclarationKind.Enum) == true && enumComparison.Operator.OperatorKind is OperatorKind.Equality or OperatorKind.Inequality &&
                    enumComparison.Left.Type.TypeKind == TypeKind.Enum && SymbolEqualityComparer.Default.Equals(enumComparison.Left.Type, enumComparison.Right.Type):
                    if (!LowerValue(enumComparison.Left)) return false;
                    instructions.Add(new(LinearInstructionKind.EnumToInt32, Syntax(expression), Type: enumComparison.Left.Type));
                    if (!LowerValue(enumComparison.Right)) return false;
                    instructions.Add(new(LinearInstructionKind.EnumToInt32, Syntax(expression), Type: enumComparison.Right.Type));
                    Add(LinearInstructionKind.Equal, Syntax(expression));
                    if (enumComparison.Operator.OperatorKind == OperatorKind.Inequality) Add(LinearInstructionKind.Not, Syntax(expression));
                    return true;
                case BoundBinaryExpression shift when shift.Operator.MethodSymbol is null &&
                    shift.Operator.LeftType.SpecialType is SpecialType.System_Int32 or SpecialType.System_Int64 or SpecialType.System_UInt32 or SpecialType.System_UInt64 &&
                    shift.Operator.RightType.SpecialType == SpecialType.System_Int32 &&
                    shift.Operator.OperatorKind is OperatorKind.ShiftLeft or OperatorKind.ShiftRight:
                    if (!LowerBinaryOperand(shift.Left, shift.Operator.LeftType) || !LowerBinaryOperand(shift.Right, shift.Operator.RightType)) return false;
                    Add(shift.Operator.OperatorKind == OperatorKind.ShiftLeft ? LinearInstructionKind.ShiftLeft : IsUnsigned(shift.Operator.LeftType.SpecialType) ? LinearInstructionKind.UnsignedShiftRight : LinearInstructionKind.ShiftRight, Syntax(expression));
                    return true;
                case BoundBinaryExpression binary when binary.Operator.MethodSymbol is null &&
                    ((IsNumeric(binary.Operator.LeftType.SpecialType) &&
                      binary.Operator.RightType.SpecialType == binary.Operator.LeftType.SpecialType) ||
                     (binary.Operator.LeftType.SpecialType == SpecialType.System_Boolean &&
                      binary.Operator.RightType.SpecialType == SpecialType.System_Boolean &&
                      binary.Operator.OperatorKind is OperatorKind.Equality or OperatorKind.Inequality or
                          OperatorKind.BitwiseAnd or OperatorKind.BitwiseOr or OperatorKind.BitwiseXor)):
                    if (binary.Operator.OperatorKind is not (OperatorKind.Addition or OperatorKind.Subtraction or OperatorKind.Multiplication or OperatorKind.Division or OperatorKind.Modulo or
                        OperatorKind.BitwiseAnd or OperatorKind.BitwiseOr or OperatorKind.BitwiseXor or
                        OperatorKind.Equality or OperatorKind.LessThan or OperatorKind.GreaterThan or
                        OperatorKind.Inequality or OperatorKind.LessThanOrEqual or OperatorKind.GreaterThanOrEqual))
                        return Reject("binary operator " + binary.Operator.OperatorKind, Syntax(expression));
                    if (!LowerBinaryOperand(binary.Left, binary.Operator.LeftType) || !LowerBinaryOperand(binary.Right, binary.Operator.RightType)) return false;
                    Add(binary.Operator.OperatorKind switch
                    {
                        OperatorKind.Addition => LinearInstructionKind.Add,
                        OperatorKind.Subtraction => LinearInstructionKind.Subtract,
                        OperatorKind.Multiplication => LinearInstructionKind.Multiply,
                        OperatorKind.Division when IsUnsigned(binary.Operator.LeftType.SpecialType) => LinearInstructionKind.UnsignedDivide,
                        OperatorKind.Division => LinearInstructionKind.Divide,
                        OperatorKind.Modulo when IsUnsigned(binary.Operator.LeftType.SpecialType) => LinearInstructionKind.UnsignedRemainder,
                        OperatorKind.Modulo => LinearInstructionKind.Remainder,
                        OperatorKind.BitwiseAnd => LinearInstructionKind.BitwiseAnd,
                        OperatorKind.BitwiseOr => LinearInstructionKind.BitwiseOr,
                        OperatorKind.BitwiseXor => LinearInstructionKind.BitwiseXor,
                        OperatorKind.Equality or OperatorKind.Inequality => LinearInstructionKind.Equal,
                        OperatorKind.GreaterThanOrEqual when binary.Operator.LeftType.SpecialType is SpecialType.System_Single or SpecialType.System_Double => LinearInstructionKind.LessOrUnordered,
                        OperatorKind.LessThanOrEqual when binary.Operator.LeftType.SpecialType is SpecialType.System_Single or SpecialType.System_Double => LinearInstructionKind.GreaterOrUnordered,
                        OperatorKind.LessThan or OperatorKind.GreaterThanOrEqual when IsUnsigned(binary.Operator.LeftType.SpecialType) => LinearInstructionKind.UnsignedLess,
                        OperatorKind.GreaterThan or OperatorKind.LessThanOrEqual when IsUnsigned(binary.Operator.LeftType.SpecialType) => LinearInstructionKind.UnsignedGreater,
                        OperatorKind.LessThan or OperatorKind.GreaterThanOrEqual => LinearInstructionKind.Less,
                        _ => LinearInstructionKind.Greater
                    }, Syntax(expression));
                    if (binary.Operator.OperatorKind is OperatorKind.Inequality or OperatorKind.LessThanOrEqual or OperatorKind.GreaterThanOrEqual)
                        Add(LinearInstructionKind.Not, Syntax(expression));
                    return true;
                case BoundFunctionExpression function when capabilities?.AllowsFunctionValues == true &&
                    function.Symbol is SourceLambdaSymbol { IsAsync: false, IsIterator: false, IsExpressionTreeLambda: false, IsGenericMethod: false } lambda &&
                    lambda.ContainingType?.Arity is not > 0 &&
                    TryType(function.DelegateType, false, out var lambdaType) && capabilities.Allows(lambdaType):
                    foreach (var capture in function.CapturedVariables)
                    {
                        BoundExpression? captured = capture switch
                        {
                            INamedTypeSymbol { IsReferenceType: true } selfType => new BoundSelfExpression(selfType),
                            // Heap async lowering moved suspension locals into fields. Capture
                            // their current value, retaining the same immutable-reference semantics
                            // as ordinary native closures; the callback still uses its capture slot.
                            ILocalSymbol { IsMutable: false } local when source.ContainingType is SynthesizedAsyncStateMachineTypeSymbol machine &&
                                machine.TryGetHoistedLocalField(local, out var field) => new BoundFieldAccess(field),
                            ILocalSymbol { IsMutable: false } local => new BoundLocalAccess(local),
                            IParameterSymbol { IsMutable: false, RefKind: RefKind.None } parameter => new BoundParameterAccess(parameter),
                            _ => null
                        };
                        if (captured is null || capabilities.Allows(LinearInstructionKind.LoadCapture) != true ||
                            !TryType(captured.Type, false, out var capturedType) || !capabilities.Allows(capturedType) ||
                            !(captured.Type.IsReferenceType || capturedType.Primitive is EmissionPrimitiveType.Int32 or
                                EmissionPrimitiveType.Int64 or EmissionPrimitiveType.Single or EmissionPrimitiveType.Double or EmissionPrimitiveType.Boolean or EmissionPrimitiveType.Byte))
                            return Reject("closure capture requires an immutable reference or supported primitive local or by-value parameter", Syntax(expression));
                        if (!LowerValue(captured)) return false;
                    }
                    functions.Add((function, Syntax(expression)));
                    instructions.Add(new(LinearInstructionKind.FunctionBind, Syntax(expression), Method: lambda, Type: function.DelegateType));
                    return true;
                case BoundDelegateCreationExpression creation when capabilities?.AllowsFunctionValues == true &&
                    creation.Method is { } target &&
                    (!target.IsGenericMethod || capabilities.AllowsGenericMethods) &&
                    (target.IsStatic || target is { IsVirtual: false, IsOverride: false, IsAbstract: false, ContainingType.IsReferenceType: true } && SupportedInstanceCall(target)
                        || target.ContainingType?.TypeKind == TypeKind.Interface && capabilities.AllowsInterfaceDispatch) &&
                    (target.ContainingType?.Arity is not > 0 || capabilities.AllowsGenericClassOwners) &&
                    !target.OriginalDefinition.DeclaringSyntaxReferences.IsEmpty &&
                    TryType(creation.DelegateType, false, out var functionType) && capabilities.Allows(functionType):
                    if (!target.IsStatic && !Receiver(creation.Receiver, target.ContainingType!, Syntax(expression))) return false;
                    instructions.Add(new(LinearInstructionKind.FunctionBind, Syntax(expression), Method: target, Type: creation.DelegateType));
                    return true;
                case BoundDelegateCreationExpression unsupportedFunction:
                    return Reject("function reference " + unsupportedFunction.Method?.ToDisplayString() + " in " + source.ToDisplayString(), Syntax(expression));
                case BoundInvocationExpression call when capabilities?.AllowsFunctionValues == true &&
                    call.Method.Name == "Invoke" && call.Method.ContainingType?.TypeKind == TypeKind.Delegate && call.Receiver is not null &&
                    call.Receiver.Type is INamedTypeSymbol function && CallableSignature.TryFunction(function, out var shape, capabilities):
                    if (call.Arguments.Count() != shape.ParameterCount || !LowerValue(call.Receiver)) return false;
                    foreach (var argument in call.Arguments) if (!LowerValue(argument)) return false;
                    instructions.Add(new(LinearInstructionKind.FunctionInvoke, Syntax(expression), Type: function));
                    return true;
                case BoundInvocationExpression call when capabilities?.Allows(LinearInstructionKind.ConstrainedCall) == true &&
                    call.Method is { IsStatic: false, IsAbstract: true, IsGenericMethod: false, ContainingType.TypeKind: TypeKind.Interface } &&
                    call.Receiver?.Type is ITypeParameterSymbol { DeclaringMethodParameterOwner: not null } receiverParameter:
                    var constrainedArguments = call.Arguments.ToArray();
                    if (!TrySignature(call.Method, out var constrainedSignature) || !capabilities.Allows(constrainedSignature) ||
                        constrainedArguments.Length != call.Method.Parameters.Length || call.Method.Parameters.Any(p => p.RefKind != RefKind.None))
                        return Reject("unsupported constrained instance signature", Syntax(expression));
                    if (!LowerReference(call.Receiver)) return false;
                    for (int i = 0; i < constrainedArguments.Length; i++)
                        if (!LowerValue(constrainedArguments[i], call.Method.Parameters[i].Type)) return false;
                    instructions.Add(new(LinearInstructionKind.ConstrainedCall, Syntax(expression), Method: call.Method is NativeSelfMethodSymbol projectedCall ? projectedCall.AdapterMethod : call.Method, Type: receiverParameter));
                    return true;
                case BoundInvocationExpression call when call.ExtensionReceiver is null &&
                    (call.Method.IsStatic && call.Receiver is null or BoundTypeExpression ||
                     call.Method.MethodKind is (MethodKind.Ordinary or MethodKind.PropertyGet or MethodKind.PropertySet) && SupportedInstanceCall(call.Method)):
                    if (!TrySignature(call.Method, out var callSignature)) return Reject("only supported value signatures and unconstrained generics (Unit only as result): " + call.Method.Name, Syntax(expression));
                    if (capabilities is not null && (!capabilities.Allows(callSignature) ||
                        !SupportedTypeArguments(call.Method)))
                        return Reject("target does not support call signature types", Syntax(expression));
                    var arguments = call.Arguments.ToArray();
                    if (arguments.Length != call.Method.Parameters.Length) return Reject("optional/expanded arguments", Syntax(expression));
                    if (!call.Method.IsStatic && !Receiver(call.Receiver, call.Method.ContainingType!, Syntax(expression))) return false;
                    for (int i = 0; i < arguments.Length; i++)
                    {
                        if (call.Method.Parameters[i].RefKind is RefKind.Ref or RefKind.Out
                            ? !LowerReference(arguments[i]) : !LowerValue(arguments[i], call.Method.Parameters[i].Type)) return false;
                    }
                    Add(call.Method.IsStatic ? LinearInstructionKind.Call : call.Receiver is BoundBaseExpression ? LinearInstructionKind.DirectInstanceCall : InstanceCallKind(call.Method), Syntax(expression), method: call.Method);
                    return true;
                case BoundInvocationExpression rejectedCall:
                    return Reject("invocation " + rejectedCall.Method.ToDisplayString(), Syntax(expression));
                case BoundConversionExpression conversion:
                    return Reject("lowered expression BoundConversionExpression (" + conversion.Expression.Type.ToDisplayString() + " -> " +
                        conversion.Type.ToDisplayString() + (conversion.IsBoxing ? "; boxing" : "") + ")", Syntax(expression));
                default: return Reject("lowered expression " + expression.GetType().Name, Syntax(expression));
            }
        }
    }
}
