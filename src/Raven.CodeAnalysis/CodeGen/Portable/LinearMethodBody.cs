using System.Collections.Immutable;

using Raven.CodeAnalysis.Symbols;
using Raven.CodeAnalysis.Syntax;

using OperatorKind = Raven.CodeAnalysis.BinaryOperatorKind;

namespace Raven.CodeAnalysis.CodeGen.Portable;

// Logical instructions carry compiler symbols, never Reflection.Emit or native metadata handles.
internal enum LinearInstructionKind { Constant, Argument, Add, Subtract, Multiply, Call, ConsoleWrite, String, Return, LoadLocal, StoreLocal, Boolean, Not, Equal, Less, Greater, Label, Branch, BranchTrue, BranchFalse, Pop, Constant64, Convert64, Convert32, Negate, Complement, Divide, Remainder, BitwiseAnd, BitwiseOr, BitwiseXor, ShiftLeft, ShiftRight, Receiver, LoadField, StoreField, InstanceCall, NewObject, NewArray, LoadElement, StoreElement, ArrayLength, Duplicate, DefaultValue }

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
internal readonly record struct EmissionType(EmissionPrimitiveType? Primitive = null, INamedTypeSymbol? Class = null, IArrayTypeSymbol? Array = null, ITypeParameterSymbol? MethodParameter = null, ITypeParameterSymbol? OwnerParameter = null);

// Build an instruction plan from the compiler-lowered body before touching a backend.
// Unsupported .NET bodies stay on the general generator; native emission reports the
// source-located boundary. Language rewrites remain owned by the existing Lowerer.
internal sealed class LinearMethodBody(ImmutableArray<LinearInstruction> instructions, ImmutableArray<EmissionType> localTypes, int labelCount)
{
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
        => CallableSignature.TryType(method.ReturnType, false, out _);

    internal static bool TryLower(IMethodSymbol source, SemanticModel model, SyntaxNode bodySyntax,
        Func<BoundInvocationExpression, bool> permitsConsoleWrite, out LinearMethodBody? lowered, out LinearBodyFailure? failure, EmissionCapabilities? capabilities = null)
    {
        var instructions = ImmutableArray.CreateBuilder<LinearInstruction>();
        var localTypes = ImmutableArray.CreateBuilder<EmissionType>();
        var nextLabel = 0;
        var labels = new Dictionary<ILabelSymbol, int>(SymbolEqualityComparer.Default);
        var locals = new Dictionary<ILocalSymbol, int>(SymbolEqualityComparer.Default);
        LinearBodyFailure? rejected = null;
        // Arrow clauses expose their bound statement block in the original view, as
        // consumed by the general generator. Reuse compiler lowering for conversions
        // and Unit expression statements instead of synthesizing backend returns.
        var body = source.MethodKind == MethodKind.Constructor && bodySyntax is ClassDeclarationSyntax
            ? new BoundBlockStatement([]) : model.Compilation.TryGetSynthesizedMethodBody(source, BoundTreeView.Lowered, out var synthesized) && synthesized is not null
            ? synthesized : bodySyntax is ArrowExpressionClauseSyntax
            ? model.GetBoundNode(bodySyntax, BoundTreeView.Original) is BoundBlockStatement arrowBody
                ? Lowerer.LowerBlock(source, arrowBody) : null
            : model.GetBoundNode(bodySyntax, BoundTreeView.Lowered) as BoundBlockStatement;
        if (body is not null && source.MethodKind == MethodKind.Constructor)
        {
            var initialization = Lowerer.LowerBlock(source, new BoundBlockStatement(FieldInitializationPlan.Create(model.Compilation, source).ToArray()));
            body = new BoundBlockStatement([initialization, body]);
        }
        var success = body is not null ? LowerBody(body) : Reject("lowered block body unavailable", bodySyntax);
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
            if (!CallableSignature.TryCreate(source, out var signature)) return Reject("only supported value signatures and unconstrained generics (Unit only as result)", bodySyntax);
            if (capabilities is not null && !capabilities.Allows(signature))
                return Reject("target does not support callable signature types", bodySyntax);
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
                if (statement is BoundExpressionStatement { Expression: BoundBlockExpression discardedBlock })
                {
                    if (!discardedBlock.LocalsToDispose.IsEmpty) return Reject("scope disposal", Syntax(discardedBlock));
                    foreach (var child in discardedBlock.Statements)
                        if (!LowerStatements(child)) return false;
                    continue;
                }
                if (statement is BoundExpressionStatement { Expression: BoundUnitExpression }) continue;
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
                        if (variable.Initializer is null || variable.FixedAddressInitializer is not null || variable.FixedPinnedLocal is not null)
                            return Reject("only initialized value locals", Syntax(variable));
                        EmissionType localType;
                        if (EmissionPrimitiveTypes.TryGetValueType(variable.Local.Type, out var primitive))
                        {
                            if (capabilities is not null && !capabilities.Allows(primitive)) return Reject("target does not support local type " + primitive, Syntax(variable));
                            localType = new(Primitive: primitive);
                        }
                        else if (variable.Local.Type is INamedTypeSymbol nominal && SourceTypePlan.TryCreate(nominal, out var typePlan) && !typePlan!.IsStatic && capabilities?.AllowsRootClassLocals == true)
                            localType = new(Class: nominal);
                        else if (variable.Local.Type is (IArrayTypeSymbol or ITypeParameterSymbol) && CallableSignature.TryType(variable.Local.Type, false, out var arrayType) && capabilities?.Allows(arrayType) == true)
                            localType = arrayType;
                        else return Reject("target does not support local type " + variable.Local.Type.Name, Syntax(variable));
                        if (!LowerValue(variable.Initializer)) return false;
                        var slot = locals.Count;
                        locals.Add(variable.Local, slot);
                        localTypes.Add(localType);
                        Add(LinearInstructionKind.StoreLocal, Syntax(variable), slot);
                    }
                    continue;
                }
                var memberAssignment = statement switch
                {
                    BoundAssignmentStatement { Expression: var expression } => expression,
                    BoundExpressionStatement { Expression: BoundAssignmentExpression expression } => expression,
                    _ => null
                };
                if (memberAssignment is BoundIndexerAssignmentExpression indexerAssignment)
                {
                    var access = indexerAssignment.Left;
                    if (access.Indexer.SetMethod is not { } setter || !LowerIndexerReceiverAndArguments(access, setter, true) || !LowerValue(indexerAssignment.Right))
                        return Reject("unsupported indexed property assignment", Syntax(statement));
                    Add(LinearInstructionKind.InstanceCall, Syntax(statement), method: setter);
                    continue;
                }
                if (memberAssignment is BoundArrayAssignmentExpression arrayAssignment)
                {
                    if (!ArrayReceiverAndIndex(arrayAssignment.Left) || !LowerValue(arrayAssignment.Right)) return false;
                    instructions.Add(new(LinearInstructionKind.StoreElement, Syntax(statement), Type: arrayAssignment.Left.ElementType));
                    continue;
                }
                if (memberAssignment is BoundFieldAssignmentExpression fieldAssignment)
                {
                    if (fieldAssignment.RequiresReceiverAddress || !SupportedField(fieldAssignment.Field) ||
                        !Receiver(fieldAssignment.Receiver, fieldAssignment.Field.ContainingType!, Syntax(statement)) || !LowerValue(fieldAssignment.Right))
                        return Reject("unsupported instance field assignment", Syntax(statement));
                    instructions.Add(new(LinearInstructionKind.StoreField, Syntax(statement), Field: fieldAssignment.Field));
                    continue;
                }
                if (memberAssignment is BoundPropertyAssignmentExpression propertyAssignment)
                {
                    if (propertyAssignment.Property.SetMethod is not { } setter || !SupportedInstanceCall(setter) ||
                        !Receiver(propertyAssignment.Receiver, setter.ContainingType!, Syntax(statement)) || !LowerValue(propertyAssignment.Right))
                        return Reject("unsupported instance property assignment", Syntax(statement));
                    Add(LinearInstructionKind.InstanceCall, Syntax(statement), method: setter);
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
                    if (permitsConsoleWrite(call) && call.Arguments.ToArray() is [var text])
                    {
                        if (!LowerValue(text)) return false;
                        Add(LinearInstructionKind.ConsoleWrite, Syntax(call), method: call.Method);
                        continue;
                    }
                    if (!LowerValue(call)) return false;
                    if (ReturnsValue(call.Method)) Add(LinearInstructionKind.Pop, Syntax(statement));
                    continue;
                }
                if (statement is BoundReturnStatement { Expression: null or BoundUnitExpression } && !ReturnsValue(source))
                {
                    Add(LinearInstructionKind.Return, Syntax(statement));
                    continue;
                }
                if (statement is not BoundReturnStatement { Expression: { } value })
                    return Reject("unsupported lowered statement " + statement.GetType().Name +
                        (statement is BoundExpressionStatement unsupported ? " (" + unsupported.Expression.GetType().Name + ")" : ""), Syntax(statement));
                if (!LowerValue(value)) return false;
                Add(LinearInstructionKind.Return, Syntax(statement));
            }
            return true;
        }

        static IEnumerable<BoundStatement> WalkStatements(BoundStatement statement)
        {
            yield return statement;
            IEnumerable<BoundStatement> children = statement switch
            {
                BoundBlockStatement block => block.Statements,
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
            CallableSignature.TryType(field.Type, false, out var type) && (capabilities is null || capabilities.Allows(type));
        bool SupportedTypeArguments(IMethodSymbol method) => capabilities is null ||
            method.TypeArguments.Concat(method.ContainingType?.TypeArguments ?? []).All(t => CallableSignature.TryType(t, false, out var type) && capabilities.Allows(type));
        bool SupportedInstanceCall(IMethodSymbol method) => !method.IsStatic && !method.IsVirtual && !method.IsOverride &&
            method.ContainingType is { } owner && SourceTypePlan.TryCreate(owner, out _) &&
            CallableSignature.TryCreate(method, out var signature) && SupportedTypeArguments(method) && (capabilities is null || capabilities.Allows(signature));
        bool Receiver(BoundExpression? receiver, INamedTypeSymbol owner, SyntaxNode syntax)
        {
            if (receiver is not null) return LowerValue(receiver);
            if (source.IsStatic || !SymbolEqualityComparer.Default.Equals(source.ContainingType, owner)) return Reject("implicit receiver unavailable", syntax);
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
        bool SupportedArray(ITypeSymbol type) => type is IArrayTypeSymbol && CallableSignature.TryType(type, false, out var array) &&
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
                if (!LowerValue(elements[i])) return false;
                instructions.Add(new(LinearInstructionKind.StoreElement, syntax, Type: type.ElementType));
            }
            return true;
        }
        bool LowerValue(BoundExpression expression)
        {
            if (capabilities is not null && EmissionPrimitiveTypes.TryGetValueType(expression.Type, out var valueType) && !capabilities.Allows(valueType))
                return Reject("target does not support value type " + valueType, Syntax(expression));
            switch (expression)
            {
                case BoundDefaultValueExpression value when CallableSignature.TryType(value.Type, false, out var defaultType) &&
                    (capabilities is null || capabilities.Allows(defaultType)):
                    instructions.Add(new(LinearInstructionKind.DefaultValue, Syntax(expression), Type: value.Type)); return true;
                case BoundIndexerAccessExpression indexer when indexer.Indexer.GetMethod is { } indexGetter:
                    if (!LowerIndexerReceiverAndArguments(indexer, indexGetter, false)) return false;
                    Add(LinearInstructionKind.InstanceCall, Syntax(expression), method: indexGetter); return true;
                case BoundCollectionExpression collection when SupportedArray(collection.Type):
                    return ArrayLiteral((IArrayTypeSymbol)collection.Type, collection.Elements, Syntax(expression));
                case BoundEmptyCollectionExpression empty when SupportedArray(empty.Type):
                    return ArrayLiteral((IArrayTypeSymbol)empty.Type, [], Syntax(expression));
                case BoundArrayAccessExpression access:
                    if (!ArrayReceiverAndIndex(access)) return false;
                    instructions.Add(new(LinearInstructionKind.LoadElement, Syntax(expression), Type: access.ElementType)); return true;
                case BoundMemberAccessExpression { Member: IPropertySymbol { Name: "Length", ContainingType.SpecialType: SpecialType.System_Array } } length when SupportedArray(length.Receiver.Type):
                    if (!LowerValue(length.Receiver)) return false;
                    Add(LinearInstructionKind.ArrayLength, Syntax(expression)); return true;
                case BoundBlockExpression block:
                    if (!block.LocalsToDispose.IsEmpty) return Reject("value block scope disposal", Syntax(block));
                    var statements = block.Statements.ToImmutableArray();
                    if (statements.IsEmpty || statements[^1] is not BoundExpressionStatement { Expression: var result } ||
                        !CallableSignature.TryType(result.Type, false, out var blockType) ||
                        capabilities is not null && !capabilities.Allows(blockType))
                        return Reject("value block requires a supported trailing value expression", Syntax(block));
                    // A value block may be evaluated with earlier operands still on the
                    // stack. Exits must not bypass that enclosing expression's completion.
                    var controlFlow = statements.Take(statements.Length - 1).SelectMany(WalkStatements).ToArray();
                    var localLabels = controlFlow.OfType<BoundLabeledStatement>()
                        .Select(label => label.Label).ToHashSet<ILabelSymbol>(SymbolEqualityComparer.Default);
                    foreach (var statement in controlFlow)
                    {
                        if (statement is BoundReturnStatement or BoundExpressionStatement { Expression: BoundReturnExpression } ||
                            statement is BoundGotoStatement jump && !localLabels.Contains(jump.Target) ||
                            statement is BoundConditionalGotoStatement branch && !localLabels.Contains(branch.Target))
                            return Reject("value block cannot exit its enclosing expression", Syntax(statement));
                    }
                    foreach (var prefix in statements.AsSpan()[..^1])
                        if (!LowerStatements(prefix)) return false;
                    return LowerValue(result);
                case BoundIfExpression conditional when conditional.ElseBranch is not null &&
                    conditional.Condition.Type.SpecialType == SpecialType.System_Boolean &&
                    CallableSignature.TryType(conditional.Type, false, out var conditionalType) &&
                    (capabilities is null || capabilities.Allows(conditionalType)) &&
                    SymbolEqualityComparer.Default.Equals(conditional.ThenBranch.Type, conditional.Type) &&
                    SymbolEqualityComparer.Default.Equals(conditional.ElseBranch.Type, conditional.Type):
                    if (!LowerValue(conditional.Condition)) return false;
                    var alternative = nextLabel++;
                    var joined = nextLabel++;
                    Add(LinearInstructionKind.BranchFalse, Syntax(expression), alternative);
                    if (!LowerValue(conditional.ThenBranch)) return false;
                    Add(LinearInstructionKind.Branch, Syntax(expression), joined);
                    Add(LinearInstructionKind.Label, Syntax(expression), alternative);
                    if (!LowerValue(conditional.ElseBranch)) return false;
                    Add(LinearInstructionKind.Label, Syntax(expression), joined);
                    return true;
                case BoundObjectCreationExpression creation when creation.Initializer is null && creation.Receiver is null &&
                    SourceTypePlan.TryCreate(creation.Constructor.ContainingType!, out var createdType) && !createdType!.IsStatic &&
                    CallableSignature.TryCreate(creation.Constructor, out var constructorSignature) && SupportedTypeArguments(creation.Constructor) &&
                    (capabilities is null || capabilities.Allows(constructorSignature)):
                    var constructorArguments = creation.Arguments.ToArray();
                    if (constructorArguments.Length != creation.Constructor.Parameters.Length) return Reject("optional/expanded constructor arguments", Syntax(expression));
                    foreach (var argument in constructorArguments) if (!LowerValue(argument)) return false;
                    Add(LinearInstructionKind.NewObject, Syntax(expression), method: creation.Constructor); return true;
                case BoundSelfExpression self when !source.IsStatic && SymbolEqualityComparer.Default.Equals(self.Type, source.ContainingType):
                    Add(LinearInstructionKind.Receiver, Syntax(expression)); return true;
                case BoundFieldAccess field when SupportedField(field.Field):
                    if (!Receiver(field.Receiver, field.Field.ContainingType!, Syntax(expression))) return false;
                    instructions.Add(new(LinearInstructionKind.LoadField, Syntax(expression), Field: field.Field)); return true;
                case BoundMemberAccessExpression { Member: IFieldSymbol memberField } fieldAccess when SupportedField(memberField):
                    if (!Receiver(fieldAccess.Receiver, memberField.ContainingType!, Syntax(expression))) return false;
                    instructions.Add(new(LinearInstructionKind.LoadField, Syntax(expression), Field: memberField)); return true;
                case BoundPropertyAccess property when property.Property.GetMethod is { } getter && SupportedInstanceCall(getter):
                    if (!Receiver(null, getter.ContainingType!, Syntax(expression))) return false;
                    Add(LinearInstructionKind.InstanceCall, Syntax(expression), method: getter); return true;
                case BoundMemberAccessExpression { Member: IPropertySymbol memberProperty } access when memberProperty.GetMethod is { } memberGetter && SupportedInstanceCall(memberGetter):
                    if (!Receiver(access.Receiver, memberGetter.ContainingType!, Syntax(expression))) return false;
                    Add(LinearInstructionKind.InstanceCall, Syntax(expression), method: memberGetter); return true;
                case BoundLiteralExpression { Value: string text }:
                    Add(LinearInstructionKind.String, Syntax(expression), text: text); return true;
                case BoundLiteralExpression { Value: bool boolean }:
                    Add(LinearInstructionKind.Boolean, Syntax(expression), boolean ? 1 : 0); return true;
                case BoundLiteralExpression { Value: long value64 }:
                    instructions.Add(new(LinearInstructionKind.Constant64, Syntax(expression), Long: value64)); return true;
                case BoundLiteralExpression { Value: int value }:
                    Add(LinearInstructionKind.Constant, Syntax(expression), value); return true;
                case BoundLocalAccess local:
                    if (!locals.TryGetValue(local.Local, out var slot)) return Reject("undeclared local", Syntax(expression));
                    Add(LinearInstructionKind.LoadLocal, Syntax(expression), slot); return true;
                case BoundParameterAccess parameter:
                    var index = source.Parameters.IndexOf(parameter.Parameter, 0, source.Parameters.Length, SymbolEqualityComparer.Default);
                    if (index < 0) return Reject("captured parameter", Syntax(expression));
                    Add(LinearInstructionKind.Argument, Syntax(expression), index + (source.IsStatic ? 0 : 1)); return true;
                case BoundUnaryExpression { Operator.OperatorKind: BoundUnaryOperatorKind.LogicalNot } unary:
                    if (!LowerValue(unary.Operand)) return false;
                    Add(LinearInstructionKind.Not, Syntax(expression)); return true;
                case BoundUnaryExpression unary when unary.Operator.OperandType.SpecialType is SpecialType.System_Int32 or SpecialType.System_Int64 &&
                    unary.Operator.OperatorKind is BoundUnaryOperatorKind.UnaryPlus or BoundUnaryOperatorKind.UnaryMinus or BoundUnaryOperatorKind.BitwiseNot:
                    if (!LowerValue(unary.Operand)) return false;
                    if (unary.Operator.OperatorKind != BoundUnaryOperatorKind.UnaryPlus)
                        Add(unary.Operator.OperatorKind == BoundUnaryOperatorKind.UnaryMinus ? LinearInstructionKind.Negate : LinearInstructionKind.Complement, Syntax(expression));
                    return true;
                case BoundParenthesizedExpression parenthesized:
                    return LowerValue(parenthesized.Expression);
                case BoundConversionExpression { IsIdentity: true } conversion:
                    return LowerValue(conversion.Expression);
                case BoundConversionExpression conversion when conversion.Conversion.IsNumeric && !conversion.IsUserDefined &&
                    conversion.Expression.Type.SpecialType is SpecialType.System_Int32 or SpecialType.System_Int64 &&
                    conversion.Type.SpecialType is SpecialType.System_Int32 or SpecialType.System_Int64:
                    if (!LowerValue(conversion.Expression)) return false;
                    Add(conversion.Type.SpecialType == SpecialType.System_Int64 ? LinearInstructionKind.Convert64 : LinearInstructionKind.Convert32, Syntax(expression));
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
                case BoundBinaryExpression shift when shift.Operator.MethodSymbol is null &&
                    shift.Operator.LeftType.SpecialType is SpecialType.System_Int32 or SpecialType.System_Int64 &&
                    shift.Operator.RightType.SpecialType == SpecialType.System_Int32 &&
                    shift.Operator.OperatorKind is OperatorKind.ShiftLeft or OperatorKind.ShiftRight:
                    if (!LowerValue(shift.Left) || !LowerValue(shift.Right)) return false;
                    Add(shift.Operator.OperatorKind == OperatorKind.ShiftLeft ? LinearInstructionKind.ShiftLeft : LinearInstructionKind.ShiftRight, Syntax(expression));
                    return true;
                case BoundBinaryExpression binary when binary.Operator.MethodSymbol is null &&
                    ((binary.Operator.LeftType.SpecialType is SpecialType.System_Int32 or SpecialType.System_Int64 &&
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
                    if (!LowerValue(binary.Left) || !LowerValue(binary.Right)) return false;
                    Add(binary.Operator.OperatorKind switch
                    {
                        OperatorKind.Addition => LinearInstructionKind.Add,
                        OperatorKind.Subtraction => LinearInstructionKind.Subtract,
                        OperatorKind.Multiplication => LinearInstructionKind.Multiply,
                        OperatorKind.Division => LinearInstructionKind.Divide,
                        OperatorKind.Modulo => LinearInstructionKind.Remainder,
                        OperatorKind.BitwiseAnd => LinearInstructionKind.BitwiseAnd,
                        OperatorKind.BitwiseOr => LinearInstructionKind.BitwiseOr,
                        OperatorKind.BitwiseXor => LinearInstructionKind.BitwiseXor,
                        OperatorKind.Equality or OperatorKind.Inequality => LinearInstructionKind.Equal,
                        OperatorKind.LessThan or OperatorKind.GreaterThanOrEqual => LinearInstructionKind.Less,
                        _ => LinearInstructionKind.Greater
                    }, Syntax(expression));
                    if (binary.Operator.OperatorKind is OperatorKind.Inequality or OperatorKind.LessThanOrEqual or OperatorKind.GreaterThanOrEqual)
                        Add(LinearInstructionKind.Not, Syntax(expression));
                    return true;
                case BoundInvocationExpression call when call.ExtensionReceiver is null &&
                    (call.Method.IsStatic && call.Receiver is null or BoundTypeExpression ||
                     call.Method.MethodKind == MethodKind.Ordinary && SupportedInstanceCall(call.Method)):
                    if (!CallableSignature.TryCreate(call.Method, out var callSignature)) return Reject("only supported value signatures and unconstrained generics (Unit only as result): " + call.Method.Name, Syntax(expression));
                    if (capabilities is not null && (!capabilities.Allows(callSignature) ||
                        !SupportedTypeArguments(call.Method)))
                        return Reject("target does not support call signature types", Syntax(expression));
                    var arguments = call.Arguments.ToArray();
                    if (arguments.Length != call.Method.Parameters.Length) return Reject("optional/expanded arguments", Syntax(expression));
                    if (!call.Method.IsStatic && !Receiver(call.Receiver, call.Method.ContainingType!, Syntax(expression))) return false;
                    foreach (var argument in arguments)
                    {
                        if (!LowerValue(argument)) return false;
                    }
                    Add(call.Method.IsStatic ? LinearInstructionKind.Call : LinearInstructionKind.InstanceCall, Syntax(expression), method: call.Method);
                    return true;
                default: return Reject("lowered expression " + expression.GetType().Name, Syntax(expression));
            }
        }
    }
}
