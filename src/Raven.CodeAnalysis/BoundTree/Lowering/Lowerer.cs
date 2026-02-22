using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;
using System.Threading;

using Raven.CodeAnalysis.Symbols;

namespace Raven.CodeAnalysis;

internal sealed partial class Lowerer : BoundTreeRewriter
{
    private static int s_lowererInstanceSeed;
    private readonly ISymbol _containingSymbol;
    private readonly Stack<(ILabelSymbol BreakLabel, ILabelSymbol ContinueLabel)> _loopStack = new();
    private readonly ILoweringTraceSink? _loweringTrace;
    private readonly int _lowererInstanceId;
    private int _labelCounter;
    private int _tempCounter;

    private Lowerer(ISymbol containingSymbol, ILoweringTraceSink? loweringTrace)
    {
        _containingSymbol = containingSymbol;
        _loweringTrace = loweringTrace;
        _lowererInstanceId = Interlocked.Increment(ref s_lowererInstanceSeed);
    }

    public static BoundBlockStatement LowerBlock(ISymbol containingSymbol, BoundBlockStatement block)
    {
        block = RewriteIteratorsIfNeeded(containingSymbol, block);

        // Promote implicit returns (last expression or if/else in a value-returning method)
        // before structural lowering so the lowered IL sees explicit BoundReturnStatements.
        block = RewriteImplicitReturnIfNeeded(containingSymbol, block);

        var lowerer = CreateLowerer(containingSymbol);
        return (BoundBlockStatement)lowerer.VisitStatement(block);
    }

    private static BoundBlockStatement RewriteImplicitReturnIfNeeded(ISymbol symbol, BoundBlockStatement body)
    {
        ITypeSymbol? returnType = symbol switch
        {
            IMethodSymbol m => m.ReturnType,
            _ => null
        };

        if (returnType is null)
            return body;

        if (symbol.ContainingAssembly is not SourceAssemblySymbol sourceAssembly)
            return body;

        var compilation = sourceAssembly.Compilation;
        var unitType = compilation.GetSpecialType(SpecialType.System_Unit);

        return ImplicitReturnRewriter.RewriteIfNeeded(returnType, unitType, body);
    }

    public static BoundStatement LowerStatement(ISymbol containingSymbol, BoundStatement statement)
    {
        if (statement is BoundBlockStatement block)
            statement = RewriteIteratorsIfNeeded(containingSymbol, block);

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
        var name = $"{prefix}_{_lowererInstanceId}_{_labelCounter++}";
        var containingType = _containingSymbol.ContainingType as INamedTypeSymbol;
        var containingNamespace = _containingSymbol.ContainingNamespace;
        return new LabelSymbol(name, _containingSymbol, containingType, containingNamespace,
            [Location.None], Array.Empty<SyntaxReference>());
    }

    private SourceLocalSymbol CreateTempLocal(string nameHint, ITypeSymbol type, bool isMutable)
    {
        var containingType = _containingSymbol.ContainingType as INamedTypeSymbol;
        var containingNamespace = _containingSymbol.ContainingNamespace;
        var name = $"<{nameHint}>__{_lowererInstanceId}_{_tempCounter++}";
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

        return new BoundConversionExpression(expression, targetType, conversion);
    }

    public override BoundNode? VisitReturnStatement(BoundReturnStatement node)
    {
        var visited = (BoundReturnStatement?)base.VisitReturnStatement(node);
        if (visited is null)
            return null;

        if (_containingSymbol is not IMethodSymbol containingMethod
            || visited.Expression is null
            || !TryGetAsyncStateMachine(containingMethod, out var stateMachine, out var moveNextMethod)
            || !SymbolEqualityComparer.Default.Equals(containingMethod, moveNextMethod))
        {
            return visited;
        }

        var compilation = GetCompilation();
        var builderMembers = stateMachine.GetBuilderMembers(containingMethod);
        var setResultStatement = CreateAsyncSetResultStatement(stateMachine, builderMembers, visited.Expression, compilation);
        if (setResultStatement is null)
            return visited;

        var statements = new BoundStatement[]
        {
            CreateAsyncStateAssignment(stateMachine, -2, compilation),
            setResultStatement,
            new BoundReturnStatement(null)
        };

        return new BoundBlockStatement(statements);
    }

    private static bool TryGetAsyncStateMachine(
        IMethodSymbol containingMethod,
        out SynthesizedAsyncStateMachineTypeSymbol stateMachine,
        out IMethodSymbol moveNextMethod)
    {
        stateMachine = null!;
        moveNextMethod = null!;

        if (containingMethod.ContainingType is SynthesizedAsyncStateMachineTypeSymbol direct)
        {
            stateMachine = direct;
            moveNextMethod = direct.MoveNextMethod;
            return true;
        }

        if (containingMethod.ContainingType is ConstructedNamedTypeSymbol constructed
            && constructed.ConstructedFrom is SynthesizedAsyncStateMachineTypeSymbol constructedStateMachine)
        {
            var constructedMembers = constructedStateMachine.GetConstructedMembers(containingMethod);
            stateMachine = constructedStateMachine;
            moveNextMethod = constructedMembers.MoveNext;
            return true;
        }

        return false;
    }

    private static BoundAssignmentStatement CreateAsyncStateAssignment(
        SynthesizedAsyncStateMachineTypeSymbol stateMachine,
        int state,
        Compilation compilation)
    {
        var literal = new BoundLiteralExpression(
            BoundLiteralExpressionKind.NumericLiteral,
            state,
            stateMachine.StateField.Type);

        var assignment = new BoundFieldAssignmentExpression(
            new BoundSelfExpression(stateMachine),
            stateMachine.StateField,
            literal,
            compilation.UnitTypeSymbol,
            requiresReceiverAddress: true);

        return new BoundAssignmentStatement(assignment);
    }

    private static BoundStatement? CreateAsyncSetResultStatement(
        SynthesizedAsyncStateMachineTypeSymbol stateMachine,
        SynthesizedAsyncStateMachineTypeSymbol.BuilderMembers builderMembers,
        BoundExpression expression,
        Compilation compilation)
    {
        var setResultMethod = builderMembers.SetResult;
        if (setResultMethod is null)
            return null;

        BoundExpression? argument = expression;
        if (setResultMethod.Parameters.Length == 1)
        {
            if (argument is BoundUnitExpression)
                argument = null;

            argument ??= CreateDefaultValueExpression(setResultMethod.Parameters[0].Type);
            if (argument is null)
                return null;

            argument = ApplyConversionIfNeeded(argument, setResultMethod.Parameters[0].Type, compilation);
        }
        else if (setResultMethod.Parameters.Length != 0)
        {
            return null;
        }
        else if (argument is not BoundUnitExpression)
        {
            return null;
        }

        var receiver = new BoundMemberAccessExpression(new BoundSelfExpression(stateMachine), builderMembers.BuilderField);
        var invocation = setResultMethod.Parameters.Length == 1
            ? new BoundInvocationExpression(setResultMethod, new[] { argument! }, receiver, requiresReceiverAddress: true)
            : new BoundInvocationExpression(setResultMethod, Array.Empty<BoundExpression>(), receiver, requiresReceiverAddress: true);
        return new BoundExpressionStatement(invocation);
    }

    private static BoundExpression? CreateDefaultValueExpression(ITypeSymbol type)
    {
        if (type.SpecialType == SpecialType.System_Void)
            return null;

        if (type.IsReferenceType)
            return new BoundLiteralExpression(BoundLiteralExpressionKind.NullLiteral, null!, type);

        if (type.SpecialType == SpecialType.System_Boolean)
            return new BoundLiteralExpression(BoundLiteralExpressionKind.FalseLiteral, false, type);

        if (type.SpecialType == SpecialType.System_Int32)
            return new BoundLiteralExpression(BoundLiteralExpressionKind.NumericLiteral, 0, type);

        if (type.SpecialType == SpecialType.System_Double)
            return new BoundLiteralExpression(BoundLiteralExpressionKind.NumericLiteral, 0d, type);

        if (type.SpecialType == SpecialType.System_Single)
            return new BoundLiteralExpression(BoundLiteralExpressionKind.NumericLiteral, 0f, type);

        if (type.SpecialType == SpecialType.System_Int64)
            return new BoundLiteralExpression(BoundLiteralExpressionKind.NumericLiteral, 0L, type);

        if (type.SpecialType == SpecialType.System_Decimal)
            return new BoundLiteralExpression(BoundLiteralExpressionKind.NumericLiteral, 0m, type);

        if (type.SpecialType == SpecialType.System_Int16)
            return new BoundLiteralExpression(BoundLiteralExpressionKind.NumericLiteral, (short)0, type);

        if (type.SpecialType == SpecialType.System_Byte)
            return new BoundLiteralExpression(BoundLiteralExpressionKind.NumericLiteral, (byte)0, type);

        if (type.SpecialType == SpecialType.System_UInt16)
            return new BoundLiteralExpression(BoundLiteralExpressionKind.NumericLiteral, (ushort)0, type);

        if (type.SpecialType == SpecialType.System_UInt32)
            return new BoundLiteralExpression(BoundLiteralExpressionKind.NumericLiteral, 0u, type);

        if (type.SpecialType == SpecialType.System_UInt64)
            return new BoundLiteralExpression(BoundLiteralExpressionKind.NumericLiteral, 0ul, type);

        return null;
    }
    public override BoundExpression? VisitObjectCreationExpression(BoundObjectCreationExpression node)
    {
        // First rewrite children.
        node = (BoundObjectCreationExpression)base.VisitObjectCreationExpression(node)!;

        // If there is no initializer, nothing special to do.
        if (node.Initializer is null)
            return node;

        // Lowering turns:
        //   new T(args) { entries }
        // into:
        //   {
        //     var <init> = new T(args);
        //     <apply entries>
        //     <init>
        //   }

        // Ensure we don't recurse forever by clearing the initializer on the instance expression.
        var instanceExpr = new BoundObjectCreationExpression(
            node.Constructor,
            node.Arguments,
            node.Receiver,
            initializer: null);

        return LowerObjectInitializerExpression(instanceExpr, node.Initializer);
    }

    private BoundExpression LowerObjectInitializerExpression(BoundExpression instanceExpression, BoundObjectInitializer initializer)
    {
        var compilation = GetCompilation();

        var instanceType = instanceExpression.Type ?? compilation.ErrorTypeSymbol;

        // Evaluate the instance exactly once.
        var temp = CreateTempLocal("init", instanceType, isMutable: true);
        var tempAccess = new BoundLocalAccess(temp);

        var statements = new List<BoundStatement>();

        // var <temp> = <instanceExpression>;
        statements.Add(new BoundLocalDeclarationStatement(
            ImmutableArray.Create(new BoundVariableDeclarator(temp, instanceExpression))));

        foreach (var entry in initializer.Entries)
        {
            switch (entry)
            {
                case BoundObjectInitializerAssignmentEntry assign:
                    statements.Add(LowerObjectInitializerAssignment(tempAccess, assign, compilation));
                    break;

                case BoundObjectInitializerExpressionEntry exprEntry:
                    statements.Add(LowerObjectInitializerContentEntry(tempAccess, exprEntry, compilation));
                    break;
            }
        }

        // The value of the initializer expression is the initialized instance.
        statements.Add(new BoundExpressionStatement(tempAccess));

        return compilation.BoundNodeFactory.CreateBlockExpression(statements);
    }

    private BoundStatement LowerObjectInitializerAssignment(
        BoundExpression receiver,
        BoundObjectInitializerAssignmentEntry entry,
        Compilation compilation)
    {
        // First lower/visit the RHS so nested initializers are lowered too.
        var loweredValue = (BoundExpression)VisitExpression(entry.Value)!;

        var value = ApplyConversionIfNeeded(loweredValue, GetMemberType(entry.Member, compilation), compilation);

        switch (entry.Member)
        {
            case IPropertySymbol property:
                {
                    var assignment = compilation.BoundNodeFactory.CreatePropertyAssignmentExpression(receiver, property, value);
                    return new BoundExpressionStatement(assignment);
                }
            case IFieldSymbol field:
                {
                    var assignment = compilation.BoundNodeFactory.CreateFieldAssignmentExpression(receiver, field, value);
                    return new BoundExpressionStatement(assignment);
                }
            default:
                // Should not happen; binder only produces property/field assignments.
                return new BoundExpressionStatement(value);
        }
    }

    private BoundStatement LowerObjectInitializerContentEntry(
        BoundExpression receiver,
        BoundObjectInitializerExpressionEntry entry,
        Compilation compilation)
    {
        // Default lowering for content entries:
        // If the constructed object has an instance Add(T) method with a compatible parameter type,
        // lower the entry to: receiver.Add(<expr>)
        // Otherwise produce an error expression statement.

        // First lower/visit the expression so nested object initializers are lowered too.
        var loweredExpr = (BoundExpression)VisitExpression(entry.Expression)!;
        var exprType = loweredExpr.Type ?? compilation.ErrorTypeSymbol;

        var receiverType = receiver.Type ?? compilation.ErrorTypeSymbol;
        if (receiverType.TypeKind == TypeKind.Error)
            return new BoundExpressionStatement(loweredExpr);

        if (receiverType is not INamedTypeSymbol named)
            return new BoundExpressionStatement(CreateCannotAddError(exprType, receiverType, compilation));

        var add = FindBestAddMethod(named, exprType, compilation);
        if (add is null)
            return new BoundExpressionStatement(CreateCannotAddError(exprType, receiverType, compilation));

        var paramType = add.Parameters[0].Type;
        var arg = ApplyConversionIfNeeded(loweredExpr, paramType, compilation);

        // Emit receiver.Add(arg)
        // NOTE: Uses the bound node factory to construct the call expression.
        var call = compilation.BoundNodeFactory.CreateInvocationExpression(add, [arg], receiver);
        return new BoundExpressionStatement(call);
    }

    private static IMethodSymbol? FindBestAddMethod(INamedTypeSymbol receiverType, ITypeSymbol itemType, Compilation compilation)
    {
        // Pick the first Add method with a single parameter that has an implicit (or identity) conversion from itemType.
        foreach (var m in receiverType.GetMembers("Add").OfType<IMethodSymbol>())
        {
            if (m.Parameters.Length != 1)
                continue;

            if (m.IsStatic)
                continue;

            var paramType = m.Parameters[0].Type;
            var conv = compilation.ClassifyConversion(itemType, paramType);
            if (conv.Exists)
                return m;
        }

        return null;
    }

    private static BoundExpression CreateCannotAddError(ITypeSymbol itemType, ITypeSymbol receiverType, Compilation compilation)
    {
        // Lowerer doesn't have direct access to a diagnostics bag here.
        // Represent as an error expression so later phases can surface a meaningful diagnostic.
        return new BoundErrorExpression(
            type: compilation.ErrorTypeSymbol,
            reason: BoundExpressionReason.TypeMismatch);
    }

    private static ITypeSymbol GetMemberType(ISymbol member, Compilation compilation)
    {
        return member switch
        {
            IPropertySymbol p => p.Type,
            IFieldSymbol f => f.Type,
            _ => compilation.ErrorTypeSymbol
        };
    }
}
