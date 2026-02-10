

using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;

using Raven.CodeAnalysis.Symbols;
using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis;

class TopLevelBinder : BlockBinder
{
    private readonly CompilationUnitSyntax _compilationUnit;
    private readonly SourceMethodSymbol _scriptMethod;

    public TopLevelBinder(
        Binder parent,
        SemanticModel semanticModel,
        SourceMethodSymbol scriptMethod,
        SynthesizedMainMethodSymbol entryPointMethod,
        CompilationUnitSyntax compilationUnit)
        : base(scriptMethod, parent)
    {
        SemanticModel = semanticModel;
        _compilationUnit = compilationUnit;
        _scriptMethod = scriptMethod;
        EntryPointMethod = entryPointMethod;
    }

    public override SemanticModel SemanticModel { get; }

    public SynthesizedMainMethodSymbol EntryPointMethod { get; }

    public SourceMethodSymbol ScriptMethod => _scriptMethod;

    public IMethodSymbol MainMethod => _scriptMethod;

    public void BindGlobalStatements(IEnumerable<GlobalStatementSyntax> statements)
    {
        var globalStatements = new List<GlobalStatementSyntax>();
        foreach (var stmt in statements)
            globalStatements.Add(stmt);

        ApplyInferredTopLevelReturnTypes(globalStatements);

        // Declare all functions first so they are available to subsequent statements
        foreach (var stmt in globalStatements)
        {
            if (stmt.Statement is FunctionStatementSyntax localFunc)
            {
                var binder = SemanticModel.GetBinder(localFunc, this);
                if (binder is FunctionBinder lfBinder)
                {
                    var symbol = lfBinder.GetMethodSymbol();
                    if (_functions.TryGetValue(symbol.Name, out var existing) && HaveSameSignature(existing, symbol))
                        _diagnostics.ReportFunctionAlreadyDefined(symbol.Name, localFunc.Identifier.GetLocation());
                    else
                        DeclareFunction(symbol);
                }
            }
        }

        // Bind each statement
        var boundStatements = new List<BoundStatement>(globalStatements.Count);

        foreach (var stmt in globalStatements)
        {
            if (stmt.Statement is FunctionStatementSyntax function)
                boundStatements.Add(BindStatement(function));
            else
                boundStatements.Add(BindStatement(stmt.Statement));
        }

        var localsToDispose = ImmutableArray.CreateBuilder<ILocalSymbol>();
        foreach (var stmt in globalStatements)
        {
            if (stmt.Statement is UseDeclarationStatementSyntax useDeclaration)
            {
                foreach (var declarator in useDeclaration.Declaration.Declarators)
                {
                    if (SemanticModel.GetDeclaredSymbol(declarator) is ILocalSymbol local)
                        localsToDispose.Add(local);
                }
            }
        }

        var block = new BoundBlockStatement(boundStatements, localsToDispose.ToImmutable());
        CacheBoundNode(_compilationUnit, block);
    }

    private void ApplyInferredTopLevelReturnTypes(IReadOnlyList<GlobalStatementSyntax> globalStatements)
    {
        if (!TryInferTopLevelResultType(globalStatements, out var inferredResultType))
            return;

        if (_scriptMethod.IsAsync)
        {
            _scriptMethod.SetReturnType(AsyncReturnTypeUtilities.InferAsyncReturnType(Compilation, inferredResultType));
            EntryPointMethod.SetReturnType(EntryPointSignature.ResolveBridgeReturnType(Compilation, _scriptMethod.ReturnType));
            return;
        }

        _scriptMethod.SetReturnType(inferredResultType);
    }

    private bool TryInferTopLevelResultType(
        IReadOnlyList<GlobalStatementSyntax> globalStatements,
        out ITypeSymbol inferredResultType)
    {
        inferredResultType = null!;

        ITypeSymbol? okType = null;
        ITypeSymbol? errorType = null;
        var foundResultReturn = false;

        using (_diagnostics.CreateNonReportingScope())
        {
            foreach (var statement in globalStatements)
            {
                CollectResultReturnTypes(statement.Statement, ref okType, ref errorType, ref foundResultReturn);
            }
        }

        if (!foundResultReturn)
            return false;

        okType ??= Compilation.GetSpecialType(SpecialType.System_Unit);
        errorType ??= Compilation.GetSpecialType(SpecialType.System_Object);

        if (okType.TypeKind == TypeKind.Error || errorType.TypeKind == TypeKind.Error)
            return false;

        var resultDefinition = LookupType("Result") as INamedTypeSymbol;
        if (resultDefinition is null || resultDefinition.Arity != 2 || resultDefinition.IsUnboundGenericType)
            return false;

        inferredResultType = resultDefinition.Construct(okType, errorType);
        return inferredResultType.TypeKind != TypeKind.Error;
    }

    private void CollectResultReturnTypes(
        SyntaxNode node,
        ref ITypeSymbol? okType,
        ref ITypeSymbol? errorType,
        ref bool foundResultReturn)
    {
        if (node is FunctionStatementSyntax or LambdaExpressionSyntax)
            return;

        if (node is ReturnStatementSyntax returnStatement)
        {
            if (TryGetResultReturnPayload(returnStatement.Expression, out var isOkCase, out var payloadType))
            {
                foundResultReturn = true;
                if (payloadType is not null && payloadType.TypeKind != TypeKind.Error)
                {
                    if (isOkCase)
                        okType = MergeInferenceType(okType, payloadType);
                    else
                        errorType = MergeInferenceType(errorType, payloadType);
                }
            }

            return;
        }

        if (node is ExpressionStatementSyntax expressionStatement)
        {
            if (TryGetResultReturnPayload(expressionStatement.Expression, out var isOkCase, out var payloadType))
            {
                foundResultReturn = true;
                if (payloadType is not null && payloadType.TypeKind != TypeKind.Error)
                {
                    if (isOkCase)
                        okType = MergeInferenceType(okType, payloadType);
                    else
                        errorType = MergeInferenceType(errorType, payloadType);
                }
            }
        }

        foreach (var child in node.ChildNodes())
            CollectResultReturnTypes(child, ref okType, ref errorType, ref foundResultReturn);
    }

    private bool TryGetResultReturnPayload(
        ExpressionSyntax? expression,
        out bool isOkCase,
        out ITypeSymbol? payloadType)
    {
        isOkCase = false;
        payloadType = null;

        if (expression is null)
            return false;

        if (expression is MemberBindingExpressionSyntax bareCase)
        {
            var name = bareCase.Name.Identifier.ValueText;
            if (!string.Equals(name, "Ok", StringComparison.Ordinal) &&
                !string.Equals(name, "Error", StringComparison.Ordinal))
            {
                return false;
            }

            isOkCase = string.Equals(name, "Ok", StringComparison.Ordinal);
            payloadType = Compilation.GetSpecialType(SpecialType.System_Unit);
            return true;
        }

        if (expression is not InvocationExpressionSyntax invocation ||
            invocation.Expression is not MemberBindingExpressionSyntax memberBinding)
        {
            return false;
        }

        var memberName = memberBinding.Name.Identifier.ValueText;
        if (!string.Equals(memberName, "Ok", StringComparison.Ordinal) &&
            !string.Equals(memberName, "Error", StringComparison.Ordinal))
        {
            return false;
        }

        isOkCase = string.Equals(memberName, "Ok", StringComparison.Ordinal);

        if (invocation.ArgumentList.Arguments.Count == 0)
        {
            payloadType = Compilation.GetSpecialType(SpecialType.System_Unit);
            return true;
        }

        var payloadExpression = BindExpression(invocation.ArgumentList.Arguments[0].Expression, allowReturn: false);
        payloadType = payloadExpression.Type?.GetPlainType();
        return true;
    }

    private static ITypeSymbol? MergeInferenceType(ITypeSymbol? current, ITypeSymbol incoming)
    {
        incoming = TypeSymbolNormalization.NormalizeForInference(incoming);

        if (current is null)
            return incoming;

        if (SymbolEqualityComparer.Default.Equals(current, incoming))
            return current;

        return TypeSymbolNormalization.NormalizeUnion([current, incoming]);
    }

    public override ISymbol? LookupSymbol(string name)
    {
        var paramSymbol = _scriptMethod.Parameters.FirstOrDefault(p => p.Name == name);
        if (paramSymbol is not null)
            return paramSymbol;

        var parentSymbol = base.LookupSymbol(name);
        if (parentSymbol != null)
            return parentSymbol;

        var currentNamespace = CurrentNamespace;
        if (currentNamespace is not null)
        {
            var namespaceMember = currentNamespace.GetMembers(name).FirstOrDefault();
            if (namespaceMember is not null)
                return namespaceMember;
        }

        return Compilation.GlobalNamespace.GetMembers(name).FirstOrDefault();
    }

    public IEnumerable<IParameterSymbol> GetParameters()
    {
        return _scriptMethod.Parameters;
    }

    public void DeclareFunction(IMethodSymbol symbol)
    {
        _functions[symbol.Name] = symbol;
    }
}
