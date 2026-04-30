using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;

using Raven.CodeAnalysis.Symbols;
using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis;

partial class BlockBinder
{
    private BoundExpression BindBuilderTrailingClosureIfNeeded(
        BoundExpression expression,
        IParameterSymbol parameter,
        SyntaxNode? syntaxNode)
    {
        if (expression is not BoundFunctionExpression lambda)
            return expression;

        if (!TryGetBuilderType(parameter, out var builderType))
            return expression;

        var parameterType = parameter.Type is NullableTypeSymbol nullable
            ? nullable.UnderlyingType
            : parameter.Type;

        if (parameterType is not INamedTypeSymbol delegateType || delegateType.TypeKind != TypeKind.Delegate)
            return expression;

        var invoke = delegateType.GetDelegateInvokeMethod();
        if (invoke is null)
            return expression;

        var body = RewriteBuilderBody(lambda.Body, builderType, invoke.ReturnType, syntaxNode ?? lambda.Unbound?.Syntax);
        if (body is null)
            return expression;

        if (lambda.Symbol is SourceLambdaSymbol sourceLambda)
        {
            sourceLambda.SetReturnType(invoke.ReturnType);
            sourceLambda.SetDelegateType(delegateType);
        }

        return new BoundFunctionExpression(
            lambda.Parameters,
            invoke.ReturnType,
            body,
            lambda.Symbol!,
            delegateType,
            lambda.CapturedVariables,
            lambda.CandidateDelegates);
    }

    private bool TryGetBuilderType(IParameterSymbol parameter, out INamedTypeSymbol builderType)
    {
        foreach (var attribute in parameter.GetAttributes())
        {
            var attributeClass = attribute.AttributeClass;
            if (attributeClass is null)
                continue;

            var name = attributeClass.Name;
            if (!string.Equals(name, "BuilderAttribute", StringComparison.Ordinal) &&
                !string.Equals(name, "Builder", StringComparison.Ordinal))
            {
                continue;
            }

            if (!attributeClass.TypeArguments.IsDefaultOrEmpty &&
                attributeClass.TypeArguments[0] is INamedTypeSymbol typeArgument)
            {
                builderType = typeArgument;
                return true;
            }

            if (attribute.ConstructorArguments.Length > 0 &&
                attribute.ConstructorArguments[0].Value is INamedTypeSymbol typedConstantType)
            {
                builderType = typedConstantType;
                return true;
            }
        }

        builderType = null!;
        return false;
    }

    private BoundExpression? RewriteBuilderBody(
        BoundExpression body,
        INamedTypeSymbol builderType,
        ITypeSymbol targetReturnType,
        SyntaxNode? syntaxNode)
    {
        var descriptor = BuilderDescriptor.Create(builderType);
        var componentType = descriptor.InferComponentType(Compilation, targetReturnType);

        if (componentType is null || componentType.TypeKind == TypeKind.Error)
            return body;

        var statements = body is BoundBlockExpression block
            ? block.Statements.ToImmutableArray()
            : ImmutableArray.Create<BoundStatement>(new BoundExpressionStatement(body));

        if (!TryRewriteBuilderStatements(statements, descriptor, componentType, syntaxNode, out var normalStatements, out var components))
            return body;

        var buildBlock = descriptor.ResolveBuildBlock(this, componentType, components, syntaxNode);
        if (buildBlock is null)
            return body;

        BoundExpression result = buildBlock;

        if (descriptor.ResolveBuildFinalResult(this, result, targetReturnType, syntaxNode) is { } finalResult)
            result = finalResult;
        else if (!SymbolEqualityComparer.Default.Equals(result.Type, targetReturnType) &&
                 IsAssignable(targetReturnType, result.Type!, out var conversion))
            result = ApplyConversion(result, targetReturnType, conversion, syntaxNode);

        normalStatements.Add(new BoundExpressionStatement(result));
        return new BoundBlockExpression(
            normalStatements,
            Compilation.GetSpecialType(SpecialType.System_Unit),
            body is BoundBlockExpression originalBlock ? originalBlock.LocalsToDispose : default);
    }

    private bool TryRewriteBuilderStatements(
        ImmutableArray<BoundStatement> statements,
        BuilderDescriptor descriptor,
        ITypeSymbol componentType,
        SyntaxNode? syntaxNode,
        out List<BoundStatement> normalStatements,
        out List<BoundExpression> components)
    {
        normalStatements = [];
        components = [];

        foreach (var statement in statements)
        {
            switch (statement)
            {
                case BoundExpressionStatement expressionStatement:
                    if (RewriteBuilderComponent(expressionStatement.Expression, descriptor, componentType, syntaxNode) is { } component)
                    {
                        components.Add(component);
                    }
                    else
                    {
                        return false;
                    }
                    break;

                case BoundReturnStatement { Expression: { } returnExpression }:
                    if (RewriteBuilderComponent(returnExpression, descriptor, componentType, syntaxNode) is { } returnComponent)
                    {
                        components.Add(returnComponent);
                    }
                    else
                    {
                        return false;
                    }
                    break;

                case BoundIfStatement ifStatement:
                    if (RewriteBuilderIfStatement(ifStatement, descriptor, componentType, syntaxNode) is { } ifComponent)
                    {
                        components.Add(ifComponent);
                    }
                    else
                    {
                        return false;
                    }
                    break;

                case BoundForStatement forStatement:
                    if (RewriteBuilderForStatement(forStatement, descriptor, componentType, syntaxNode) is { } forComponent)
                    {
                        components.Add(forComponent);
                    }
                    else
                    {
                        return false;
                    }
                    break;

                case BoundLocalDeclarationStatement:
                case BoundAssignmentStatement:
                    normalStatements.Add(statement);
                    break;

                default:
                    normalStatements.Add(statement);
                    break;
            }
        }

        return true;
    }

    private BoundExpression? RewriteBuilderIfStatement(
        BoundIfStatement statement,
        BuilderDescriptor descriptor,
        ITypeSymbol componentType,
        SyntaxNode? syntaxNode)
    {
        var thenComponent = RewriteBuilderStatementBody(statement.ThenNode, descriptor, componentType, syntaxNode);
        if (thenComponent is null)
            return null;

        if (statement.ElseNode is null)
        {
            var optionalInput = new BoundIfExpression(
                statement.Condition,
                thenComponent,
                new BoundDefaultValueExpression(componentType));

            return descriptor.ResolveBuilderCall(this, "BuildOptional", [optionalInput], syntaxNode);
        }

        var elseComponent = RewriteBuilderStatementBody(statement.ElseNode, descriptor, componentType, syntaxNode);
        if (elseComponent is null)
            return null;

        var first = descriptor.ResolveBuilderCall(this, "BuildEither", [thenComponent], syntaxNode, "first");
        var second = descriptor.ResolveBuilderCall(this, "BuildEither", [elseComponent], syntaxNode, "second");

        if (first is null || second is null)
            return null;

        return new BoundIfExpression(statement.Condition, first, second);
    }

    private BoundExpression? RewriteBuilderForStatement(
        BoundForStatement statement,
        BuilderDescriptor descriptor,
        ITypeSymbol componentType,
        SyntaxNode? syntaxNode)
    {
        if (syntaxNode is null)
            return null;

        var listDefinition = Compilation.GetTypeByMetadataName("System.Collections.Generic.List`1") as INamedTypeSymbol;
        if (listDefinition is null)
            return null;

        var listType = (INamedTypeSymbol)listDefinition.Construct(componentType);
        var constructor = listType.Constructors.FirstOrDefault(static ctor => !ctor.IsStatic && ctor.Parameters.Length == 0);
        var addMethod = listType.GetMembers("Add")
            .OfType<IMethodSymbol>()
            .FirstOrDefault(method =>
                !method.IsStatic &&
                method.Parameters.Length == 1 &&
                IsAssignable(componentType, method.Parameters[0].Type, out _));
        var toArrayMethod = listType.GetMembers("ToArray")
            .OfType<IMethodSymbol>()
            .FirstOrDefault(static method => !method.IsStatic && method.Parameters.Length == 0);

        if (constructor is null || addMethod is null || toArrayMethod is null)
            return null;

        var listLocal = CreateTempLocal("builderArray", listType, syntaxNode);
        var declaration = new BoundLocalDeclarationStatement([
            new BoundVariableDeclarator(listLocal, new BoundObjectCreationExpression(constructor, []))
        ]);

        var loopComponent = RewriteBuilderStatementBody(statement.Body, descriptor, componentType, syntaxNode);
        if (loopComponent is null)
            return null;

        var addInvocation = new BoundInvocationExpression(
            addMethod,
            [loopComponent],
            receiver: new BoundLocalAccess(listLocal));

        var loopBody = new BoundBlockStatement([new BoundExpressionStatement(addInvocation)]);
        var loop = new BoundForStatement(statement.Local, statement.Iteration, statement.Collection, loopBody);

        var toArrayInvocation = new BoundInvocationExpression(
            toArrayMethod,
            [],
            receiver: new BoundLocalAccess(listLocal));

        var buildArray = descriptor.ResolveBuilderCall(this, "BuildArray", [toArrayInvocation], syntaxNode);
        if (buildArray is null)
            return null;

        return new BoundBlockExpression(
            [
                declaration,
                loop,
                new BoundExpressionStatement(buildArray)
            ],
            Compilation.GetSpecialType(SpecialType.System_Unit));
    }

    private BoundExpression? RewriteBuilderStatementBody(
        BoundStatement statement,
        BuilderDescriptor descriptor,
        ITypeSymbol componentType,
        SyntaxNode? syntaxNode)
    {
        var statements = statement switch
        {
            BoundBlockStatement block => block.Statements.ToImmutableArray(),
            _ => ImmutableArray.Create(statement)
        };

        return TryRewriteBuilderStatements(statements, descriptor, componentType, syntaxNode, out var normalStatements, out var components)
            ? BuildBuilderBlockExpression(descriptor, componentType, normalStatements, components, syntaxNode)
            : null;
    }

    private BoundExpression? BuildBuilderBlockExpression(
        BuilderDescriptor descriptor,
        ITypeSymbol componentType,
        List<BoundStatement> normalStatements,
        List<BoundExpression> components,
        SyntaxNode? syntaxNode)
    {
        var buildBlock = descriptor.ResolveBuildBlock(this, componentType, components, syntaxNode);
        if (buildBlock is null)
            return null;

        if (normalStatements.Count == 0)
            return buildBlock;

        normalStatements.Add(new BoundExpressionStatement(buildBlock));
        return new BoundBlockExpression(normalStatements, Compilation.GetSpecialType(SpecialType.System_Unit));
    }

    private BoundExpression? RewriteBuilderComponent(
        BoundExpression expression,
        BuilderDescriptor descriptor,
        ITypeSymbol componentType,
        SyntaxNode? syntaxNode)
    {
        if (expression.Type is not null &&
            SymbolEqualityComparer.Default.Equals(expression.Type, componentType))
        {
            return expression;
        }

        if (expression.Type is not null &&
            IsAssignable(componentType, expression.Type, out var conversion))
        {
            return ApplyConversion(expression, componentType, conversion, syntaxNode);
        }

        return descriptor.ResolveBuilderCall(this, "BuildExpression", [expression], syntaxNode);
    }

    private sealed class BuilderDescriptor
    {
        private readonly INamedTypeSymbol _builderType;

        private BuilderDescriptor(INamedTypeSymbol builderType)
        {
            _builderType = builderType;
        }

        public static BuilderDescriptor Create(INamedTypeSymbol builderType) => new(builderType);

        public ITypeSymbol? InferComponentType(Compilation compilation, ITypeSymbol targetReturnType)
        {
            foreach (var method in GetStaticMethods("BuildBlock"))
            {
                if (method.ReturnType.TypeKind != TypeKind.Error)
                    return method.ReturnType;
            }

            foreach (var method in GetStaticMethods("BuildExpression"))
            {
                if (method.ReturnType.TypeKind != TypeKind.Error)
                    return method.ReturnType;
            }

            return targetReturnType.TypeKind == TypeKind.Error
                ? compilation.ErrorTypeSymbol
                : targetReturnType;
        }

        public BoundExpression? ResolveBuildBlock(
            BlockBinder binder,
            ITypeSymbol componentType,
            IReadOnlyList<BoundExpression> components,
            SyntaxNode? syntaxNode)
        {
            var arrayType = binder.Compilation.CreateArrayTypeSymbol(componentType);
            var arrayExpression = new BoundCollectionExpression(arrayType, components);

            return ResolveBuilderCall(binder, "BuildBlock", [arrayExpression], syntaxNode)
                ?? ResolveBuilderCall(binder, "BuildBlock", components, syntaxNode);
        }

        public BoundExpression? ResolveBuildFinalResult(
            BlockBinder binder,
            BoundExpression component,
            ITypeSymbol targetReturnType,
            SyntaxNode? syntaxNode)
        {
            var finalResult = ResolveBuilderCall(binder, "BuildFinalResult", [component], syntaxNode, reportMissing: false);
            if (finalResult is null)
                return null;

            if (finalResult.Type is not null &&
                SymbolEqualityComparer.Default.Equals(finalResult.Type, targetReturnType))
            {
                return finalResult;
            }

            if (finalResult.Type is not null &&
                binder.IsAssignable(targetReturnType, finalResult.Type, out var conversion))
            {
                return binder.ApplyConversion(finalResult, targetReturnType, conversion, syntaxNode);
            }

            return finalResult;
        }

        public BoundExpression? ResolveBuilderCall(
            BlockBinder binder,
            string methodName,
            IReadOnlyList<BoundExpression> expressions,
            SyntaxNode? syntaxNode,
            string? argumentName = null,
            bool reportMissing = true)
        {
            var methods = GetStaticMethods(methodName);
            if (methods.IsDefaultOrEmpty)
            {
                if (reportMissing)
                    binder._diagnostics.ReportNoOverloadForMethod("builder method", methodName, expressions.Count, syntaxNode?.GetLocation() ?? Location.None);
                return null;
            }

            var arguments = new BoundArgument[expressions.Count];
            for (var i = 0; i < expressions.Count; i++)
            {
                var name = i == 0 ? argumentName : null;
                arguments[i] = new BoundArgument(expressions[i], RefKind.None, name, syntaxNode);
            }

            var resolution = OverloadResolver.ResolveOverload(
                methods,
                arguments,
                binder.Compilation,
                binder: binder,
                callSyntax: syntaxNode);

            if (resolution.Method is null)
            {
                if (reportMissing)
                    binder._diagnostics.ReportNoOverloadForMethod("builder method", methodName, expressions.Count, syntaxNode?.GetLocation() ?? Location.None);
                return null;
            }

            var converted = binder.ConvertArguments(resolution.Method.Parameters, arguments);
            return new BoundInvocationExpression(resolution.Method, converted);
        }

        private ImmutableArray<IMethodSymbol> GetStaticMethods(string name)
            => _builderType.GetMembers(name)
                .OfType<IMethodSymbol>()
                .Where(static method => method.IsStatic)
                .ToImmutableArray();
    }
}
