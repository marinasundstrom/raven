using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;

using Raven.CodeAnalysis.Symbols;
using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis;

internal sealed class ConstructorInitializerBinder : MethodBodyBinder
{
    private readonly SourceMethodSymbol _constructor;

    public ConstructorInitializerBinder(SourceMethodSymbol constructor, Binder parent)
        : base(constructor, parent)
    {
        _constructor = constructor;
    }

    public BoundObjectCreationExpression? Bind(ConstructorInitializerSyntax initializerSyntax)
    {
        return initializerSyntax switch
        {
            BaseConstructorInitializerSyntax baseInitializer => BindBaseInitializer(baseInitializer.ArgumentList),
            SelfConstructorInitializerSyntax selfInitializer => BindInitializer(selfInitializer.ArgumentList, _constructor.ContainingType),
            _ => null
        };
    }

    /// <summary>
    /// Binds the base constructor call expressed as a <see cref="PrimaryConstructorBaseTypeSyntax"/>
    /// argument list (e.g. <c>record Add(Expr Left, Expr Right) : BinaryExpr(Left, Right)</c>).
    /// </summary>
    public BoundObjectCreationExpression? BindFromPrimaryConstructorBase(ArgumentListSyntax argumentList)
        => BindBaseInitializer(argumentList);

    private BoundObjectCreationExpression? BindBaseInitializer(ArgumentListSyntax argumentList)
        => BindInitializer(argumentList, _constructor.ContainingType?.BaseType);

    private BoundObjectCreationExpression? BindInitializer(ArgumentListSyntax argumentList, INamedTypeSymbol? targetType)
    {
        if (targetType is null)
            return null;

        var boundArguments = new List<BoundArgument>();
        var hasErrors = false;

        foreach (var argument in argumentList.Arguments)
        {
            var boundArgument = BindExpression(argument.Expression);
            if (boundArgument is BoundErrorExpression)
                hasErrors = true;
            var instanceUse = new InstanceUseWalker();
            instanceUse.Visit(boundArgument);
            if (instanceUse.Found)
            {
                _diagnostics.ReportConstructorInitializerCannotAccessInstance(argument.GetLocation());
                hasErrors = true;
            }

            var name = argument.NameColon?.Name.Identifier.ValueText;
            if (string.IsNullOrEmpty(name))
                name = null;
            var isSpread = argument.DotDotDotToken.Kind == SyntaxKind.DotDotDotToken;
            boundArguments.Add(new BoundArgument(boundArgument, RefKind.None, name, argument, isSpread));
        }

        if (hasErrors)
            return null;

        var constructors = targetType.Constructors.Where(c => !c.IsStatic).ToImmutableArray();
        var argumentArray = boundArguments.ToArray();
        var resolution = OverloadResolver.ResolveOverload(constructors, argumentArray, Compilation, binder: this, callSyntax: argumentList);

        if (!resolution.Success)
        {
            if (resolution.IsAmbiguous)
            {
                _diagnostics.ReportCallIsAmbiguous(targetType.Name, resolution.AmbiguousCandidates, argumentList.GetLocation());
                return null;
            }

            var matchingByArity = constructors
                .Where(c => SupportsArgumentCount(c.Parameters, boundArguments.Count))
                .ToImmutableArray();
            if (matchingByArity.Length == 1)
            {
                var candidate = matchingByArity[0];
                var converted = ConvertArguments(candidate.Parameters, argumentArray);
                if (converted.Any(static argument => argument is BoundErrorExpression))
                    return null;
            }

            _diagnostics.ReportNoOverloadForMethod("constructor for type", targetType.Name, boundArguments.Count, argumentList.GetLocation());
            return null;
        }

        var constructor = resolution.Method!;
        constructor = EnsureConstructedConstructor(constructor, targetType);
        var convertedArguments = ConvertArguments(constructor.Parameters, argumentArray);
        if (convertedArguments.Any(static argument => argument is BoundErrorExpression))
            return null;

        var receiver = new BoundSelfExpression(_constructor.ContainingType!);

        return new BoundObjectCreationExpression(constructor, convertedArguments, receiver);
    }

    private sealed class InstanceUseWalker : BoundTreeWalker
    {
        internal bool Found { get; private set; }
        public override void VisitSelfExpression(BoundSelfExpression self) => Found = true;
        public override void VisitBaseExpression(BoundBaseExpression node) => Found = true;
        public override void VisitPropertyAccess(BoundPropertyAccess node)
        {
            if (!node.Property.IsStatic) Found = true;
        }
        public override void VisitFieldAccess(BoundFieldAccess node)
        {
            if (!node.Field.IsStatic && node.Receiver is null) Found = true;
            base.VisitFieldAccess(node);
        }
        public override void VisitInvocationExpression(BoundInvocationExpression node)
        {
            if (!node.Method.IsStatic && node.Receiver is null) Found = true;
            base.VisitInvocationExpression(node);
        }
    }
}
