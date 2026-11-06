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

    private BoundObjectCreationExpression? BindInitializer(ArgumentListSyntax argumentList, NamedTypeSymbol? targetType)
    {
        if (targetType is null)
            return null;

        var constructors = targetType.Constructors.Where(c => !c.IsStatic).ToImmutableArray();
        var argumentArray = BindInvocationArgumentsWithCandidateTargetTypes(
            constructors, argumentList.Arguments, out var hasErrors);

        for (var i = 0; i < argumentArray.Length; i++)
        {
            var instanceUse = new InstanceUseWalker();
            instanceUse.Visit(argumentArray[i].Expression);
            if (instanceUse.Found)
            {
                _diagnostics.ReportConstructorInitializerCannotAccessInstance(argumentList.Arguments[i].GetLocation());
                hasErrors = true;
            }
        }

        if (hasErrors)
            return null;

        var resolution = OverloadResolver.ResolveOverload(constructors, argumentArray, Compilation, binder: this, callSyntax: argumentList);

        if (!resolution.Success)
        {
            if (resolution.IsAmbiguous)
            {
                _diagnostics.ReportCallIsAmbiguous(targetType.Name, resolution.AmbiguousCandidates, argumentList.GetLocation());
                return null;
            }

            var matchingByArity = constructors
                .Where(c => SupportsArgumentCount(c.Parameters, argumentArray.Length))
                .ToImmutableArray();
            if (matchingByArity.Length == 1)
            {
                var candidate = matchingByArity[0];
                var converted = ConvertArguments(candidate.Parameters, argumentArray, argumentList);
                if (converted.Any(static argument => argument is BoundErrorExpression))
                    return null;
            }

            _diagnostics.ReportNoOverloadForMethod("constructor for type", targetType.Name, argumentArray.Length, argumentList.GetLocation());
            return null;
        }

        var constructor = resolution.Method!;
        constructor = EnsureConstructedConstructor(constructor, targetType);
        var convertedArguments = ConvertArguments(constructor.Parameters, argumentArray, argumentList);
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
