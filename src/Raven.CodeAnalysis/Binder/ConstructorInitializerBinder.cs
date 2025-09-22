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
            BaseConstructorInitializerSyntax baseInitializer => BindBaseInitializer(baseInitializer),
            _ => null
        };
    }

    private BoundObjectCreationExpression? BindBaseInitializer(BaseConstructorInitializerSyntax initializerSyntax)
    {
        var baseType = _constructor.ContainingType?.BaseType;
        if (baseType is null)
            return null;

        var boundArguments = new List<BoundExpression>();
        var hasErrors = false;

        foreach (var argument in initializerSyntax.ArgumentList.Arguments)
        {
            var boundArgument = BindExpression(argument.Expression);
            if (boundArgument is BoundErrorExpression)
                hasErrors = true;

            boundArguments.Add(boundArgument);
        }

        if (hasErrors)
            return null;

        var constructors = baseType.Constructors.Where(c => !c.IsStatic).ToImmutableArray();
        var resolution = OverloadResolver.ResolveOverload(constructors, boundArguments.ToArray(), Compilation);

        if (!resolution.Success)
        {
            if (resolution.IsAmbiguous)
            {
                _diagnostics.ReportCallIsAmbiguous(baseType.Name, resolution.AmbiguousCandidates, initializerSyntax.ArgumentList.GetLocation());
                return null;
            }

            var matchingByArity = constructors.Where(c => c.Parameters.Length == boundArguments.Count).ToImmutableArray();
            if (matchingByArity.Length == 1)
            {
                var candidate = matchingByArity[0];
                var converted = ConvertArguments(candidate.Parameters, boundArguments, initializerSyntax.ArgumentList.Arguments);
                if (converted.Any(static argument => argument is BoundErrorExpression))
                    return null;
            }

            _diagnostics.ReportNoOverloadForMethod(baseType.Name, boundArguments.Count, initializerSyntax.ArgumentList.GetLocation());
            return null;
        }

        var constructor = resolution.Method!;
        var convertedArguments = ConvertArguments(constructor.Parameters, boundArguments, initializerSyntax.ArgumentList.Arguments);
        if (convertedArguments.Any(static argument => argument is BoundErrorExpression))
            return null;

        var receiver = new BoundSelfExpression(_constructor.ContainingType!);

        return new BoundObjectCreationExpression(constructor, convertedArguments, receiver);
    }
}
