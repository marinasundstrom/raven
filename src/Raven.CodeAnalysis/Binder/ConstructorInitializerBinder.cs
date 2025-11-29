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

        var boundArguments = new List<BoundArgument>();
        var hasErrors = false;

        foreach (var argument in initializerSyntax.ArgumentList.Arguments)
        {
            var boundArgument = BindExpression(argument.Expression);
            if (boundArgument is BoundErrorExpression)
                hasErrors = true;

            var name = argument.NameColon?.Name.Identifier.ValueText;
            if (string.IsNullOrEmpty(name))
                name = null;
            boundArguments.Add(new BoundArgument(boundArgument, RefKind.None, name, argument));
        }

        if (hasErrors)
            return null;

        var constructors = baseType.Constructors.Where(c => !c.IsStatic).ToImmutableArray();
        var argumentArray = boundArguments.ToArray();
        var resolution = OverloadResolver.ResolveOverload(constructors, argumentArray, Compilation, callSyntax: initializerSyntax);

        if (!resolution.Success)
        {
            if (resolution.IsAmbiguous)
            {
                _diagnostics.ReportCallIsAmbiguous(baseType.Name, resolution.AmbiguousCandidates, initializerSyntax.ArgumentList.GetLocation());
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

            _diagnostics.ReportNoOverloadForMethod(baseType.Name, boundArguments.Count, initializerSyntax.ArgumentList.GetLocation());
            return null;
        }

        var constructor = resolution.Method!;
        constructor = EnsureConstructedConstructor(constructor, baseType);
        var convertedArguments = ConvertArguments(constructor.Parameters, argumentArray);
        if (convertedArguments.Any(static argument => argument is BoundErrorExpression))
            return null;

        var receiver = new BoundSelfExpression(_constructor.ContainingType!);

        return new BoundObjectCreationExpression(constructor, convertedArguments, receiver);
    }
}
