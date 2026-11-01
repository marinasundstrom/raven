using System.Collections.Generic;
using System.Linq;

using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis;

partial class BlockBinder
{
    private bool IsIndexType(ITypeSymbol? type) => type is not null &&
        type.TypeKind != TypeKind.Error && SymbolEqualityComparer.Default.Equals(type, GetIndexType());

    private IPropertySymbol? ResolveIndexerWithImplicitIndex(
        BoundExpression receiver,
        BoundExpression[] arguments,
        SeparatedSyntaxList<ArgumentSyntax> syntax,
        bool requireSetter,
        out BoundExpression[] convertedArguments,
        bool requireGetter = false)
    {
        var indexer = ResolveIndexer(receiver.Type!, arguments, syntax, requireSetter, out convertedArguments, requireGetter);
        if (indexer is not null || arguments.Length != 1 || !IsIndexType(arguments[0].Type) ||
            FindImplicitIndexLength(receiver.Type!) is null || FindIndexOffsetMethod(arguments[0].Type) is null)
            return indexer;

        var offset = new BoundLiteralExpression(BoundLiteralExpressionKind.NumericLiteral, 0,
            Compilation.GetSpecialType(SpecialType.System_Int32));
        indexer = ResolveIndexer(receiver.Type!, [offset], syntax, requireSetter, out _, requireGetter);
        // Keep the authored Index until the location is captured; ordinary overloads
        // have already had priority over this language-defined fallback.
        convertedArguments = arguments;
        return indexer is { Parameters.Length: 1 } &&
            indexer.Parameters[0].Type.SpecialType == SpecialType.System_Int32 ? indexer : null;
    }

    private static IMethodSymbol? FindIndexOffsetMethod(ITypeSymbol? type) =>
        (type as INamedTypeSymbol)?.GetMembers("GetOffset").OfType<IMethodSymbol>().FirstOrDefault(m =>
            !m.IsStatic && m.DeclaredAccessibility == Accessibility.Public &&
            m.ReturnType.SpecialType == SpecialType.System_Int32 && m.Parameters.Length == 1 &&
            m.Parameters[0].Type.SpecialType == SpecialType.System_Int32);

    private IPropertySymbol? FindImplicitIndexLength(ITypeSymbol type)
    {
        foreach (var name in new[] { "Length", "Count" })
        {
            var types = type.TypeKind == TypeKind.Interface
                ? new[] { type }.Concat(type.AllInterfaces.OrderByDescending(i => i.AllInterfaces.Length))
                : new[] { type };
            foreach (var current in types)
            {
                var property = current.GetMembers(name).OfType<IPropertySymbol>().FirstOrDefault(p =>
                    !p.IsStatic && p.Parameters.Length == 0 && p.Type.SpecialType == SpecialType.System_Int32 &&
                    p.GetMethod is { } getter && IsSymbolAccessible(p) && IsSymbolAccessible(getter));
                if (property is not null)
                    return property;
            }
        }
        return null;
    }

    private BoundIndexerAccessExpression CreateIndexerAccess(
        BoundExpression receiver, BoundExpression[] arguments, IPropertySymbol indexer, SyntaxNode syntax)
    {
        receiver = ConvertExtensionIndexerReceiver(receiver, indexer, syntax);
        if (arguments.Length != 1 || !IsIndexType(arguments[0].Type) ||
            indexer.Parameters.Length != 1 || indexer.Parameters[0].Type.SpecialType != SpecialType.System_Int32 ||
            FindImplicitIndexLength(receiver.Type!) is not { GetMethod: { } lengthGetter } ||
            FindIndexOffsetMethod(arguments[0].Type) is not { } getOffset)
            return new BoundIndexerAccessExpression(receiver, arguments, indexer);

        var captures = new List<BoundStatement>();
        BoundExpression Capture(BoundExpression value)
        {
            var local = CreateTempLocal("indexLocation", value.Type!, syntax);
            captures.Add(new BoundLocalDeclarationStatement([new BoundVariableDeclarator(local, value)]));
            return new BoundLocalAccess(local);
        }

        // Preserve mutable struct storage, while value-producing expressions get
        // their own temporary. Capturing an address also stabilizes array/field paths.
        if (receiver.Type!.IsValueType && receiver is BoundLocalAccess or BoundParameterAccess or
            BoundFieldAccess or BoundArrayAccessExpression or BoundDereferenceExpression or BoundSelfExpression)
        {
            var type = receiver.Type;
            receiver = new BoundDereferenceExpression(Capture(new BoundAddressOfExpression(receiver)), type);
        }
        else
            receiver = Capture(receiver);

        var index = Capture(arguments[0]);
        var length = new BoundInvocationExpression(lengthGetter, [], receiver,
            requiresReceiverAddress: receiver.Type!.IsValueType);
        var offset = Capture(new BoundInvocationExpression(getOffset, [length], index, requiresReceiverAddress: true));
        return new BoundIndexerAccessExpression(receiver, [offset], indexer, captures);
    }
}
