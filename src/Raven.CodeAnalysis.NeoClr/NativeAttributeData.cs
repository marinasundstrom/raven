using System.Collections.Immutable;

using NeoCLR.Metadata.Experimental.Introspection;
using NeoCLR.Metadata.Experimental.Model;

namespace Raven.CodeAnalysis.NeoClr;

// Convert declaration data only. Constructors and named assignments never execute.
internal static class NativeAttributeData
{
    internal static ImmutableArray<AttributeData> Read(Compilation compilation, NativeModuleSymbol module,
        IReadOnlyList<CustomAttributeInfo> attributes)
    {
        return [.. attributes.Select(ReadOne)];

        AttributeData ReadOne(CustomAttributeInfo attribute)
        {
            var owner = (INamedTypeSymbol)module.MapView(attribute.GetAttributeType());
            var arguments = attribute.GetArguments().Select(Constant).ToImmutableArray();
            var constructors = owner.Constructors.Where(c => !c.IsStatic && c.DeclaredAccessibility == Accessibility.Public &&
                c.Parameters.Length == arguments.Length && c.Parameters.Zip(arguments).All(pair =>
                    pair.First.RefKind == RefKind.None && SymbolEqualityComparer.Default.Equals(pair.First.Type, pair.Second.Type))).ToArray();
            if (constructors.Length != 1)
                throw new InvalidDataException("Missing or ambiguous native attribute constructor: " + owner.ToFullyQualifiedMetadataName());
            var named = attribute.GetNamedArguments().Select(argument =>
            {
                var constant = Constant(argument.TypedValue);
                var matches = owner.GetMembers(argument.MemberName).Where(member => argument.IsField
                    ? member is IFieldSymbol { IsStatic: false, IsReadOnly: false, IsConst: false, DeclaredAccessibility: Accessibility.Public } field &&
                        SymbolEqualityComparer.Default.Equals(field.Type, constant.Type)
                    : member is IPropertySymbol
                    {
                        IsStatic: false, IsIndexer: false,
                        GetMethod.DeclaredAccessibility: Accessibility.Public, SetMethod.DeclaredAccessibility: Accessibility.Public
                    } property &&
                        SymbolEqualityComparer.Default.Equals(property.Type, constant.Type)).ToArray();
                if (matches.Length != 1)
                    throw new InvalidDataException("Invalid native named attribute member: " + argument.MemberName);
                return new KeyValuePair<string, TypedConstant>(argument.MemberName, constant);
            }).ToImmutableArray();
            return new AttributeData(owner, constructors[0], arguments, named, null);
        }

        TypedConstant Constant(CustomAttributeArgument argument)
        {
            var type = argument.Type.Primitive is { } primitive
                ? compilation.GetSpecialType(primitive switch
                {
                    PrimitiveType.String => SpecialType.System_String,
                    PrimitiveType.Int32 => SpecialType.System_Int32,
                    PrimitiveType.Boolean => SpecialType.System_Boolean,
                    _ => throw new InvalidDataException("Unsupported native attribute primitive")
                })
                : argument.Type.ReferencedType is { } reference
                    ? module.MapView(NativeMetadataContext.For(compilation).Resolve(reference))
                    : throw new InvalidDataException("Unsupported native attribute type identity");
            if (argument.Type.Primitive is null && type.TypeKind != TypeKind.Enum)
                throw new InvalidDataException("Native attribute nominal arguments require an enum");
            return type.TypeKind == TypeKind.Enum ? TypedConstant.CreateEnum(type, argument.Value)
                : TypedConstant.CreatePrimitive(type, argument.Value);
        }
    }
}
