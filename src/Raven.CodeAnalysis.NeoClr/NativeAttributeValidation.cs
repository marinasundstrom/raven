using NeoCLR.Metadata.Experimental.Introspection;
using NeoCLR.Metadata.Experimental.Model;

namespace Raven.CodeAnalysis.NeoClr;

// Validate before publishing native symbols so malformed data becomes RAVT003,
// rather than a late exception or an absent AttributeUsage policy during binding.
internal static class NativeAttributeValidation
{
    internal static void Validate(MetadataLoadContext context, AssemblyIdentity identity)
    {
        var assembly = context.Resolve(identity);
        Check(assembly.GetCustomAttributes());
        foreach (var module in assembly.GetModules())
        {
            foreach (var type in module.GetTypes())
            {
                Check(type.GetCustomAttributes());
                foreach (var field in type.GetFields()) Check(field.GetCustomAttributes());
                foreach (var property in type.GetProperties()) Check(property.GetCustomAttributes());
                foreach (var method in type.GetMethods().Concat(type.GetConstructors())) CheckMethod(method);
            }
            foreach (var function in module.GetFunctions()) CheckMethod(function);
        }

        void CheckMethod(MethodInfo method)
        {
            Check(method.GetCustomAttributes());
            foreach (var parameter in method.GetParameters()) Check(parameter.GetCustomAttributes());
        }

        bool Matches(NeoCLR.Metadata.Experimental.Introspection.TypeInfo type, CustomAttributeArgument argument) => argument.Type.Primitive is { } primitive
            ? type is PrimitiveTypeInfo p && p.Kind == primitive
            : argument.Type.ReferencedType is { } reference && type is NominalTypeInfo { IsEnum: true } &&
                ReferenceEquals(type, context.Resolve(reference));

        void Check(IReadOnlyList<CustomAttributeInfo> attributes)
        {
            foreach (var attribute in attributes)
            {
                var owner = attribute.GetAttributeType();
                var arguments = attribute.GetArguments();
                var constructors = owner.GetConstructors().Where(c => !c.IsStatic && c.Accessibility == MetadataAccessibility.Public &&
                    c.GetParameters().Count == arguments.Count && c.GetParameters().Zip(arguments).All(pair =>
                        pair.First.PassingMode == ParameterPassingMode.Value && Matches(pair.First.ParameterType, pair.Second))).ToArray();
                if (constructors.Length != 1)
                    throw new InvalidDataException("Missing or ambiguous native attribute constructor: " + owner.FullName);
                foreach (var argument in attribute.GetNamedArguments())
                {
                    var matches = argument.IsField
                        ? owner.GetFields().Count(f => f.Name == argument.MemberName && !f.IsStatic && !f.IsReadOnly && !f.IsLiteral &&
                            f.Accessibility == MetadataAccessibility.Public && Matches(f.FieldType, argument.TypedValue))
                        : owner.GetProperties().Count(p => p.Name == argument.MemberName && !p.IsStatic && p.IndexParameterTypes.Count == 0 &&
                            p.GetMethod?.Accessibility == MetadataAccessibility.Public && p.SetMethod?.Accessibility == MetadataAccessibility.Public &&
                            Matches(p.PropertyType, argument.TypedValue));
                    if (matches != 1)
                        throw new InvalidDataException("Invalid native named attribute member: " + argument.MemberName);
                }
            }
        }
    }
}
