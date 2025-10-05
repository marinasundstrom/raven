namespace System.Reflection2;

using System;
using System.Collections.Generic;
using System.Globalization;
using System.Reflection;

internal static class MetadataConstantDecoder
{
    private const string DecimalConstantAttributeName = "System.Runtime.CompilerServices.DecimalConstantAttribute";
    private const string DateTimeConstantAttributeName = "System.Runtime.CompilerServices.DateTimeConstantAttribute";

    public static bool IsEnumLike(Type type)
    {
        if (type.IsEnum)
        {
            return true;
        }

        try
        {
            var baseType = type.BaseType;
            return baseType is not null && string.Equals(baseType.FullName, typeof(Enum).FullName, StringComparison.Ordinal);
        }
        catch (NotSupportedException)
        {
            return false;
        }
    }

    public static Type? TryResolveRuntimeEnumType(Type type)
    {
        if (!IsEnumLike(type))
        {
            return null;
        }

        if (type.IsEnum && type.GetType().Assembly == typeof(Type).Assembly)
        {
            return type;
        }

        var candidates = new string?[]
        {
            type.AssemblyQualifiedName,
            type.FullName,
        };

        foreach (var candidate in candidates)
        {
            if (string.IsNullOrEmpty(candidate))
            {
                continue;
            }

            var runtimeType = Type.GetType(candidate, throwOnError: false);
            if (runtimeType is not null && runtimeType.IsEnum)
            {
                return runtimeType;
            }
        }

        var fullName = type.FullName;
        if (fullName is null)
        {
            return null;
        }

        foreach (var assembly in AppDomain.CurrentDomain.GetAssemblies())
        {
            var runtimeType = assembly.GetType(fullName, throwOnError: false, ignoreCase: false);
            if (runtimeType is not null && runtimeType.IsEnum)
            {
                return runtimeType;
            }
        }

        return null;
    }

    public static bool TryDecodeDecimalConstant(IList<CustomAttributeData> attributes, out decimal value)
    {
        foreach (var attribute in attributes)
        {
            if (!string.Equals(attribute.AttributeType.FullName, DecimalConstantAttributeName, StringComparison.Ordinal))
            {
                continue;
            }

            var args = attribute.ConstructorArguments;
            if (args.Count != 5)
            {
                continue;
            }

            var scale = Convert.ToByte(args[0].Value, CultureInfo.InvariantCulture);
            var sign = Convert.ToByte(args[1].Value, CultureInfo.InvariantCulture);
            var high = ToInt32Unchecked(args[2].Value!);
            var mid = ToInt32Unchecked(args[3].Value!);
            var low = ToInt32Unchecked(args[4].Value!);

            value = new decimal(low, mid, high, sign != 0, scale);
            return true;
        }

        value = default;
        return false;
    }

    public static bool TryDecodeDateTimeConstant(IList<CustomAttributeData> attributes, out DateTime value)
    {
        foreach (var attribute in attributes)
        {
            if (!string.Equals(attribute.AttributeType.FullName, DateTimeConstantAttributeName, StringComparison.Ordinal))
            {
                continue;
            }

            var args = attribute.ConstructorArguments;
            if (args.Count != 1)
            {
                continue;
            }

            var ticks = Convert.ToInt64(args[0].Value, CultureInfo.InvariantCulture);
            value = new DateTime(ticks, DateTimeKind.Unspecified);
            return true;
        }

        value = default;
        return false;
    }

    private static int ToInt32Unchecked(object value)
    {
        if (value is null)
        {
            return 0;
        }

        return value switch
        {
            int i => i,
            uint u => unchecked((int)u),
            short s => s,
            ushort us => us,
            byte b => b,
            sbyte sb => sb,
            _ => Convert.ToInt32(value, CultureInfo.InvariantCulture),
        };
    }
}
