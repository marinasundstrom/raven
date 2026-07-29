using System.Collections.Immutable;

using Newtonsoft.Json.Linq;

using Raven.CodeAnalysis;

namespace Raven.LanguageServer;

internal static class RavenDiagnosticData
{
    private const int CurrentVersion = 1;
    private const string VersionProperty = "version";
    private const string MessageFormatProperty = "messageFormat";
    private const string MessageArgumentsProperty = "messageArguments";
    private const string PropertiesProperty = "properties";

    public static JToken Create(Diagnostic diagnostic)
    {
        var messageArguments = new JArray(
            diagnostic.GetMessageArgs().Select(SerializeMessageArgument));
        var properties = new JObject(
            diagnostic.Properties.Select(property =>
                new JProperty(property.Key, property.Value is null
                    ? JValue.CreateNull()
                    : new JValue(property.Value))));

        return new JObject
        {
            [VersionProperty] = CurrentVersion,
            [MessageFormatProperty] = diagnostic.Descriptor.MessageFormat.ToString(),
            [MessageArgumentsProperty] = messageArguments,
            [PropertiesProperty] = properties
        };
    }

    public static bool TryRead(
        JToken? data,
        out string messageFormat,
        out object[] messageArguments,
        out ImmutableDictionary<string, string?> properties)
    {
        messageFormat = string.Empty;
        messageArguments = [];
        properties = ImmutableDictionary<string, string?>.Empty;

        if (data is not JObject payload ||
            payload.Value<int?>(VersionProperty) != CurrentVersion ||
            payload.Value<string>(MessageFormatProperty) is not { Length: > 0 } format)
        {
            return false;
        }

        messageFormat = format;
        if (payload[MessageArgumentsProperty] is JArray arguments)
        {
            messageArguments = arguments
                .Select(DeserializeMessageArgument)
                .ToArray();
        }

        if (payload[PropertiesProperty] is JObject diagnosticProperties)
        {
            properties = diagnosticProperties.Properties()
                .ToImmutableDictionary(
                    property => property.Name,
                    property => property.Value.Type == JTokenType.Null
                        ? null
                        : property.Value.Value<string>(),
                    StringComparer.Ordinal);
        }

        return true;
    }

    private static JToken SerializeMessageArgument(object? argument)
    {
        if (argument is null)
            return JValue.CreateNull();

        if (argument is ISymbol symbol)
        {
            return new JValue(
                symbol.ToDisplayString(SymbolDisplayFormat.MinimallyQualifiedFormat));
        }

        return argument switch
        {
            string value => new JValue(value),
            bool value => new JValue(value),
            byte value => new JValue(value),
            sbyte value => new JValue(value),
            short value => new JValue(value),
            ushort value => new JValue(value),
            int value => new JValue(value),
            uint value => new JValue(value),
            long value => new JValue(value),
            ulong value => new JValue(value),
            float value => new JValue(value),
            double value => new JValue(value),
            decimal value => new JValue(value),
            _ => new JValue(argument.ToString())
        };
    }

    private static object DeserializeMessageArgument(JToken argument)
        => argument.Type switch
        {
            JTokenType.Null => string.Empty,
            JTokenType.Boolean => argument.Value<bool>(),
            JTokenType.Integer => argument.Value<long>(),
            JTokenType.Float => argument.Value<double>(),
            _ => argument.Value<string>() ?? string.Empty
        };
}
