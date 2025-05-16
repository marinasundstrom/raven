



using Raven.CodeAnalysis.Syntax;

namespace Raven.CodeAnalysis;

public class Diagnostic
{
    private readonly object[]? _messageArgs;

    public DiagnosticDescriptor Descriptor { get; }

    public Location Location { get; }

    public object[] GetMessageArgs() => _messageArgs ?? [];

    public Diagnostic(DiagnosticDescriptor descriptor, Location location, object[]? messageArgs)
    {
        Descriptor = descriptor;
        Location = location;
        _messageArgs = messageArgs;
    }

    public override string ToString() => GetDescription();

    public static Diagnostic Create(DiagnosticDescriptor descriptor, Location location, params object[]? messageArgs)
    {
        return new Diagnostic(descriptor, location, messageArgs);
    }

    public string GetDescription() => $"{Descriptor.DefaultSeverity.ToString().ToLower()} {Descriptor.Id}: {string.Format(Descriptor.MessageFormat, _messageArgs is not null ? ProcessArgs(_messageArgs).ToArray() : [])}";

    public string GetMessage() => string.Format(Descriptor.MessageFormat, _messageArgs is not null ? ProcessArgs(_messageArgs).ToArray() : []);

    private IEnumerable<object> ProcessArgs(object[]? messageArgs)
    {
        return messageArgs?.Select(arg =>
        {
            if (arg is ISymbol symbol)
            {
                return symbol.ToDisplayString(SymbolDisplayFormat.MinimallyQualifiedFormat);
            }

            return arg;
        }) ?? [];
    }

    internal static Diagnostic Create(object memberAccessOnVoid, Location location)
    {
        throw new NotImplementedException();
    }

    public override bool Equals(object? obj)
    {
        if (obj is not Diagnostic other)
            return false;

        if (!Descriptor.Equals(other.Descriptor))
            return false;

        if (!Location.Equals(other.Location))
            return false;

        var args1 = GetMessageArgs();
        var args2 = other.GetMessageArgs();

        /*
        if (!args1.SequenceEqual(args2))
            return false;
        */

        return true;
    }

    public override int GetHashCode()
    {
        var hash = new HashCode();
        hash.Add(Descriptor);
        hash.Add(Location);

        /*
        foreach (var arg in GetMessageArgs())
            hash.Add(arg);
        */

        return hash.ToHashCode();
    }
}
