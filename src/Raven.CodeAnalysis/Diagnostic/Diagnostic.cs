



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

    public override string ToString() => GetMessage();

    public static Diagnostic Create(DiagnosticDescriptor descriptor, Location location, params object[]? messageArgs)
    {
        return new Diagnostic(descriptor, location, messageArgs);
    }

    public string GetMessage() => $"{Descriptor.DefaultSeverity.ToString().ToLower()} {Descriptor.Id}: {string.Format(Descriptor.MessageFormat, _messageArgs ?? [])}";

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

        if (args1.Length != args2.Length)
            return false;

        for (int i = 0; i < args1.Length; i++)
        {
            if (!Equals(args1[i], args2[i]))
                return false;
        }

        return true;
    }

    public override int GetHashCode()
    {
        var hash = new HashCode();
        hash.Add(Descriptor);
        hash.Add(Location);

        foreach (var arg in GetMessageArgs())
            hash.Add(arg);

        return hash.ToHashCode();
    }
}
