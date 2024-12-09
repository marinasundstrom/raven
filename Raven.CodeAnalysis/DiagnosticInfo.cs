namespace Raven.CodeAnalysis;

public class DiagnosticInfo
{
    public string Message { get; }
    public int Start { get; }
    public int Length { get; }
    public string Severity { get; }

    public DiagnosticInfo(string message, int start, int length, string severity)
    {
        Message = message;
        Start = start;
        Length = length;
        Severity = severity;
    }
}