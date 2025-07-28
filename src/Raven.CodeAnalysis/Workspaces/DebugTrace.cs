using System.Text;

namespace Raven.CodeAnalysis;

public static class DebugTrace
{
    public static string Describe(Solution solution)
    {
        var sb = new StringBuilder();
        sb.AppendLine($"Solution: {solution.Id} (v{solution.Version})");

        foreach (var project in solution.Projects)
        {
            sb.AppendLine($"  Project: {project.Id} (v{project.Version}) - {project.Name}");
            foreach (var doc in project.Documents)
            {
                sb.AppendLine($"    Document: {doc.Id} (v{doc.Version}) - {doc.Name}");
            }
        }

        return sb.ToString();
    }
}

