using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;

namespace Raven.CodeAnalysis;

public sealed class CompositeProjectSystemService : IProjectSystemService
{
    private readonly ImmutableArray<IProjectSystemService> _services;

    public CompositeProjectSystemService(params IProjectSystemService[] services)
    {
        ArgumentNullException.ThrowIfNull(services);

        _services = services
            .Where(static service => service is not null)
            .ToImmutableArray();

        if (_services.Length == 0)
            throw new ArgumentException("At least one project-system service must be provided.", nameof(services));
    }

    public bool CanOpenProject(string projectFilePath)
        => TryGetService(projectFilePath, out _);

    public IReadOnlyList<string> GetProjectReferencePaths(string projectFilePath)
        => GetRequiredService(projectFilePath).GetProjectReferencePaths(projectFilePath);

    public ProjectId OpenProject(Workspace workspace, string projectFilePath)
        => GetRequiredService(projectFilePath).OpenProject(workspace, projectFilePath);

    public void SaveProject(Project project, string filePath)
        => GetRequiredService(filePath).SaveProject(project, filePath);

    private IProjectSystemService GetRequiredService(string projectFilePath)
    {
        if (TryGetService(projectFilePath, out var service))
            return service;

        throw new NotSupportedException($"No registered project-system service can open '{projectFilePath}'.");
    }

    private bool TryGetService(string projectFilePath, out IProjectSystemService service)
    {
        service = _services.FirstOrDefault(candidate => candidate.CanOpenProject(projectFilePath))!;
        return service is not null;
    }
}
