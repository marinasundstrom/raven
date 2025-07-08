namespace Raven.CodeAnalysis;

record struct StateChange(SolutionState NewSolutionState, ProjectState OldProjectState, ProjectState NewProjectState);