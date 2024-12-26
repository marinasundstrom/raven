namespace Raven.CodeAnalysis;

public class AdhocWorkspace : Workspace
{
    public AdhocWorkspace() : base(kind: "Adhoc")
    {
        CurrentSolution = new Solution(
            this,
            new SolutionInfo(
                new SolutionAttributes(SolutionId.CreateNewId(), VersionStamp.Create(), null), []));
    }
}
