using System;

using Microsoft.Build.Locator;

namespace Raven.CodeAnalysis;

internal static class MsBuildLocatorRegistration
{
    private static readonly object Gate = new();

    public static void EnsureRegistered()
    {
        if (MSBuildLocator.IsRegistered)
            return;

        lock (Gate)
        {
            if (MSBuildLocator.IsRegistered)
                return;

            try
            {
                MSBuildLocator.RegisterDefaults();
            }
            catch (InvalidOperationException) when (MSBuildLocator.IsRegistered)
            {
                // Another concurrent caller won the race.
            }
        }
    }
}
