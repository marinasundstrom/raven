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

            var configuredSdkPath = Environment.GetEnvironmentVariable("MSBuildSDKsPath");
            try
            {
                MSBuildLocator.RegisterDefaults();
            }
            catch (InvalidOperationException) when (MSBuildLocator.IsRegistered)
            {
                // Another concurrent caller won the race.
            }
            finally
            {
                // Locator initializes SDK defaults, but an explicit SDK selection
                // must also govern the compiler's in-process project evaluation.
                if (!string.IsNullOrWhiteSpace(configuredSdkPath))
                    Environment.SetEnvironmentVariable("MSBuildSDKsPath", configuredSdkPath);
            }
        }
    }
}
