namespace Raven.Editor.Tests;

using System;
using System.Reflection;
using Terminal.Gui;

public class ProgramTests
{
    [Fact]
    public void HandleCancelKeyPress_SetsCancel()
    {
        Application.Init(new FakeDriver());

        var e = (ConsoleCancelEventArgs)Activator.CreateInstance(
            typeof(ConsoleCancelEventArgs),
            BindingFlags.Instance | BindingFlags.NonPublic,
            null,
            new object[] { ConsoleSpecialKey.ControlC },
            null)!;

        Program.HandleCancelKeyPress(null, e);

        e.Cancel.ShouldBeTrue();

        Application.Shutdown();
    }
}
