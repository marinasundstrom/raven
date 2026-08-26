using Microsoft.Maui.Hosting;

namespace MauiCounter.Host;

public static class MauiProgram
{
    public static MauiApp CreateMauiApp()
        => MauiApp.CreateBuilder()
            .UseMauiApp<App>()
            .Build();
}
