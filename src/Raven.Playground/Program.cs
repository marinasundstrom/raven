using Microsoft.AspNetCore.Components.Web;
using Microsoft.AspNetCore.Components.WebAssembly.Hosting;

using Raven.Playground;
using Raven.Playground.Services;

var builder = WebAssemblyHostBuilder.CreateDefault(args);
builder.RootComponents.Add<App>("#app");
builder.RootComponents.Add<HeadOutlet>("head::after");

builder.Services.AddScoped(sp => new HttpClient { BaseAddress = new Uri(builder.HostEnvironment.BaseAddress) });
builder.Services.AddSingleton<PlaygroundFrameworkReferences>();
builder.Services.AddSingleton<PlaygroundLanguageService>();
builder.Services.AddSingleton<PlaygroundProgramRunner>();

await builder.Build().RunAsync();
