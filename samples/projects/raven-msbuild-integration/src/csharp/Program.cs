using System.Reflection;

var ravenAssemblyPath = Path.Combine(AppContext.BaseDirectory, "RavenGreeter.dll");
var ravenAssembly = Assembly.LoadFrom(ravenAssemblyPath);
var greeterType = ravenAssembly.GetType("Greeter", throwOnError: true)!;
var messageMethod = greeterType.GetMethod("Message", BindingFlags.Public | BindingFlags.Static)!;
var message = (string)messageMethod.Invoke(null, null)!;

Console.WriteLine(message);
