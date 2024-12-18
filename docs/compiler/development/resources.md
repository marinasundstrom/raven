# Resources

Here is a list of useful resources for when developing the compiler.

---

* [ILSpy](https://github.com/icsharpcode/ILSpy) - .NET de-compiler that can be used to inspect assemblies and turn instructions back into C#. Useful when trying to understand generated IL code, as well as when understanding the C# compiler. Since a lot of Roslyn's source code is generated and not available in the repository.

* [Roslyn Quoter](https://roslynquoter.azurewebsites.net/) - Turns C# code into Roslyn syntax node definitions in C#. Useful as a reference when designing syntax nodes. Also for when working on our "Generator" project.