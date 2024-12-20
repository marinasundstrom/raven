# Resources

Here is a list of useful resources for when developing the compiler.

---

## Tools

Some helpful tools:

* [ILSpy](https://github.com/icsharpcode/ILSpy) - .NET de-compiler that can be used to inspect assemblies and turn instructions back into C#. Useful when trying to understand generated IL code, as well as when understanding the C# compiler. Since a lot of Roslyn's source code is generated and not available in the repository.

* [Roslyn Quoter](https://roslynquoter.azurewebsites.net/) - Turns C# code into Roslyn syntax node definitions in C#. Useful as a reference when designing syntax nodes. Also for when working on our "Generator" project.

## Compilers

Here are some compilers to have a look at:

* [Good for nothing compiler](https://github.com/johandanforth/good-for-nothing-compiler) - a simple compiler project in C#, originally by Joel Pobar and Joe Duffy, published in an article for MSDN Magazine.
* [Roslyn C# compiler](https://github.com/dotnet/roslyn) - the C# compiler and related services.
* [SmallBasic](https://github.com/sb/smallbasic-editor) - A Basic-like programming language and IDE meant to teach kids about programming. Built on C#/.NET.
* [IronPython3](https://github.com/IronLanguages/ironpython3) - An implementation of Python 3 for .NET. Source code C#.