# Details

## Reference assemblies

.NET have implementation assemblies and reference assemblies. Reference assemblies just describe the API surface and doesn't have any implementation. In that regard they are like header files. The implementation are in implementation assemblies that are unique to every runtime version.

### Compiling against reference assemblies

In order to to target different runtimes, you compile against the reference assemblies. Then at execution, the runtime will pick the implementation that is suitable.

### Loading metadata assemblies

The reference assemblies are pure metadata assemblies, so no IL, and therefore must be loaded in a special way. Using the `MetadataLoadContext` class you can load that metadata to query it for references.

## API

The compiler API doesn't keep track of what framework to target.

What version you are targeting depend on the core libraries that you reference.

You can add the `TargetFrameworkAttribute` to an assembly to indicate what version.