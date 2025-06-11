# Run command from Raven.Compiler directory

dotnet run -- samples/arrays.rav -o output/arrays.dll
dotnet run -- samples/enums.rav -o output/enums.dll
dotnet run -- samples/general.rav -o output/general.dll
dotnet run -- samples/generics.rav -o output/generics.dll
dotnet run -- samples/io.rav -o output/io.dll
dotnet run -- samples/test.rav -o output/test.dll
dotnet run -- samples/test2.rav -o output/test2.dll
dotnet run -- samples/type-unions.rav -o output/type-unions.dll
cp ../TestDep/bin/Debug/net9.0/TestDep.dll output/TestDep.dll