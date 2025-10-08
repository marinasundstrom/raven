# Run command from Raven.Compiler directory

set -euo pipefail

OUTPUT_DIR="output"
mkdir -p "$OUTPUT_DIR"

dotnet run -- samples/arrays.rav -o "$OUTPUT_DIR"/arrays.dll
dotnet run -- samples/enums.rav -o "$OUTPUT_DIR"/enums.dll
dotnet run -- samples/general.rav -o "$OUTPUT_DIR"/general.dll
dotnet run -- samples/generics.rav -o "$OUTPUT_DIR"/generics.dll
dotnet run -- samples/io.rav -o "$OUTPUT_DIR"/io.dll
dotnet run -- samples/test.rav -o "$OUTPUT_DIR"/test.dll
dotnet run -- samples/test2.rav -o "$OUTPUT_DIR"/test2.dll
dotnet run -- samples/type-unions.rav -o "$OUTPUT_DIR"/type-unions.dll
dotnet run -- samples/tuples.rav -o "$OUTPUT_DIR"/tuples.dll
dotnet run -- samples/main.rav -o "$OUTPUT_DIR"/main.dll
dotnet run -- samples/classes.rav -o "$OUTPUT_DIR"/classes.dll
cp ../TestDep/bin/Debug/net9.0/TestDep.dll "$OUTPUT_DIR"/TestDep.dll