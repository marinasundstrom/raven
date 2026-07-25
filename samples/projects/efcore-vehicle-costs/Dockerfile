FROM mcr.microsoft.com/dotnet/sdk:11.0-preview AS build
WORKDIR /src

COPY src/Raven.Compiler ./src/Raven.Compiler
COPY src/Raven.CodeAnalysis ./src/Raven.CodeAnalysis
COPY src/Raven.Core ./src/Raven.Core
COPY samples/projects/efcore-vehicle-costs ./samples/projects/efcore-vehicle-costs

WORKDIR /src
RUN dotnet build ./src/Raven.Compiler/Raven.Compiler.csproj -f net11.0 /property:WarningLevel=0

WORKDIR /src/samples/projects/efcore-vehicle-costs
RUN dotnet build VehicleCostsApi.rvnproj /property:WarningLevel=0

FROM mcr.microsoft.com/dotnet/aspnet:11.0-preview
WORKDIR /app
COPY --from=build /src/samples/projects/efcore-vehicle-costs/bin ./bin

ENV ASPNETCORE_URLS=http://+:8080
EXPOSE 8080

ENTRYPOINT ["dotnet", "/app/bin/Debug/net11.0/VehicleCostsApi.dll"]
