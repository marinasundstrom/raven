using System;

namespace Raven.MetadataFixtures.DependencyInjection;

public enum ServiceLifetime
{
    Singleton = 0,
    Scoped = 1,
    Transient = 2,
}

public sealed class ServiceCollection
{
}

public sealed class DbContextOptionsBuilder
{
    public DbContextOptionsBuilder UseProvider(string connectionString) => this;
}

public static class ServiceCollectionExtensions
{
    public static ServiceCollection AddDbContext<TContext>(
        this ServiceCollection serviceCollection,
        Action<DbContextOptionsBuilder>? optionsAction = null,
        ServiceLifetime contextLifetime = ServiceLifetime.Scoped,
        ServiceLifetime optionsLifetime = ServiceLifetime.Scoped)
        => serviceCollection;

    public static ServiceCollection AddDbContext<TContext>(
        this ServiceCollection serviceCollection,
        Action<ServiceCollection, DbContextOptionsBuilder>? optionsAction,
        ServiceLifetime contextLifetime = ServiceLifetime.Scoped,
        ServiceLifetime optionsLifetime = ServiceLifetime.Scoped)
        => serviceCollection;
}
