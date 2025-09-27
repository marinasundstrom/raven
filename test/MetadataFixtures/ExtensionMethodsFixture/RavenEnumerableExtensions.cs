using System;
using System.Collections.Generic;

namespace Raven.MetadataFixtures.Linq;

public static class RavenEnumerableExtensions
{
    public static IEnumerable<TResult> Select<TSource, TResult>(this IEnumerable<TSource> source, Func<TSource, TResult> selector)
    {
        if (source is null)
            throw new ArgumentNullException(nameof(source));
        if (selector is null)
            throw new ArgumentNullException(nameof(selector));

        return SelectIterator(source, selector);
    }

    public static IEnumerable<TSource> Where<TSource>(this IEnumerable<TSource> source, Func<TSource, bool> predicate)
    {
        if (source is null)
            throw new ArgumentNullException(nameof(source));
        if (predicate is null)
            throw new ArgumentNullException(nameof(predicate));

        return WhereIterator(source, predicate);
    }

    public static IEnumerable<TSource> OrderBy<TSource, TKey>(this IEnumerable<TSource> source, Func<TSource, TKey> keySelector)
        => OrderBy(source, keySelector, comparer: null);

    public static IEnumerable<TSource> OrderBy<TSource, TKey>(this IEnumerable<TSource> source, Func<TSource, TKey> keySelector, IComparer<TKey>? comparer)
    {
        if (source is null)
            throw new ArgumentNullException(nameof(source));
        if (keySelector is null)
            throw new ArgumentNullException(nameof(keySelector));

        comparer ??= Comparer<TKey>.Default;
        return OrderIterator(source, keySelector, comparer, descending: false);
    }

    public static IEnumerable<TSource> OrderByDescending<TSource, TKey>(this IEnumerable<TSource> source, Func<TSource, TKey> keySelector)
        => OrderByDescending(source, keySelector, comparer: null);

    public static IEnumerable<TSource> OrderByDescending<TSource, TKey>(this IEnumerable<TSource> source, Func<TSource, TKey> keySelector, IComparer<TKey>? comparer)
    {
        if (source is null)
            throw new ArgumentNullException(nameof(source));
        if (keySelector is null)
            throw new ArgumentNullException(nameof(keySelector));

        comparer ??= Comparer<TKey>.Default;
        return OrderIterator(source, keySelector, comparer, descending: true);
    }

    public static bool Any<TSource>(this IEnumerable<TSource> source)
    {
        if (source is null)
            throw new ArgumentNullException(nameof(source));

        using var enumerator = source.GetEnumerator();
        return enumerator.MoveNext();
    }

    public static bool Any<TSource>(this IEnumerable<TSource> source, Func<TSource, bool> predicate)
    {
        if (source is null)
            throw new ArgumentNullException(nameof(source));
        if (predicate is null)
            throw new ArgumentNullException(nameof(predicate));

        foreach (var item in source)
        {
            if (predicate(item))
                return true;
        }

        return false;
    }

    public static int Count<TSource>(this IEnumerable<TSource> source)
    {
        if (source is null)
            throw new ArgumentNullException(nameof(source));

        if (source is ICollection<TSource> collection)
            return collection.Count;

        var count = 0;
        foreach (var _ in source)
            count++;
        return count;
    }

    public static int Count<TSource>(this IEnumerable<TSource> source, Func<TSource, bool> predicate)
    {
        if (source is null)
            throw new ArgumentNullException(nameof(source));
        if (predicate is null)
            throw new ArgumentNullException(nameof(predicate));

        var count = 0;
        foreach (var item in source)
        {
            if (predicate(item))
                count++;
        }

        return count;
    }

    public static int Sum(this IEnumerable<int> source)
    {
        if (source is null)
            throw new ArgumentNullException(nameof(source));

        var sum = 0;
        foreach (var value in source)
            sum += value;
        return sum;
    }

    public static double Sum(this IEnumerable<double> source)
    {
        if (source is null)
            throw new ArgumentNullException(nameof(source));

        var sum = 0.0;
        foreach (var value in source)
            sum += value;
        return sum;
    }

    public static TSource First<TSource>(this IEnumerable<TSource> source)
    {
        if (source is null)
            throw new ArgumentNullException(nameof(source));

        using var enumerator = source.GetEnumerator();
        if (!enumerator.MoveNext())
            throw new InvalidOperationException("Sequence contains no elements.");

        return enumerator.Current;
    }

    public static TSource FirstOrDefault<TSource>(this IEnumerable<TSource> source, TSource defaultValue = default!)
    {
        if (source is null)
            throw new ArgumentNullException(nameof(source));

        using var enumerator = source.GetEnumerator();
        return enumerator.MoveNext() ? enumerator.Current : defaultValue;
    }

    public static List<TSource> ToList<TSource>(this IEnumerable<TSource> source)
    {
        if (source is null)
            throw new ArgumentNullException(nameof(source));

        return new List<TSource>(source);
    }

    private static IEnumerable<TResult> SelectIterator<TSource, TResult>(IEnumerable<TSource> source, Func<TSource, TResult> selector)
    {
        foreach (var item in source)
            yield return selector(item);
    }

    private static IEnumerable<TSource> WhereIterator<TSource>(IEnumerable<TSource> source, Func<TSource, bool> predicate)
    {
        foreach (var item in source)
        {
            if (predicate(item))
                yield return item;
        }
    }

    private static IEnumerable<TSource> OrderIterator<TSource, TKey>(IEnumerable<TSource> source, Func<TSource, TKey> keySelector, IComparer<TKey> comparer, bool descending)
    {
        var entries = new List<(TSource Item, TKey Key)>();
        foreach (var item in source)
        {
            entries.Add((item, keySelector(item)));
        }

        entries.Sort((left, right) => descending
            ? comparer.Compare(right.Key, left.Key)
            : comparer.Compare(left.Key, right.Key));

        foreach (var entry in entries)
            yield return entry.Item;
    }
}

public static class RavenArrayExtensions
{
    public static bool IsNullOrEmpty<T>(this T[]? values)
        => values is null || values.Length == 0;

    public static IEnumerable<T> AsEnumerable<T>(this T[] values)
    {
        if (values is null)
            throw new ArgumentNullException(nameof(values));

        for (var i = 0; i < values.Length; i++)
            yield return values[i];
    }
}

public static class RavenNullableExtensions
{
    public static T GetValueOr<T>(this T? value, T fallback)
        where T : struct
        => value ?? fallback;

    public static bool IsPresent<T>(this T? value)
        where T : struct
        => value.HasValue;
}
