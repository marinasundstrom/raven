namespace Raven;

public static class HashSetExtensions
{
    public static void AddRange<T>(this HashSet<T> set, params IEnumerable<T> items)
    {
        foreach (var item in items)
            set.Add(item);
    }
}
