using ObjCRuntime;

using UIKit;

namespace MauiCounter.Host;

public static class Program
{
    public static void Main(string[] args)
        => UIApplication.Main(args, null, typeof(AppDelegate));
}
