using System.Runtime.InteropServices;

namespace Raven;

internal static class NativeMethods
{
    [DllImport("libc", SetLastError = true)]
    public static extern int chmod(string pathname, int mode);
}
