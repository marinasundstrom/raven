param(
    [Parameter(Mandatory = $true)]
    [string] $Version,
    [string] $InstallRoot = $(if ($env:RAVEN_INSTALL_ROOT) { $env:RAVEN_INSTALL_ROOT } else { Join-Path $HOME ".raven" }),
    [string] $Repository = $(if ($env:RAVEN_GITHUB_REPOSITORY) { $env:RAVEN_GITHUB_REPOSITORY } else { "marinasundstrom/raven" })
)

$ErrorActionPreference = "Stop"
$Version = $Version.TrimStart("v")
$architecture = [System.Runtime.InteropServices.RuntimeInformation]::OSArchitecture
$arch = switch ($architecture) {
    "X64" { "x64" }
    "Arm64" { "arm64" }
    default { throw "Unsupported Windows architecture: $architecture" }
}

$rid = "win-$arch"
$asset = "raven-sdk-$Version-$rid.zip"
$baseUrl = if ($env:RAVEN_RELEASE_BASE_URL) { $env:RAVEN_RELEASE_BASE_URL } else { "https://github.com/$Repository/releases/download/v$Version" }
$temporaryDirectory = Join-Path ([System.IO.Path]::GetTempPath()) ("raven-install-" + [guid]::NewGuid())
$archivePath = Join-Path $temporaryDirectory $asset
$checksumPath = Join-Path $temporaryDirectory "SHA256SUMS"

try {
    New-Item -ItemType Directory -Path $temporaryDirectory | Out-Null
    Invoke-WebRequest "$baseUrl/$asset" -OutFile $archivePath
    Invoke-WebRequest "$baseUrl/SHA256SUMS" -OutFile $checksumPath

    $checksumLine = Get-Content $checksumPath | Where-Object { $_ -match "\s\*?$([regex]::Escape($asset))$" } | Select-Object -First 1
    if (-not $checksumLine) {
        throw "No checksum was published for $asset."
    }

    $expected = ($checksumLine -split "\s+")[0].ToLowerInvariant()
    $actual = (Get-FileHash $archivePath -Algorithm SHA256).Hash.ToLowerInvariant()
    if ($actual -ne $expected) {
        throw "Checksum verification failed for $asset."
    }

    Expand-Archive $archivePath -DestinationPath $temporaryDirectory
    $sourceDirectory = Join-Path $temporaryDirectory "raven-sdk-$Version-$rid"
    $destination = Join-Path (Join-Path $InstallRoot "sdk") $Version
    $binDirectory = Join-Path $InstallRoot "bin"

    if (-not (Test-Path $sourceDirectory -PathType Container)) {
        throw "The archive does not contain the expected SDK directory."
    }

    New-Item -ItemType Directory -Force -Path (Split-Path $destination), $binDirectory | Out-Null
    if (Test-Path $destination) {
        Remove-Item -Recurse -Force $destination
    }
    Move-Item $sourceDirectory $destination

    $escapedDestination = $destination.Replace("%", "%%")
    Set-Content (Join-Path $binDirectory "rvn.cmd") "@dotnet `"$escapedDestination\tools\rvn\rvn.dll`" %*" -Encoding Ascii
    Set-Content (Join-Path $binDirectory "rvnc.cmd") "@dotnet `"$escapedDestination\tools\rvnc\rvnc.dll`" %*" -Encoding Ascii
    Set-Content (Join-Path $binDirectory "raven-language-server.cmd") "@dotnet `"$escapedDestination\tools\language-server\Raven.LanguageServer.dll`" %*" -Encoding Ascii

    Write-Output "Installed Raven $Version to $destination"
    Write-Output "Add $binDirectory to PATH."
}
finally {
    if (Test-Path $temporaryDirectory) {
        Remove-Item -Recurse -Force $temporaryDirectory
    }
}
