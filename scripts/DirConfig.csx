#!/usr/bin/env dotnet-script

#r "nuget:NetStandard.Library,2.0.3"

public static class DirConfig
{
    public static string HomeDir()
    {
        return Environment.GetEnvironmentVariable("HOME");
    }

    public static string BuildDir()
    {
        return Path.Combine(DirConfig.HomeDir(), "dev");
    }

    public static string SrcDir()
    {
        return Path.Combine(DirConfig.BuildDir(), "src");
    }

    public static string BinDir()
    {
        return Path.Combine(DirConfig.BuildDir(), "bin");
    }

    public static string LibDir()
    {
        return Path.Combine(DirConfig.BuildDir(), "lib");
    }
}