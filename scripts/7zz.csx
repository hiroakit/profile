#!/usr/bin/env dotnet-script
#r "nuget: ProcessX, 1.5.5"
#r "nuget: Chell, 1.0.0"
#load "DirConfig.csx"
#load "PackageInfo.csx"

using Zx;
using static Zx.Env;
using Chell;
using static Chell.Exports;

// 7zz (7-Zip)
var pkgInfo = new PackageInfo();
pkgInfo.Url = "https://7-zip.org/a/7z2107-mac.tar.xz";
pkgInfo.FileName = "7z2107-mac.tar.xz";
pkgInfo.Name = "7z2107-mac";

// Installation
using(Cd(DirConfig.SrcDir()))
{
    if(!File.Exists(pkgInfo.FileName))
    {
       await $"curl -OL {pkgInfo.Url}";
    }
    if(!Directory.Exists(pkgInfo.Name))
    {
       await $"mkdir -p {pkgInfo.Name}";
       await $"tar xzf {pkgInfo.FileName} -C {pkgInfo.Name}";      
    }
    using(Cd($"{pkgInfo.Name}"))
    {
       await $"cp ./7zz {DirConfig.BinDir()}";
    }
}