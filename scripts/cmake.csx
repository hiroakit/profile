#!/usr/bin/env dotnet-script
#r "nuget: ProcessX, 1.5.5"
#r "nuget: Chell, 1.0.0"
#load "DirConfig.csx"
#load "PackageInfo.csx"

using Zx;
using static Zx.Env;
using Chell;
using static Chell.Exports;

// CMake
var pkgInfo = new PackageInfo();
pkgInfo.Url = "https://github.com/Kitware/CMake/releases/download/v3.23.3/cmake-3.23.3.tar.gz";
pkgInfo.FileName = "cmake-3.23.3.tar.gz";
pkgInfo.Name = "cmake-3.23.3";

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
       await $"tar xzf {pkgInfo.FileName}";      
    }
    using(Cd($"{pkgInfo.Name}"))
    {
       await $"./bootstrap --prefix={DirConfig.BuildDir()} --docdir={DirConfig.BuildDir()}/share/doc/cmake";
       await $"make && make install";
    }
}
