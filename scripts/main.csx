#!/usr/bin/env dotnet-script
#r "nuget: ProcessX, 1.5.4"
#r "nuget: Chell, 1.0.0"
#load "font.csx"

using Zx;
using static Zx.Env;
using Chell;
using static Chell.Exports;

shell = "/bin/sh -c";

// await Font.Install();

var homeDir = Environment.GetEnvironmentVariable("HOME");
var buildDir = Path.Combine(homeDir, "dev");
var binDir = Path.Combine(buildDir, "bin");
var srcDir = Path.Combine(buildDir, "src");

Chell.Exports.Env.Vars["PATH"] = $"{binDir}:{Chell.Exports.Env.Vars["PATH"]}";

var autoconf = "autoconf-2.71";
var autoconfPkg = $"{autoconf}.tar.gz";
var autoconfUrl = $"https://ftp.jaist.ac.jp/pub/GNU/autoconf/{autoconfPkg}";
var automake = "automake-1.16.5";
var automakePkg = $"{automake}.tar.gz";
var automakeUrl = $"https://ftp.jaist.ac.jp/pub/GNU/automake/{automakePkg}";
var libtool = "libtool-2.4.6";
var libtoolPkg = $"{libtool}.tar.gz";
var libtoolUrl = $"https://ftp.jaist.ac.jp/pub/GNU/libtool/{libtoolPkg}";

var stow = "stow-2.3.1";
var stowPkg = $"{stow}.tar.gz";
var stowUrl = $"https://ftp.jaist.ac.jp/pub/GNU/stow/{stowPkg}";

var texinfo = "texinfo-6.8";
var texinfoPkg = $"{texinfo}.tar.gz";
var texinfoUrl = $"https://ftp.jaist.ac.jp/pub/GNU/texinfo/{texinfoPkg}";

if(!Directory.Exists(srcDir))
{
  await $"mkdir -p {srcDir}";
}
using(Cd($"{srcDir}"))
{
  // Autoconf
  await $"curl -OL {autoconfUrl}";
  await $"tar xzf {autoconfPkg}";
  using (Cd($"{autoconf}"))
  {
      await $"./configure --prefix={buildDir}";
      await "make";
      await "make install";
  }

  // Automake
  await $"curl -OL {automakeUrl}";
  await $"tar xzf {automakePkg}";
  using (Cd($"{automake}"))
  {
      await $"./configure --prefix={buildDir}";
      await "make";
      await "make install";
  }

  // Libtool
  await $"curl -OL {libtoolUrl}";
  await $"tar xzf {libtoolPkg}";
  using (Cd($"{libtool}"))
  {
      await $"./configure --prefix={buildDir}";
      await "make";
      await "make install";
  }

  // stow
  await $"curl -OL {stowUrl}";
  await $"tar xzf {stowPkg}";
  using (Cd($"{stow}"))
  {
      await $"./configure --prefix={buildDir}";
      await "make";
      await "make install";
  }

  // texinfo
  await $"curl -OL {texinfoUrl}";
  await $"tar xzf {texinfoPkg}";
  using (Cd($"{texinfo}"))
  {
      await $"./configure --prefix={buildDir}";
      await "make";
      await "make install";
  }
}
// using (Cd($"{~/path/to/your/profile/resources}"))
// {
//     await "stow -v zsh -t ~/";
//     await "stow -v emacs -t ~/";
//     await "stow -v git -t ~/";    
// }
