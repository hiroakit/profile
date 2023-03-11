#!/usr/bin/env dotnet-script
#r "nuget: ProcessX, 1.5.5"
#r "nuget: Chell, 1.0.0"

using Zx;
using static Zx.Env;
using Chell;
using static Chell.Exports;

var gitUrl = "https://github.com/pyenv/pyenv.git";
var homeDir = Environment.GetEnvironmentVariable("HOME");
var pyenvDir = Path.Combine(homeDir, ".pyenv");

if(!Directory.Exists(pyenvDir))
{
  Console.WriteLine($"URL: {gitUrl}");
  await $"git clone {gitUrl} {pyenvDir}";
}

// $HOME/.pyenv/libexec/pyenv-realpath.dylib
var realPathDyLibPath = Path.Combine(pyenvDir, "libexec", "pyenv-realpath.dylib");
if(!File.Exists(realPathDyLibPath))
{
  using(Cd(pyenvDir))
  {
    await "src/configure && make -C src";
  }
}
else
{
  Console.WriteLine($"Found pyenv-realpath.dylib: {realPathDyLibPath}"); 
}

// W.I.P
// var zshDotDir = Environment.GetEnvironmentVariable("ZDOTDIR");
// var zshrcFile = Path.Combine(zshDotDir, ".zshrc");
// File.AppendAllText(zshrcFile, Environment.NewLine+$"export PYENV_ROOT=\"$HOME/.pyenv\"");

Console.WriteLine(Environment.NewLine+"Apply your shell config"+Environment.NewLine);
Console.WriteLine("export PYENV_ROOT=\"$HOME/.pyenv\"");
Console.WriteLine("command -v pyenv >/dev/null || export PATH=\"$PYENV_ROOT/bin:$PATH\"");
Console.WriteLine("eval \"$(pyenv init -)\"");