#!/usr/bin/env dotnet-script
#r "nuget: ProcessX, 1.5.5"
#r "nuget: Chell, 1.0.0"

using Zx;
using static Zx.Env;
using Chell;
using static Chell.Exports;

// Create symbolic link by GNU Stow
string currentDir = Directory.GetCurrentDirectory();
string resourcesDir = Path.Combine(currentDir, "resources");
using (Cd($"{resourcesDir}"))
{
    await "stow -v 2 -t ~/ vim";
    await "stow -v 2 -t ~/ git";
    await "stow -v 2 -t ~/ zsh";
    await "stow -v 2 -t ~/ emacs";    
}