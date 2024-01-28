#!/usr/bin/env dotnet-script
#r "nuget: ProcessX, 1.5.5"
#r "nuget: Chell, 1.0.0"

using Cysharp.Diagnostics;
using Zx;
using static Zx.Env;
using Chell;
using static Chell.Exports;

public async Task<bool> ExsistCommandAsync(string command)
{
    try
    {
        await $"type {command}";
        return true;
    }
    catch (ProcessErrorException ex)
    {
        Echo($"{command} Not Found. {ex.Message}");
    }

    return false;
}

bool hasBrewCommand = await ExsistCommandAsync("brew");
if (!hasBrewCommand)
{
    throw new InvalidOperationException("Required Homebrew.");
}

bool hasStowCommand = await ExsistCommandAsync("stow");
if (!hasStowCommand)
{
    await "brew install stow";
}

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
