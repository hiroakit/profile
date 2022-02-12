#!/usr/bin/env dotnet-script
#r "nuget: ProcessX, 1.5.4"
#r "nuget: Chell, 1.0.0"

using Zx;
using static Zx.Env;
using Chell;
using static Chell.Exports;
using System.Security.Cryptography;

public static class Font
{
    public static async Task Install()
    {
        Console.WriteLine("Install font...");

        var workDir = Path.Join("/", "tmp", "macos-setup");
        var fileName = "migmix-2m-20200307.zip";
        var url = $"https://osdn.net/projects/mix-mplus-ipa/downloads/72510/{fileName}";
        var sha256sum="f43cf36d697785989c49b231461ca49f69579c647634c51a914aaf7e61f2aeec";
                
        await $"mkdir -p {workDir}";

        Console.WriteLine($"Downloading {fileName}");
        await $"curl -s -L -G -o {Path.Join(workDir, fileName)} {url}";
                
        using (Cd(workDir))
        {
            var fileHash = "";
            using(SHA256 sha = SHA256.Create())
            {
                byte[] file = File.ReadAllBytes(fileName);
                byte[] hash = sha.ComputeHash(file);
                fileHash = String.Join("", hash.Select(x => x.ToString("x2")).ToArray());
            }                
            Console.WriteLine($"{fileName} has hash: {fileHash}");

            if (fileHash != sha256sum)
            {
                Console.WriteLine("Mismatch...");
                return;
            }

            await $"unzip -f {fileName}";
            using (Cd("migmix-2m-20200307"))
            {
                await "cp migmix-2m-bold.ttf ~/Library/Fonts/migmix-2m-reqular.ttf";
                await "cp migmix-2m-bold.ttf ~/Library/Fonts/migmix-2m-bold.ttf";
            }
        }
    }
}