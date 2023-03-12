#!/usr/bin/env dotnet-script

public class PackageInfo
{
     public string Name { get; set; }
     public string Url { get; set; }
     public string FileName { get; set; }
     public string Version { get; set; }

     public PackageInfo()
     {

     }

     public PackageInfo(string name, string version, string url, string fileName)
     {
        this.Name = name;
        this.Version = version;
        this.Url = url;
        this.FileName = fileName;
     }
}
