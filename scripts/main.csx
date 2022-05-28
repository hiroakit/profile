#!/usr/bin/env dotnet-script
#r "nuget: ProcessX, 1.5.4"
#r "nuget: Chell, 1.0.0"
#load "font.csx"
#load "Package.csx"

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
var libDir = Path.Combine(buildDir, "lib");

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
var pkgconfig = "pkg-config-0.29.2";
var pkgconfigPkg = $"{pkgconfig}.tar.gz";
var pkgconfigUrl = $"https://pkgconfig.freedesktop.org/releases/{pkgconfigPkg}";
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
  var cmdPath = string.Empty;

  // Autoconf 
  TryWhich("autoconf", out cmdPath);
  if(cmdPath.Length < 1)
  {
    await $"curl -OL {autoconfUrl}";
    await $"tar xzf {autoconfPkg}";
    using (Cd($"{autoconf}"))
    {
        await $"./configure --prefix={buildDir}";
        await "make";
        await "make install";
    }
  }

  TryWhich("gdb", out cmdPath);	
  if (cmdPath.Length < 1)
  {
  }
  else 
  {
  }


  // Automake
  TryWhich("automake", out cmdPath);
  if(cmdPath.Length < 1)
  {
     await $"curl -OL {automakeUrl}";
     await $"tar xzf {automakePkg}";
     using (Cd($"{automake}"))
     {
        await $"./configure --prefix={buildDir}";
        await "make";
        await "make install";
     }
  }

  // Libtool
  TryWhich("libtool", out cmdPath);
  if(cmdPath.Length < 1)
  {
    await $"curl -OL {libtoolUrl}";
    await $"tar xzf {libtoolPkg}";
    using (Cd($"{libtool}"))
    {
      await $"./configure --prefix={buildDir}";
      await "make";
      await "make install";
    }
  }
  
  // stow
  TryWhich("stow", out cmdPath);
  if(cmdPath.Length < 1)
  {
    await $"curl -OL {stowUrl}";
    await $"tar xzf {stowPkg}";
    using (Cd($"{stow}"))
    {
       await $"./configure --prefix={buildDir}";
       await "make";
       await "make install";
    }
  }

  // texinfo
  TryWhich("makeinfo", out cmdPath);
  if(cmdPath.Length < 1)
  {
    await $"curl -OL {texinfoUrl}";
    await $"tar xzf {texinfoPkg}";
    using (Cd($"{texinfo}"))
    {
      await $"./configure --prefix={buildDir}";
      await "make";
      await "make install";
    }
  }

  // pkg-config
  TryWhich("pkg-config", out cmdPath);
  if(cmdPath.Length < 1)
  {
    await $"curl -OL {pkgconfigUrl}";
    await $"tar xzf {pkgconfigPkg}";
    using(Cd($"{pkgconfig}"))
    {
      await $"./configure --prefix={buildDir} --with-internal-glib";
      await "make";
      await "make install";
    }
  }
  else
  {
     Chell.Exports.Env.Vars["PKG_CONFIG_PATH"] = $"{libDir}/pkgconfig:{Chell.Exports.Env.Vars["PKG_CONFIG_PATH"]}";
  }

  // libGmp
  var libGmp = new Package();
  libGmp.Name = "gmp-6.2.1";
  libGmp.FileName = "gmp-6.2.1.tar.xz";
  libGmp.Url = "https://ftp.jaist.ac.jp/pub/GNU/gmp/gmp-6.2.1.tar.xz";
  if(!File.Exists($"{libDir}/libGmp.dylib"))
  {
    if(!File.Exists(libGmp.FileName))
    {
       await $"curl -OL {libGmp.Url}";
    }
    await $"tar xzf {libGmp.FileName}";
    using(Cd($"{libGmp.Name}"))
    {
      await $"./configure --prefix={buildDir}";
      await "make";
      await "make check";
      await "make install";
    }
  }

  // libnettle
  var libNettle = new Package();
  libNettle.Name = "nettle-3.7.3";
  libNettle.FileName = "nettle-3.7.3.tar.gz";
  libNettle.Url = "https://ftp.gnu.org/gnu/nettle/nettle-3.7.3.tar.gz";
  if(!File.Exists($"{libDir}/libnettle.dylib"))
  {
    if(!File.Exists(libNettle.FileName))
    {
       await $"curl -OL {libNettle.Url}";
    }
    await $"tar xzf {libNettle.FileName}";
    using(Cd($"{libNettle.Name}"))
    {
      // ./configure --prefix=$HOME/build --build=aarch64-apple-darwin`uname -r` --disable-openssl --disable-static --enable-mini-gmp
      await $"./configure --prefix={buildDir} --build=aarch64-apple-darwin`uname -r` --disable-openssl --disable-static --enable-mini-gmp";
      await "make";
      await "make install";
      await "make check";
    }
  }

  // gnutls
  // export ~/dev/bin/pkg-config
  var libgnutls = new Package();
  libgnutls.Name = "gnutls-3.6.15";
  libgnutls.FileName = "gnutls-3.6.15.tar.xz";
  libgnutls.Url = "http://www.ring.gr.jp/pub/net/gnupg/gnutls/v3.6/gnutls-3.6.15.tar.xz";
  if(!File.Exists($"{libDir}/libgnutls.dylib"))
  {
    if(!File.Exists(libgnutls.FileName))
    {
       await $"curl -OL {libgnutls.Url}";
    }
    await $"tar xzf {libgnutls.FileName}";
    using(Cd($"{libgnutls.Name}"))
    {
      // https://gitlab.com/gnutls/gnutls/-/issues/1347
      // --disable-hardware-acceleration
      // --disable-heartbeat-support
      // --disable-silent-rules
      // --disable-static
      // GMP_CFLAGS=`pkg-config --cflags gmp` GMP_LIBS=`pkg-config --libs gmp` ./configure --prefix=~/dev --disable-cxx --without-p11-kit --with-included-libtasn1 --with-included-unistring --build=aarch64-apple-darwin`uname -r` --disable-hardware-acceleration --disable-heartbeat-support --disable-doc
      await $"./configure --prefix={buildDir} --disable-cxx --without-p11-kit --with-included-libtasn1 --with-included-unistring --build=aarch64-apple-darwin`uname -r`";
      await "make";
      await "make install";
    }
  }  
}

// using (Cd($"{~/path/to/your/profile/resources}"))
// {
//     await "stow -v zsh -t ~/";
//     await "stow -v emacs -t ~/";
//     await "stow -v git -t ~/";    
// }
