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
  // Maybe required: export ~/dev/bin/pkg-config
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

  // Emacs
  var emacs = new Package();
  emacs.Name = "emacs-28.1";
  emacs.FileName = "emacs-28.1.tar.xz";
  emacs.Url = "https://ftp.jaist.ac.jp/pub/GNU/emacs/emacs-28.1.tar.xz";

  Echo("Emacs: Start to build. Using {emacs.Url}");
  if(!File.Exists($"{srcDir}/{emacs.Name}/nextstep/Emacs.app/Contents/MacOS/Emacs"))
  {
    if(!File.Exists(emacs.FileName))
    {
       await $"curl -OL {emacs.Url}";
    }
    await $"tar xzf {emacs.FileName}";
    using(Cd($"{emacs.Name}"))
    {
      await $"./configure --prefix={buildDir} CC=clang --with-ns --with-modules --without-x --without-selinux --without-makeinfo --without-mail-unlink --without-mailhost --without-pop --without-mailutils --without-jpeg --without-lcms2";
      await "make";
      await "make install";
    }
  }

  using(Cd($"{emacs.Name}/nextstep/Emacs.app/Contents"))
  {
	  Echo("Emacs: adding portability");
	  
	  // Clean up
	  if(Directory.Exists("Frameworks"))
	  {
		  await $"rm -rf ./Frameworks";
	  }

	  // Copy dylib in {libDir} to Frameworks dir
	  await $"mkdir -p Frameworks";
	  await $"cp {libDir}/libgnutls.30.dylib Frameworks";
	  await $"cp {libDir}/libnettle.8.dylib Frameworks";
	  await $"cp {libDir}/libhogweed.6.dylib Frameworks";
	  await $"cp {libDir}/libgmp.10.dylib Frameworks";

	  // Replace linker paths
	  using(Cd($"MacOS"))
	  {
		  Echo("Emacs: Replace linker paths");
		  var hogehoge = await $"otool -l ./Emacs | grep @executable_path/../Frameworks";
		  Echo($"Emacs: {hogehoge}");
		  if (!hogehoge.Contains("@executable_path/../Frameworks"))
		  {
			  // Add rpath (LC_RPATH)
			  Echo("Emacs: Add @rpath");		  
			  await $"install_name_tool -add_rpath @executable_path/../Frameworks Emacs";
		  }

		  // Rename shared library id name
		  Echo("Emacs: Rename shared library id name");		  
		  await $"install_name_tool -id @rpath/libgnutls.30.dylib ../Frameworks/libgnutls.30.dylib";
		  await $"install_name_tool -id @rpath/libgmp.10.dylib ../Frameworks/libgmp.10.dylib";
		  await $"install_name_tool -id @rpath/libhogweed.6.dylib ../Frameworks/libhogweed.6.dylib";
		  await $"install_name_tool -id @rpath/libnettle.8.dylib ../Frameworks/libnettle.8.dylib";

		  // Replace path to @rpath based
		  Echo("Emacs: Replace path to @rpath based");		  
		  await $"install_name_tool -change ~/dev/lib/libnettle.8.dylib @rpath/libnettle.8.dylib ../Frameworks/libgnutls.30.dylib";
		  await $"install_name_tool -change ~/dev/lib/libgmp.10.dylib @rpath/libgmp.10.dylib ../Frameworks/libgnutls.30.dylib";
		  await $"install_name_tool -change ~/dev/lib/libhogweed.6.dylib @rpath/libhogweed.6.dylib ../Frameworks/libgnutls.30.dylib";
		  await Run($"install_name_tool -change ~/dev/lib/libgnutls.30.dylib @rpath/libgnutls.30.dylib Emacs").NoThrow();      		  
	  }
  }

  var securityResponse = await $"security find-certificate -c EmacsSelfSignedCertificate";
  if (securityResponse.Contains("EmacsSelfSignedCertificate"))
  {
	  using(Cd($"{emacs.Name}/nextstep"))
	  {
		  await $"codesign --remove-signature Emacs.app";
		  await $"codesign --deep -s EmacsSelfSignedCertificate ./Emacs.app";
	  }           
  }
}

// using (Cd($"{~/path/to/your/profile/resources}"))
// {
//     await "stow -v zsh -t ~/";
//     await "stow -v emacs -t ~/";
//     await "stow -v git -t ~/";    
// }
