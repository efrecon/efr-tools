# Introduction #

This wiki page describes the "preferred" directory structure when working with the efr-tools. This structure has a lot of historical background, but it has proven useful across the years.  It attempts to separate the common code from the code that is specific to applications, and to make sure most libraries end up in the common code so that they can be reused. This structuring easily adapts to code migration between machines and operating systems.


# Linking #

Symbolic links have existing on UNIX systems for years and when moving to Windows, there are no perfect equivalent. However, files ending with `.lnk` can be considered as links. The efr-tools contain a number of ways to read and understand those files.  The work is performed in a file called [bootstrap.tcl](http://code.google.com/p/efr-tools/source/browse/trunk/efr-lib/bootstrap.tcl) and the code will attempt to do as follows:

  * Use [tcom](http://wiki.tcl.tk/1821) if it is present, since it is quick and the solution that works best. The code comes from the [wiki](http://wiki.tcl.tk/1844).
  * Use `cscript` as pointed out on the [wiki](http://wiki.tcl.tk/1844). This has the disadvantage of using an external process, which can slowdown link reading if many links are to be read.
  * Try to extract the snippets as proposed in the same wiki page, which does not always work but sometimes is the last resort.

To create "symbolic links" on Windows, the wiki (again) proposes a number of solutions. efr-tools creates links using a separate process, originally found at [flexhex](http://www.flexhex.com/docs/articles/hard-links.phtml#download).

Once ways exist to create and read links on Windows, organisation of data across directories can be more flexible.

# Organising #

## Common Code ##

I keep all common code in a directory called... `common`. Under that directory, I keep a copy of the `til`.  I check out separately the directory of the `til` using the following command.

```
svn checkout http://efr-tools.googlecode.com/svn/trunk/til/ til/
```

Under `common`, I have a directory called `tcldev` that contains all other common code. Under that directory, I create the following ones:

  * `bin` contains a number of (unreleased) useful scripts.
  * `contrib` contains a number of packages found here and there. I usually have one directory for each package to keep things tidy and clean.
  * `lib` is a checkout of the `efr-lib` directory, though renamed with `lib` instead (to make it consistent with the UNIX naming conventions). The checkout uses a command similar to the following one:

```
svn checkout http://efr-tools.googlecode.com/svn/trunk/efr-lib/ lib/
```

## Projects ##

Each project (the definition is vague!) finds its home in a directory that contains the name of the project. Under that directory, I usually keep the same directory structure, i.e.

  * The main script(s) for the project is(are) kept directly under the directory.
  * A `lib` directory contains all libraries that are necessary to run the project. The content of this directory contains either libraries that are project specific, each in its own directory or links into the directories of the [DirectoryStructure#Common\_Code](DirectoryStructure#Common_Code.md). These links are created automatically as described below.
  * A `make` directory contains small scripts to create the links and to make the binaries, i.e. generate the Windows executable for those projects.

The `make` directory always contains a script that I copy (and adapt) from one project to the next and which aims at creating all the links and copy all the necessary code for successful projects. An example of this script is [contained](http://code.google.com/p/efr-tools/source/browse/trunk/apps/cxManager/make/makelinks.tcl) in the directory for the context manager.  The code creates links from all relevant directories into the `lib` directory of the project.  It also copies `bootstrap.tcl` and `init.tcl`, two files that are included at the beginning of each main script placed under the main project directory.  The `make` directory also contains a number of rules to create the "binaries" of the application.  The initialisation files have the following goals:

  * [bootstrap.tcl](http://code.google.com/p/efr-tools/source/browse/trunk/efr-lib/bootstrap.tcl) arranges to access `argutil.tcl` an initialisation file that is required for all `til`-based projects.  Since the `til` is usually placed in a directory that is linked from the project, `bootstrap.tcl` contains a simplified version of the Windows `.lnk` files.
  * [init.tcl](http://code.google.com/p/efr-tools/source/browse/trunk/efr-lib/init.tcl) does a whole lot of program/script initialisation. It provides a number of facilities that are common to be performed at the beginning of a script. These are:
    * A number of common options to set verbosity level, to read program arguments and options from local files and remote locations, to connect to the program using [tkcon](http://tkcon.sourceforge.net/) (via the services of [tkconclient](http://www2.tcl.tk/14701)), to set the language of the application and to set its version number.
    * Access to the top directory in a wrap-agnostic manner.
    * Services to select the localisation files to use for the application.
    * A generic program start routine that have support for a splash-screen (on Tk), options parsing and overriding, a callback system for following the initialisation, the access to linked directories (on Windows), the loading of a series of packages, but also libraries written using the conventions of the `til`, etc.

## Migration ##

I migrate between machines and operating systems on a daily basis. To migrate I use a memory stick containing the main copy of all my projects.
  * On Windows, I keep the content of the memory stick and of the hard drive in sync using [SyncBack](http://www.2brightsparks.com/freeware/freeware-hub.html).
  * On Linux, I keep the content of the memory stick and of the hard drive in sync using [usbsync](http://wellhoefer.info/usbsync/).

A side-effect of this migration is that I get backup for free, or almost. I always have a computer, somewhere with a fairly recent copy of my work. Sometimes it is a few days old, but never much more.

# Conclusion #

This document contains a number of conventions for arranging and structuring files when working with Tcl-projects of fairly decent size. At the core of these working practices is the use a central repository of reusable code, a repository that is linked into the `lib` directory of each project. On Windows, these links are implemented using `.lnk` files; on other platforms, using symbolic links.