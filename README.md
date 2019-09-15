# omnisharp-emacs
<a href="//travis-ci.org/OmniSharp/omnisharp-emacs">
    <img src="https://travis-ci.org/OmniSharp/omnisharp-emacs.svg?branch=master" />
</a>

omnisharp-emacs is a port of the awesome [omnisharp-roslyn][] server to the
Emacs text editor. It provides IDE-like features for editing files in
C# solutions in Emacs, provided by an OmniSharp server instance that
works in the background.

Note that C# syntax highlighting and indenting is provided by [`csharp-mode`](https://github.com/josteink/csharp-mode) which is a dependency of this 
package. See [Configuration](#configuration) section below on how to enable
`omnisharp-mode` via the `csharp-mode` hook.

This package is licensed under GNU General Public License version 3, 
or (at your option) any later version.


## Features
Please see [omnisharp-emacs Features](doc/features.md). Please note that information
on the Features page is outdated and some commands are not ported to the new roslyn
version of `omnisharp-emacs` yet.


## Package Installation
This package requires Emacs 24.4 and above. It has been tested on
Ubuntu, Windows 7+ and on macOS.


### External Dependencies
You may need to have one or more of .NET SDKs (and mono – on UNIX platforms)
installed for your project to be properly processed by  omnisharp server. 

Note that multiple .NET SDKs can be installed in parallel, too.

See:
 - [.NET (Core) SDKs](https://www.microsoft.com/net/targeting)
 - [mono project](http://www.mono-project.com/download/)


### Installation on Spacemacs
Add `csharp` layer to `dotspacemacs-configuration-layers` on your `.spacemacs`
file and restart Emacs or run `dotspacemacs/sync-configuration-layers` (`SPC f e R`
in evil mode, `M-m f e R` in Emacs mode).
`csharp-mode` and `omnisharp` packages will get installed automatically for you
as the configuration is reloaded.


### Installation on Regular Emacs
To install, use [MELPA][].
After MELPA is configured correctly, use

<pre>
M-x package-refresh-contents RET
M-x package-install RET omnisharp RET
</pre>
to install.

When installing the `omnisharp` package `package.el` will also 
automatically pull in `csharp-mode` for you as well.

You can also add
```
(package-install 'omnisharp)
```
to your `init.el` to force installation of `omnisharp-emacs` on
every install.


## Configuration
This section describes an example configuration for `omnisharp-emacs`.
This is not required if you are using spacemacs unless you want to 
override existing bindings, etc.

### Applying omnisharp-mode and auto-starting server when visiting C# files
Add this to `csharp-mode-hook` to your `init.el` to automatically invoke
`omnisharp-emacs` when opening C# files:
```
(add-hook 'csharp-mode-hook 'omnisharp-mode)
```

`omnisharp-emacs` will attempt to start a server automatically for you when
opening a .cs file (if a server has not been started already). Otherwise, you
will need to start the server with `M-x omnisharp-start-omnisharp-server RET`
should it fail to find a .sln file or project root (via projectile). It will
prompt you for a solution file or project root directory you want to work
with.

### Autocompletion
For autocompletion via [company](https://github.com/company-mode/company-mode)
mode to work you will also need this in your `init.el`:
```
(eval-after-load
 'company
 '(add-to-list 'company-backends 'company-omnisharp))

(add-hook 'csharp-mode-hook #'company-mode)
```

Also, for company completion to work  you need to install `company` from 
[melpa](https://melpa.org/#/company). 

### Flycheck
`omnisharp-emacs` supports [Flycheck](https://github.com/flycheck/flycheck)
and it can be enabled automatically by hooking up `flycheck-mode` to be enabled
for `csharp-mode` buffers:

```
(add-hook 'csharp-mode-hook #'flycheck-mode)
```

### Combined setup example

This is an example code that will enable `company-mode` and `flycheck-mode`
and will set some formatting variables for `csharp-mode`. Also, it shows how
to setup keybindings for `csharp-mode`.

```
(eval-after-load
  'company
  '(add-to-list 'company-backends #'company-omnisharp))

(defun my-csharp-mode-setup ()
  (omnisharp-mode)
  (company-mode)
  (flycheck-mode)

  (setq indent-tabs-mode nil)
  (setq c-syntactic-indentation t)
  (c-set-style "ellemtel")
  (setq c-basic-offset 4)
  (setq truncate-lines t)
  (setq tab-width 4)
  (setq evil-shift-width 4)

  ;csharp-mode README.md recommends this too
  ;(electric-pair-mode 1)       ;; Emacs 24
  ;(electric-pair-local-mode 1) ;; Emacs 25

  (local-set-key (kbd "C-c r r") 'omnisharp-run-code-action-refactoring)
  (local-set-key (kbd "C-c C-c") 'recompile))

(add-hook 'csharp-mode-hook 'my-csharp-mode-setup t)
```

There is also an example configuration for evil-mode included in the project,
please see `doc/example-config-for-evil-mode.el`.


## Server Installation
This emacs package requires the [omnisharp-roslyn][] server program.
Emacs will manage connection to the server as a subprocess.

The easiest/default way to install the server is to invoke 
`M-x omnisharp-install-server` and follow instructions on minibuffer.

If that fails (or you feel adventurous) please see 
[installing omnisharp server](doc/server-installation.md) on how to install the
server manually.


## Troubleshooting
Most of the time (if the server has been installed properly) you can diagnose
issues by looking at the `*omnisharp-log*` buffer where `omnisharp-emacs` emits
any log messages from the omnisharp server.


### macOS: Mono.framework not on $PATH
Some projects may fail to load in omnisharp-server when Mono.framework is not
on $PATH or $PATH is not picked up by emacs.

An example output in *omnisharp-log* is:
```
[12:23:33] ERROR: OmniSharp.MSBuild.ProjectFile.ProjectFileInfo, The reference assemblies for
framework ".NETFramework,Version=v3.5" were not found. To resolve this, install the SDK or
Targeting Pack for this framework version or retarget your application to a version of the framework
for which you have the SDK or Targeting Pack installed. Note that assemblies will be resolved from
the Global Assembly Cache (GAC) and will be used in place of reference assemblies. Therefore your
assembly may not be correctly targeted for the framework you intend.
```

See [issue #426](https://github.com/OmniSharp/omnisharp-emacs/issues/426).

### Linux: reference assemblies not found

If you see

```
[13:04:01] ERROR: OmniSharp.MSBuild.ProjectFile.ProjectFileInfo, The reference assemblies for framework ".NETFramework,Version=v4.5.1" were not found. To resolve this, install the SDK or Targeting Pack for this framework version or retarget your application to a version of the framework for which you have the SDK or Targeting Pack installed. Note that assemblies will be resolved from the Global Assembly Cache (GAC) and will be used in place of reference assemblies. Therefore your assembly may not be correctly targeted for the framework you intend.
```

then add the official Mono repository by following the instructions on
https://www.mono-project.com/download/stable/ and install the package
`mono-complete`.

On Debian-based systems:

    sudo apt install --no-install-recommends mono-complete

On Fedora:

    sudo dnf install mono-complete

On CentOS:

    sudo yum install mono-complete

You'll need this even if you've installed the official Microsoft Linux
packages (`dotnet` etc.).


### Missing .NET SDKs
You may find that your project can not be loaded when .NET SDK is not installed 
on your machine.

A log line indicating the problem would look like this on `*omnisharp-log*`:
```
[19:24:59] WARNING: Microsoft.Build.Exceptions.InvalidProjectFileException: The SDK 'Microsoft.NET.Sdk' specified could not be found. ...
```


### Error loading csproj file
You may encounter an issue where omnisharp server fails to load a project, this looks like on `*omnisharp-log*`:
```
[22:46:22] WARNING: OmniSharp.MSBuild.MSBuildProjectSystem, Failed to load project file '/Users/{user}/temp/temp.csproj'.
```

To fix this, on Linux, you may need to install the `msbuild-stable` package.

This issue and a fix has been reported on [issue #430](https://github.com/OmniSharp/omnisharp-emacs/issues/430).


### Server opened in a different program (e.g. wine), instead of mono.
On Linux, it's possible for the plugin to open the server binary, OmniSharp.exe, in a different program than mono, due to [binary format rules](https://en.wikipedia.org/wiki/Binfmt_misc). OmniSharp.exe needs to be passed to mono, but a binfmt rule might override that. 

In the case of wine being used to run OmniSharp.exe, the plugin might trigger a wine desktop to appear, if the prefix is set to emulate one. Additionally, the plugin will issue several errors like this: 
```
omnisharp--handle-server-message error: (wrong-type-argument listp 0). See the OmniServer process buffer for detailed server output.
```

Different distros may manage binfmt a bit differently. To fix this, either consult distro specific documentation and find how to remove the offending rule or set `omnisharp-server-executable-path` to a shell script that explictly calls mono:
```sh
#!/bin/sh
exec mono "[path to omnisharp]/OmniSharp.exe" "$@"
```

This issue and workarounds for the Arch+Wine case have been reported on [issue #477](https://github.com/OmniSharp/omnisharp-emacs/issues/477).  

## Contributing

### How to run tests

You can run all kind of tests by following shell script.

```sh
./run-tests.sh
```

* * * * *

Pull requests welcome!

[omnisharp-roslyn]: https://github.com/OmniSharp/omnisharp-roslyn
[popup.el]: https://github.com/auto-complete/popup-el
[company-mode]: http://company-mode.github.io
[ido-mode]: http://www.emacswiki.org/emacs/InteractivelyDoThings
[Flycheck]: https://github.com/lunaryorn/flycheck
[MELPA]: https://github.com/milkypostman/melpa/#usage
