# omnisharp-emacs
<!--
disabled for now, for the roslyn version

<a href="//travis-ci.org/OmniSharp/omnisharp-emacs">
    <img src="https://travis-ci.org/OmniSharp/omnisharp-emacs.svg?branch=master" />
</a>
-->

omnisharp-emacs is a port of the awesome [omnisharp-roslyn][] server to the
Emacs text editor. It provides IDE-like features for editing files in
C# solutions in Emacs, provided by an OmniSharp server instance that
works in the background.

See [omnisharp-emacs Features](doc/features.md).

## Installation of the omnisharp-roslyn server application
This emacs package requires the [omnisharp-roslyn][] server program.
You can build the server yourself from the source or get precompiled 
binaries from the [omnisharp-roslyn releases](https://github.com/OmniSharp/omnisharp-roslyn/releases) page.

The server must be a recent one, e.g. at least from year 2016.
If you haven't updated your server copy since that, you must upgrade.

You can install the server from source as detailed on [omnisharp-roslyn building page](https://github.com/OmniSharp/omnisharp-roslyn#building). Or use the following instructions.

 * TODO: there are plans to add automatic server installation/update mechanism to `omnisharp-emacs` eventually. See [omnisharp-emacs#275](https://github.com/OmniSharp/omnisharp-emacs/issues/275). But for now you have to install it manually.

### On macOS with brew
<pre>
brew install omnisharp/omnisharp-roslyn/omnisharp-mono
</pre>

Then you need to set the `omnisharp-server-executable-path`:

```lisp
(setq omnisharp-server-executable-path "/usr/local/bin/omnisharp")
```

### On linux
Extract binary from [omnisharp-roslyn releases page](https://github.com/OmniSharp/omnisharp-roslyn/releases).

### On Windows (non-Cygwin)
Use binary from [omnisharp-roslyn releases page](https://github.com/OmniSharp/omnisharp-roslyn/releases).

Then you need to set the `omnisharp-server-executable-path` the path
to where you have extracted server file, e.g.:

```lisp
(setq omnisharp-server-executable-path "C:\\Bin\\omnisharp-roslyn\\OmniSharp.exe")
```

### On Windows (with Cygwin)
Use binary from [omnisharp-roslyn releases page](https://github.com/OmniSharp/omnisharp-roslyn/releases):

 - https://github.com/OmniSharp/omnisharp-roslyn/releases/download/v1.9-beta22/omnisharp-win-x64-net46.zip
 
If I use omnisharp-roslyn net46 directly, omnisharp-emacs hangs when interacting via stdio.
This seems to be a difference in how console IO is handled in newer versions of .Net.
My workaround involves using a wrapper written in C#:

https://gist.github.com/corngood/d982c3c21c016127a2f1600dc895c000

You need to compile the wrapper against an older .net framework (3.5 seems to work). A simple way to use the wrapper is to create a shell script like:

```shell
#!/bin/sh
set -e
[path-to-wrapper-exe] "$(cygpath -w [path-to-omnisharp-exe])" "$@"
```

If you name this script OmniSharp, and put it in your path (e.g. /usr/local/bin/OmniSharp),
`omnisharp-emacs` should find it and launch omnisharp correctly.
Or you can point to it directly in your init.el

```lisp
(setq omnisharp-server-executable-path "/home/<username>/bin/OmniSharp")
```

## Package Installation and Configuration
This package requires Emacs 24.3 and above. It has been tested on
Ubuntu, Windows 7+ and on macOS.

### Installation on Spacemacs
Add `csharp` layer to `dotspacemacs-configuration-layers` on
your `.spacemacs` file. `csharp-mode` and `omnisharp` packages
will get installed automatically on restart for you.

 * TODO: as of 2017-02-19 `csharp` layer installs non-roslyn version of omnisharp-emacs.

### Installation on Regular Emacs
To install, use [MELPA][].
After MELPA is configured correctly, use

<pre>
M-x package-refresh-contents RET
M-x package-install omnisharp RET
</pre>
to install.

When installing the `omnisharp` package `package.el` will also 
automatically pull in `csharp-mode` for you as well.

To automatically load omnisharp-emacs when editing csharp files, add
something like this to `csharp-mode-hook` on your `init.el`:

```
(add-hook 'csharp-mode-hook 'omnisharp-mode)
```

## Configuration and commands available
To start it, use `M-x omnisharp-start-omnisharp-server RET`.
The command will ask you for a solution file or a directory 
you want to code in.

Emacs will manage connection to the server as a subprocess.

You will probably want to create a custom configuration for accessing
omnisharp-emacs in your normal coding sessions. There is an example
configuration for evil-mode included in the project.

* * * * *

Pull requests welcome!

[omnisharp-roslyn]: https://github.com/OmniSharp/omnisharp-roslyn
[popup.el]: https://github.com/auto-complete/popup-el
[company-mode]: http://company-mode.github.io
[ido-mode]: http://www.emacswiki.org/emacs/InteractivelyDoThings
[Flycheck]: https://github.com/lunaryorn/flycheck
[MELPA]: https://github.com/milkypostman/melpa/#usage
