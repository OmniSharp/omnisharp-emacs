# Installation of the omnisharp-roslyn server application
This emacs package requires the [omnisharp-roslyn](https://github.com/OmniSharp/omnisharp-roslyn) server program.

You have three options here:
  * You can use M-x `omnisharp-install-server` to install omnisharp-server binary automatically. 

    * *NOTE: On Windows, this command requires PowerShell v5+ to be installed
      – see [omnisharp-emacs#275](https://github.com/OmniSharp/omnisharp-emacs/issues/275).*

    * *NOTE 2*: On macOS and Linux omnisharp server binary requires
      [mono](http://www.mono-project.com/) to be installed on your system.

  * Download and extract server binaries
    manually and then point `omnisharp-server-executable-path` variable to the binary.

  * Build the server yourself from the source. 
    Building instructions are detailed in 
    [omnisharp-roslyn building page](https://github.com/OmniSharp/omnisharp-roslyn#building).

## Manual installation on macOS with brew
<pre>
brew install omnisharp/omnisharp-roslyn/omnisharp-mono
</pre>

Then you need to set the `omnisharp-server-executable-path`:

```lisp
(setq omnisharp-server-executable-path "/usr/local/bin/omnisharp")
```

## Manual installation on Linux
Extract binary from [omnisharp-roslyn releases page](https://github.com/OmniSharp/omnisharp-roslyn/releases).

Then you need to set the `omnisharp-server-executable-path`:

```lisp
(setq omnisharp-server-executable-path "<path-to-server-wrapper-script>")
```

## Manual installation on Windows (non-Cygwin)
Use binary from [omnisharp-roslyn releases page](https://github.com/OmniSharp/omnisharp-roslyn/releases).

*NOTE: For the moment you HAVE to use the `omnisharp-win-x86.zip` bundle as -x64- one makes emacs
to crash in `src/w32proc.c:w32_executable_type`.* See https://github.com/OmniSharp/omnisharp-emacs/issues/315

Then you need to set the `omnisharp-server-executable-path` the path
to where you have extracted server file, e.g.:

```lisp
(setq omnisharp-server-executable-path "C:\\Bin\\omnisharp-roslyn\\OmniSharp.exe")
```

## Manual installation on windows (with Cygwin)
Spawning omnisharp-roslyn from cygwin on the microsoft .net framework will result in hangs as described in:

https://cygwin.com/ml/cygwin/2013-12/msg00345.html

To work around this, you can run it on mono.

- Install latest mono runtime http://www.mono-project.com/download/
- Download and unpack mono release from https://github.com/OmniSharp/omnisharp-roslyn/releases
- Create an `omnisharp` shell script like:
```
#!/bin/sh
exec [cygwin path to mono]/mono "$(cygpath -wa [cygwin path to omnisharp]/OmniSharp.exe)" "$@"
``` 
- Set `omnisharp-server-executable-path` to the shell script.
