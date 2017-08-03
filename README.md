# omnisharp-emacs
<a href="//travis-ci.org/OmniSharp/omnisharp-emacs">
    <img src="https://travis-ci.org/OmniSharp/omnisharp-emacs.svg?branch=master" />
</a>

omnisharp-emacs is a port of the awesome [omnisharp-roslyn][] server to the
Emacs text editor. It provides IDE-like features for editing files in
C# solutions in Emacs, provided by an OmniSharp server instance that
works in the background.

This package is licensed under GNU General Public License version 3, or (at your option) any later version.

See [omnisharp-emacs Features](doc/features.md).

## Package Installation
This package requires Emacs 24.3 and above. It has been tested on
Ubuntu, Windows 7+ and on macOS.

### Installation on Spacemacs
Add `csharp` layer to `dotspacemacs-configuration-layers` on
your `.spacemacs` file. `csharp-mode` and `omnisharp` packages
will get installed automatically for you on restart.

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

For autocompletion via company mode you will also need this in your `init.el`:

```
(eval-after-load
 'company
 '(add-to-list 'company-backends 'company-omnisharp))
```

## Server Installation
This emacs package requires the [omnisharp-roslyn][] server program.
Emacs will manage connection to the server as a subprocess.

The easiest way to install the server is to invoke 
`M-x omnisharp-install-server` and follow instructions on minibufer.

If that fails (or you feel adventurous) please see [installing omnisharp server](doc/server-installation.md) 
on how to install the server manually.

## Configuration
To start using omnisharp-emacs, start the server with `M-x omnisharp-start-omnisharp-server RET`.
The command will ask you for a project or solution file you want to code in.

You will probably want to create a custom configuration for accessing
omnisharp-emacs in your normal coding sessions.
Usually all this customization goes in your custom `csharp-mode-hook` in your `init.el`.

There is also an example configuration for evil-mode included in the project,
please see `doc/example-config-for-evil-mode.el`.


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
