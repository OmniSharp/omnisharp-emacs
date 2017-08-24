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

## Configuration
`omnisharp-emacs` will attempt to start a server automatically for you when
opening a .cs file (if a server has not been started already). Otherwise, you
will need to start the server with `M-x omnisharp-start-omnisharp-server RET`.
should it fail to find a .sln file or project root (via projectile). The command
will prompt you for a solution file or project root directory you want to work
with.

You will probably want to create a custom configuration for omnisharp-emacs
in your normal coding sessions. Usually all this customization
goes in your custom `csharp-mode-hook`.

Here is a sample configuration script (that goes into your `~/.emacs.d/init.el`
or in your `dotspacemacs/user-config()` in `~/.spacemacs`):

```
  (defun my-csharp-mode-setup ()
    (setq indent-tabs-mode nil)
    (setq c-syntactic-indentation t)
    (c-set-style "ellemtel")
    (setq c-basic-offset 4)
    (setq truncate-lines t)
    (setq tab-width 4)
    (setq evil-shift-width 4)
    (local-set-key (kbd "C-c C-c") 'recompile))

  (add-hook 'csharp-mode-hook 'my-csharp-mode-setup t)
```

There is also an example configuration for evil-mode included in the project,
please see `doc/example-config-for-evil-mode.el`.


## Server Installation
This emacs package requires the [omnisharp-roslyn][] server program.
Emacs will manage connection to the server as a subprocess.

The easiest/default way to install the server is to invoke 
`M-x omnisharp-install-server` and follow instructions on minibufer.

If that fails (or you feel adventurous) please see [installing omnisharp server](doc/server-installation.md) 
on how to install the server manually.


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
