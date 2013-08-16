# omnisharp-emacs

omnisharp-emacs is a port of the awesome [OmniSharp][] server to the
Emacs text editor. It provides IDE-like features for editing files in
C# solutions in Emacs, provided by an OmniSharp server instance that
works in the background.

## Project maturity
Experimental. Lacks easy setup, a better UI and a good default
configuration.

## Features

* Contextual code completion (i.e. auto-complete / IntelliSense) using
  [popup.el][] or [ido-mode][].
    * Popup.el provides a more sophisticated interface, with a possibility
      to fall back on all of ido's flexible matching power.
    * Also shows documentation like other IDEs
* Show type of the current symbol in the minibuffer
* Navigation helpers
    * Go to definition of a type/variable/method etc.
    * Find usages of the current symbol in the solution
    * Find implementations/derived types of the current type
    * Go to definition of a type in the current file with [ido-mode][]
      (fast).
    * Go to definition of a member in the current type with
      [ido-mode][] (likewise fast :)).
* Rename the current symbol and all references to it
* Solution manipulation
    * Add/remove the current file
    * Add/remove selected files in the dired directory editor
* Override selected superclass member
* Run a refactoring on the current position
    * Uses the refactorings from the NRefactory library, which is also
      used by the MonoDevelop and SharpDevelop IDEs
* Solution building
    * The user may choose whether they want to build in the emacs
      `*compilation*` buffer or at OmniSharp's end (non-asynchronous,
      that is, blocking)
    * Jump to errors like in normal `*compilation*` output
* Format the current buffer
    * Currently only one formatting style supported, easy to add more.
* Syntax checker for parse errors
    * Can be run manually or using the provided [Flycheck][] checker
      in the background.
* OmniSharp server instance manipulation
    * Reload solution
    * Stop server

## Installation

This supports Emacs 24.3 and above at least. It has been tested on
Ubuntu 12.04 (Precise) and on Windows 7.

To install, use [MELPA][].
After MELPA is installed, use

```
M-x package-install omnisharp RET
```
to install.

omnisharp-emacs depends on the external program `curl` for accessing
the background OmniSharp server process. You need to ensure this is
installed and can be found by Emacs.

Start an OmniSharp server process on a solution, and you should have
access to all of this program's functions. You probably need to
create a custom configuration for accessing them in your normal
coding sessions.


* * * * *

Pull requests welcome!

[OmniSharp]: https://github.com/nosami/OmniSharpServer
[popup.el]: https://github.com/auto-complete/popup-el
[ido-mode]: http://www.emacswiki.org/emacs/InteractivelyDoThings
[Flycheck]: https://github.com/lunaryorn/flycheck
[MELPA]: http://melpa.milkbox.net/#installing

