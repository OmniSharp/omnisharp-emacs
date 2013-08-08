# omnisharp-emacs

omnisharp-emacs is a port of the awesome [OmniSharp][] server to the
Emacs text editor. It provides IDE-like features for editing files in
C# solutions in Emacs, provided by an OmniSharp server instance that
works in the background.

##Features

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
      `*compilation*` buffer or at OmniSharp's end (non-asynchronous:
      blocking)
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
Installation is currently laborous at best.

This supports Emacs 24.3 and above at least. It has been tested on
Ubuntu 12.04 (Precise) only.

omnisharp-emacs depends on the external program `curl` for accessing
the background OmniSharp server process. You need to ensure this is
installed and can be found by Emacs.

Start an OmniSharp server process on a solution, and load-file
omnisharp.el. Then you should have access to all of this program's
functions. You probably need to create a custom configuration for
accessing them in your normal coding sessions.

You can package-install all dependencies in the source file.

* * * * *

Pull requests welcome!

[OmniSharp]: https://github.com/nosami/OmniSharpServer
[popup.el]: https://github.com/auto-complete/popup-el
[ido-mode]: http://www.emacswiki.org/emacs/InteractivelyDoThings
[Flycheck]: https://github.com/lunaryorn/flycheck

