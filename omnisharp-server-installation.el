;; -*- lexical-binding: t -*-

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

(defcustom omnisharp-expected-server-version "1.19.0"
  "Version of the omnisharp-roslyn server that this omnisharp-emacs package
is built for. Also used to select version for automatic server installation."
  :group 'omnisharp
  :type 'string)

(defun omnisharp--server-installation-dir ()
  "Returns installation directory for automatic server installation."
  (f-join (expand-file-name "~") ".emacs.d" ".cache" "omnisharp" "server" (concat "v" omnisharp-expected-server-version)))

(defun omnisharp--server-installation-executable-name ()
  (if (eq system-type 'windows-nt)
      "OmniSharp.exe"
    "omnisharp"))

(defun omnisharp--server-installation-path (&rest ok-if-missing)
  "Returns path to installed omnisharp server binary, if any."
  (let* ((executable-name (omnisharp--server-installation-executable-name))
         (executable-path (f-join (omnisharp--server-installation-dir) executable-name)))
    (if (or (f-exists-p executable-path) ok-if-missing)
        executable-path
      nil)))

(defun omnisharp--server-installation-download-and-extract (url filename reinstall)
  "Downloads and extracts a tgz/zip into it's parent directory."

  ;; remove the file if reinstall is set
  (if (and reinstall (f-exists-p filename))
      (f-delete filename))

  (unless (f-exists-p filename)
    (message (format "omnisharp: downloading server binary from \"%s\"..." url))
    (url-copy-file url filename t))

  (let ((target-dir (f-dirname filename)))
    (message (format "omnisharp: extracting \"%s\" into \"%s\""
                     (f-filename filename)
                     target-dir))

    (if (eq system-type 'windows-nt)
        ;; on windows, we attempt to use powershell v5+, available on Windows 10+
        (let ((powershell-version (substring
                                   (shell-command-to-string "powershell -command \"(Get-Host).Version.Major\"")
                                   0 -1)))
          (if (>= (string-to-number powershell-version) 5)
            (call-process "powershell"
                          nil
                          nil
                          nil
                          "-command"
                          (concat "add-type -assembly system.io.compression.filesystem;"
                                  "[io.compression.zipfile]::ExtractToDirectory(\"" filename "\", \"" target-dir "\")"))

            (message (concat "omnisharp: for the 'M-x omnisharp-install-server' "
                             " command to work on Windows you need to have powershell v5+ installed"))))
      (progn
        (call-process "tar" nil nil t "xf" filename "-C" target-dir)

        ;; setup wrapper script on unix platforms
        (let ((server-exe (omnisharp--server-installation-executable-name)))
          (unless (f-exists-p server-exe)
            (f-write (format (concat "#!/usr/bin/env bash\n"
                                     "MONO_ON_MACOS=\"/Library/Frameworks/Mono.framework/Versions/Current/Commands/mono\"\n"
                                     "if [[ -x \"$MONO_ON_MACOS\" ]]; then\n"
                                     "    # mono may not be on $PATH on macOS, give it a helping hand\n"
                                     "    MONO=\"$MONO_ON_MACOS\"\n"
                                     "else\n"
                                     "    # otherwise, we expect mono to be on $PATH\n"
                                     "    MONO=mono\n"
                                     "fi\n"
                                     "$MONO " (f-join target-dir "OmniSharp.exe") " \"$@\"\n"))
                     'utf-8
                     (f-join target-dir server-exe))
            (set-file-modes (f-join target-dir server-exe) #o755)))))))

(defun omnisharp--server-installation-tarball-name ()
  "Resolves a tarball or zip file to use for this installation.
Note that due to a bug in emacs on Windows we currently use the x86/32bit version.
See https://github.com/OmniSharp/omnisharp-emacs/issues/315"
  (if (eq system-type 'windows-nt)
      "omnisharp-win-x86-net46.zip"
    "omnisharp-mono.tar.gz"))

(defun omnisharp--install-server (reinstall)
  "Implementation for autoloaded omnisharp-install-server in omnisharp.el."
  (let* ((server-dir (omnisharp--server-installation-dir))
         (distro-tarball (omnisharp--server-installation-tarball-name))
         (distro-url (concat "https://github.com/OmniSharp/omnisharp-roslyn/releases/download"
                             "/v" omnisharp-expected-server-version
                             "/" distro-tarball))
         (expected-executable-path (omnisharp--server-installation-path t)))
    (if (or reinstall (not (f-exists-p expected-executable-path)))
        (if (y-or-n-p (format "omnisharp: this will download and extract ~20-30 MB from \"%s\"; do you want to continue?"
                              distro-url))
            (progn
              (message (format "omnisharp: attempting to download and install OmniSharp server into %s"
                               server-dir))
              (omnisharp--mkdirp server-dir)
              (omnisharp--server-installation-download-and-extract
               distro-url
               (f-join server-dir distro-tarball)
               reinstall)
              (let ((executable-path (omnisharp--server-installation-path)))
                (if executable-path
                    (message (format "omnisharp: server was installed to \"%s\"; you can now do M-x 'omnisharp-start-omnisharp-server' "
                                     executable-path))
                  (message (concat "omnisharp: server could not be installed automatically. "
                                   "Please check https://github.com/OmniSharp/omnisharp-emacs/blob/master/doc/server-installation.md for instructions."))))))
      (message (format "omnisharp: server is already installed (%s)"
                       expected-executable-path)))))

(provide 'omnisharp-server-installation)
