;; -*- lexical-binding: t -*-

(defcustom omnisharp-expected-server-version "1.19.0"
  "Version of the omnisharp-roslyn server that this omnisharp-emacs package
is built for. Also used to select version for automatic server installation."
  :group 'omnisharp
  :type 'string)

(defun omnisharp--server-installation-dir ()
  "Returns installation directory for automatic server installation."
  (f-join (expand-file-name "~") ".emacs.d" ".cache" "omnisharp" "server" (concat "v" omnisharp-expected-server-version)))

(defun omnisharp--server-installation-path (&rest ok-if-missing)
  "Returns path to installed omnisharp server binary, if any."
  (let* ((executable-name "omnisharp") ;; TODO: platform dependant
         (executable-path (f-join (omnisharp--server-installation-dir) executable-name)))
    (if (or (f-exists-p executable-path) ok-if-missing)
        executable-path
      nil)))

(defun omnisharp--server-installation-download-and-extract (url filename reinstall)
  "Downloads and extracts a tgz binary into given directory."

  ;; remove the file if reinstall is set
  (if (and reinstall (f-exists-p filename))
      (f-delete filename))

  (unless (f-exists-p filename)
    (message (format "omnisharp: downloading server binary from \"%s\"..." url))
    (url-copy-file url filename t))

  (message (format "omnisharp: extracting into %s" (f-dirname filename)))
  (call-process "tar" nil nil t "xf" filename "-C" (f-dirname filename)))

(defun omnisharp--server-installation-prepare-wrapper-script (filename server-dir)
  (unless (f-exists-p filename)
    (f-write (format "#!/usr/bin/env bash\nmono %s $@\n" (f-join server-dir "OmniSharp.exe"))
             'utf-8
             (f-join server-dir filename))
    (call-process "chmod" nil nil nil "0755" (f-join server-dir filename))))

(defun omnisharp--install-server (reinstall)
  "Implementation for autoloaded omnisharp-install-server in omnisharp.el."
  (let* ((server-dir (omnisharp--server-installation-dir))
         (distro-tarball "omnisharp-mono.tar.gz") ;; TODO: platform specific
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
              (omnisharp--server-installation-prepare-wrapper-script "omnisharp" server-dir)
              (let ((executable-path (omnisharp--server-installation-path)))
                (if executable-path
                    (message (format "omnisharp: server was installed to \"%s\"; you can now do M-x 'omnisharp-start-omnisharp-server' "
                                     executable-path))
                  (message (concat "omnisharp: server could not be installed automatically."
                                   "Please check https://github.com/OmniSharp/omnisharp-emacs/blob/master/README.md#installation-of-the-omnisharp-roslyn-server-application for instructions."))))))
      (message (format "omnisharp: server is already installed (%s)"
                       expected-executable-path)))))

(provide 'omnisharp-server-installation)
