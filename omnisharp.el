;; -*- mode: Emacs-Lisp; lexical-binding: t; -*-
;;; omnisharp.el --- Omnicompletion (intellisense) and more for C#
;; Copyright (C) 2013 Mika Vilpas (GPLv3)
;; Author: Mika Vilpas
;; Version: 3.4
;; Url: https://github.com/sp3ctum/omnisharp-emacs
;; Package-Requires: ((json "1.2") (flycheck "0.21") (dash "2.4.0") (auto-complete "1.4") (popup "0.5.1") (csharp-mode "0.8.7"))
;; Keywords: csharp c# IDE auto-complete intellisense

;;; Commentary:
;; omnisharp-emacs is a port of the awesome OmniSharp server to the
;; Emacs text editor. It provides IDE-like features for editing files
;; in C# solutions in Emacs, provided by an OmniSharp server instance
;; that works in the background.
;;
;; See the project home page for more information.

;; Work in progress! Judge gently!
(require 'dash)
(funcall
 (-lambda ((&alist 'Foo display-text))
          display-text)
 '((Foo . 3)))
