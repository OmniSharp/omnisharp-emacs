;; You can use this in your emacs session to start developing, as the first
;; thing you want to do is load the development istance your emacs session.

;; So to get started you must
;; M-x eval-buffer (with this file open)
;; evaluate the `omnisharp--load-development-files` form at the bottom manually

(defvar root-directory (-> (f-this-file)
                           f-parent
                           f-parent))

(defun el-file-or-directory? (f)
  (or (f-directory? f) (equal "el" (f-ext f))))

(defun omnisharp--files (path)
  (let ((path (f-join root-directory path))
        (recursive? t))
    (if (f-directory? path)
        (f-files path 'el-file-or-directory? recursive?)
      path)))

(defun omnisharp--load-development-files (&rest paths)
  (let ((files (->> paths
                    (-map 'omnisharp--files)
                    (-flatten))))
    (-each files 'load-file)))

(when nil
  ;; evaluate this manually
  (omnisharp--load-development-files "src"
                                     "features"
                                     "omnisharp.el"
                                     "test"))
