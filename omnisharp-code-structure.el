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

(require 'dash)

(defun omnisharp--cs-inspect-buffer (callback)
  "Calls into the /v2/codestructure endpoint to retrieve code structure for
the current buffer from the server and invokes CALLBACK with single argument
that contains sequence of elements retrieved.

Element sequence is hierarchical -- see the 'Children property for each element
to inspect it resursively or invoke 'omnisharp--cs-filter-resursively on elements
to grab the things you need out of the tree."

  (omnisharp--send-command-to-server
   "/v2/codestructure"
   (omnisharp--get-request-object)
   (-lambda ((&alist 'Elements elements))
     (funcall callback elements))))


(defun omnisharp--cs-inspect-elements-recursively (fn elements)
  "Invokes FN on each of elements on the ELEMENTS tree
in a depth-first fashion."
  (seq-each
   (lambda (el)
     (funcall fn el)
     (-let* (((&alist 'Children children) el))
       (omnisharp--cs-inspect-elements-recursively fn children)))
   elements))


(defun omnisharp--cs-filter-resursively (predicate elements)
  "Filters out code elements in the sequence given and returns
a list of elements that match the predicate given."

  (let ((results nil))
    (omnisharp--cs-inspect-elements-recursively
     (lambda (el)
       (if (funcall predicate el)
           (setq results (cons el results))))
     elements)
    results))


(defun omnisharp--cs-l-c-within-range (l c range)
  "Returns 't when L (line) and C (column) are within the RANGE."

  (-let* (((&alist 'Start start 'End end) range)
          ((&alist 'Line start-l 'Column start-c) start)
          ((&alist 'Line end-l 'Column end-c) end))
    (or (and (= l start-l) (>= c start-c) (or (> end-l start-l) (<= c end-c)))
        (and (> l start-l) (< l end-l))
        (and (= l end-l) (<= c end-c)))))


(defun omnisharp--cs-element-stack-on-l-c (l c elements)
  "Returns a list of elements that enclose a point in file specified
by L (line) and C (column). If the point is enclosed by any of the ELEMENTS
the result contains hierarchical list of namespace, class and [method|field]
 elements."

  (let ((matching-element (seq-find (lambda (el)
                                      (-let* (((&alist 'Ranges ranges) el)
                                              ((&alist 'full full-range) ranges))
                                        (omnisharp--cs-l-c-within-range l c full-range)))
                                    elements)))
    (if matching-element
        (-let (((&alist 'Children children) matching-element))
          (cons matching-element (omnisharp--cs-element-stack-on-l-c l c children))))))


(defun omnisharp--cs-element-stack-at-point (callback)
  "Invokes callback with a stack of code elements on point in the current buffer"
  (omnisharp--cs-inspect-buffer
   (lambda (elements)
     (let ((pos-line (line-number-at-pos))
           (pos-col (current-column)))
       (funcall callback
                (omnisharp--cs-element-stack-on-l-c pos-line pos-col elements))))))


(defun omnisharp--cs-unit-test-method-p (el)
  "Returns a list (test-method-name test-framework) if the element
given is a test method, nil otherwise."

  (-let* (((&alist 'Kind kind
                   'Properties properties) el)
          ((&alist 'testMethodName test-method-name
                   'testFramework test-framework) properties))
    (if (and test-method-name test-framework)
        (list test-method-name test-framework))))


(provide 'omnisharp-code-structure)

