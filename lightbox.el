;;; lightbox.el --- inline documentation using overlay boxes

;; Copyright (C) 2013 Alvin Francis Dumalus

;; Author: Alvin Francis Dumalus <alvin.francis.dumalus@gmail.com>
;; Maintainer: Alvin Francis Dumalus <alvin.francis.dumalus@gmail.com>
;; Keywords: lisp
;; Version: 0.0.20140303
;; Created: 01 March 2014

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Inline documentation (ala Light Table) using overlays. Currently targets elisp.

;;; Code:

(defun lightbox-doc ()
  "Create lightbox for the function at point."
  (interactive)
  (when (plist-get (text-properties-at (point)) 'lightbox)
    (lightbox-remove-next-line-overlay))
  (let ((fn (function-called-at-point)))
    (let ((start (save-excursion (beginning-of-line) (point)))
          (end (save-excursion (end-of-line) (point))))
      (put-text-property start end 'lightbox t)
      (put-text-property
       start end 'modification-hooks
       (list (lambda (beg end)
               (remove-overlays
                (lightbox--anchor-fn) (lightbox--anchor-fn)
                'lightbox t)))))
    (lightbox-create-box-overlay
     (lightbox--create-box-text (lightbox--get-function-summary fn)
                                (- (save-excursion
                                     (beginning-of-thing 'symbol)
                                     (point))
                                   (save-excursion
                                     (beginning-of-line)
                                     (point))))
     (lightbox--anchor-fn))))

(cl-defun lightbox--create-box-text (text &optional (offset 0))
  "Return text with attendant properties."
  (let* ((lines (split-string text "\n"))
         (max-line-length (lightbox--maximum-line-length lines)))
    (concat
     (mapconcat (lambda (x)
                  (concat
                   (make-string offset ?\ )
                   (lightbox--create-box-line x (+ max-line-length 2))))
                (split-string text "\n")
                "\n")
     "\n")))

(defun lightbox-create-box-overlay (text pos)
  "Create a lightbox overlay at POS containing TEXT."
  (let ((o (make-overlay pos pos)))
    (overlay-put o 'before-string text)
    (overlay-put o 'lightbox t)))

;; Helper functions
(defun lightbox--maximum-line-length (lines)
  (cl-loop for line in lines maximizing (length line)))

(defun lightbox--create-box-line (text length)
  (let* ((len (length text))
         (offset (- length len)))
    (propertize (concat " " text
                        (make-string offset ? ))
                'face font-lock-doc-face)))

(defun lightbox--get-function-summary (function)
  "Return the first few paragraphs of the documentation of FUNCTION (a symbol).
Note that the summary text will also be fill-paragraphed and justified."
  (if (null function)
      (message "You didn't specify a function")
    (save-excursion
      (with-temp-buffer
        (let ((standard-output (current-buffer)))
          (prin1 function)
          ;; Use " is " instead of a colon so that
          ;; it is easier to get out the function name using forward-sexp.
          (princ " is ")
          (describe-function-1 function)
          (buffer-substring (point-min)
                            (save-excursion
                              (beginning-of-buffer)
                              (dotimes (i 3 (funcall (lambda ()
                                                       (when (string= "\n" (thing-at-point 'line))
                                                         (backward-char))
                                                       (point))))
                                (fill-paragraph)
                                (forward-paragraph)))))))))

(defun lightbox-remove-all-overlays ()
  "Remove all lightboxes in buffer."
  (interactive)
  (lightbox--remove-overlays (point-min) (point-max)))

(defun lightbox-remove-next-line-overlay ()
  "Remove lightbox anchored to current line."
  (interactive)
  (lightbox--remove-overlays
   (save-excursion
     (beginning-of-line)
     (point))
   (lightbox--anchor-fn)))

(defun lightbox--remove-overlays (start end)
  "Remove lightbox overlays from START to END."
  (remove-overlays
   start end
   'lightbox t))

(defun lightbox--anchor-fn ()
  "Function to run which will return the anchor point for
  lightbox-doc."
  (save-excursion
    (forward-line)
    (point)))

(provide 'lightbox)

;;; lightbox.el ends here
