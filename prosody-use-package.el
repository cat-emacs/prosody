;;; prosody-use-package.el --- use-package integration for Prosody -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Misaka

;; Author: Misaka <chuxubank@qq.com>
;; Maintainer: Misaka <chuxubank@qq.com>
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.4"))
;; Keywords: faces, convenience
;; URL: https://github.com/cat-emacs/prosody

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;; Commentary:

;; Adds the `:font-role' keyword to `use-package'.

;;; Code:

(require 'cl-lib)
(require 'prosody)
(require 'seq)
(require 'subr-x)
(require 'use-package-core)

(defun prosody-use-package--default-mode (name)
  "Return the conventional major mode associated with package NAME."
  (let ((name (symbol-name name)))
    (intern (if (string-suffix-p "-mode" name)
                name
              (concat name "-mode")))))

(defun use-package-normalize/:font-role (name _keyword args)
  "Normalize a `:font-role' declaration for package NAME."
  (use-package-only-one ":font-role" args
    (lambda (_label arg)
      (let* ((spec (cond
                    ((symbolp arg) (list :font arg))
                    ((and (listp arg) (keywordp (car arg))) arg)
                    ((and (consp arg) (symbolp (car arg)))
                     (cons :font arg))
                    (t (use-package-error
                        ":font-role expects ROLE or (ROLE :KEY VALUE...)"))))
             (allowed '(:modes :font :faces :rescale))
             (tail spec))
        (unless (zerop (% (length spec) 2))
          (use-package-error ":font-role expects a property list"))
        (while tail
          (unless (memq (pop tail) allowed)
            (use-package-error ":font-role contains an unknown property"))
          (pop tail))
        (unless (plist-member spec :modes)
          (setq spec
                (plist-put spec :modes
                           (prosody-use-package--default-mode name))))
        (let ((font-present-p (plist-member spec :font))
              (role (plist-get spec :font))
              (faces-present-p (plist-member spec :faces))
              (faces (plist-get spec :faces)))
          (when (and font-present-p
                     (not (and role (symbolp role))))
            (use-package-error ":font-role :font must name a role"))
          (when (and faces-present-p (not (listp faces)))
            (use-package-error ":font-role :faces must be a list"))
          (dolist (face-rule faces)
            (unless (and (consp face-rule)
                         (symbolp (car face-rule))
                         (cadr face-rule)
                         (symbolp (cadr face-rule)))
              (use-package-error
               ":font-role face rules must have the form (FACE ROLE ...)")))
          (unless (or (and font-present-p role)
                      (and faces-present-p faces))
            (use-package-error ":font-role requires :font or :faces")))
        spec))))

(defun use-package-handler/:font-role (name _keyword rule rest state)
  "Register normalized font RULE for package NAME."
  (use-package-concat
   `((prosody-register ',name ',rule))
   (use-package-process-keywords name rest state)))

(defun prosody-use-package--position-keyword ()
  "Process `:font-role' eagerly after conditional keywords."
  (setq use-package-keywords (delq :font-role use-package-keywords))
  (let ((position (or (cl-position :catch use-package-keywords)
                      (length use-package-keywords))))
    (setq use-package-keywords
          (append (seq-take use-package-keywords position)
                  (list :font-role)
                  (seq-drop use-package-keywords position)))))

(put 'use-package-handler/:font-role
     'function-documentation
     "Register semantic font roles for a package's major modes")
(prosody-use-package--position-keyword)

;; `use-package-ensure' can prepend keywords when it loads.
(with-eval-after-load 'use-package-ensure
  (prosody-use-package--position-keyword))

(provide 'prosody-use-package)

;;; prosody-use-package.el ends here
