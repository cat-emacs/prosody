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

;; Adds the `:font-rule' keyword to `use-package'.

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

(defun use-package-normalize/:font-rule (name _keyword args)
  "Normalize a `:font-rule' declaration for package NAME."
  (use-package-only-one ":font-rule" args
    (lambda (_label arg)
      (let* ((spec (cond
                    ((symbolp arg) (list :font arg))
                    ((and (listp arg) (keywordp (car arg))) arg)
                    ((and (consp arg) (symbolp (car arg)))
                     (cons :font arg))
                    (t (use-package-error
                        ":font-rule expects ROLE or (ROLE :KEY VALUE...)"))))
             (allowed '(:modes :buffer-name :font :faces :rescale))
             (tail spec))
        (unless (zerop (% (length spec) 2))
          (use-package-error ":font-rule expects a property list"))
        (while tail
          (unless (memq (pop tail) allowed)
            (use-package-error ":font-rule contains an unknown property"))
          (pop tail))
        (unless (or (plist-member spec :modes)
                    (plist-member spec :buffer-name))
          (setq spec
                (plist-put spec :modes
                           (prosody-use-package--default-mode name))))
        (let ((buffer-name-present-p (plist-member spec :buffer-name))
              (buffer-name (plist-get spec :buffer-name))
              (font-present-p (plist-member spec :font))
              (role (plist-get spec :font))
              (faces-present-p (plist-member spec :faces))
              (faces (plist-get spec :faces)))
          (when (and buffer-name-present-p (not (stringp buffer-name)))
            (use-package-error ":font-rule :buffer-name must be a regexp string"))
          (when (and font-present-p
                     (not (and role (symbolp role))))
            (use-package-error ":font-rule :font must name a role"))
          (when (and faces-present-p (not (listp faces)))
            (use-package-error ":font-rule :faces must be a list"))
          (dolist (face-rule faces)
            (unless (and (consp face-rule)
                         (symbolp (car face-rule))
                         (cadr face-rule)
                         (symbolp (cadr face-rule)))
              (use-package-error
               ":font-rule face rules must have the form (FACE ROLE ...)")))
          (unless (or (and font-present-p role)
                      (and faces-present-p faces))
            (use-package-error ":font-rule requires :font or :faces")))
        spec))))

(defun use-package-handler/:font-rule (name _keyword rule rest state)
  "Register normalized font RULE for package NAME."
  (use-package-concat
   `((prosody-register ',name ',rule))
   (use-package-process-keywords name rest state)))

(defun prosody-use-package--position-keyword ()
  "Process `:font-rule' eagerly after conditional keywords."
  (setq use-package-keywords (delq :font-rule use-package-keywords))
  (let ((position (or (cl-position :catch use-package-keywords)
                      (length use-package-keywords))))
    (setq use-package-keywords
          (append (seq-take use-package-keywords position)
                  (list :font-rule)
                  (seq-drop use-package-keywords position)))))

(put 'use-package-handler/:font-rule
     'function-documentation
     "Register a package's semantic font rule")
(prosody-use-package--position-keyword)

;; `use-package-ensure' can prepend keywords when it loads.
(with-eval-after-load 'use-package-ensure
  (prosody-use-package--position-keyword))

(provide 'prosody-use-package)

;;; prosody-use-package.el ends here
