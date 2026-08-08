;;; prosody-nerd-icons.el --- Nerd Icons integration for Prosody -*- lexical-binding: t; -*-

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

;; Adds Nerd Fonts private-use ranges to every generated Prosody fontset.

;;; Code:

(require 'cl-lib)
(require 'prosody)

(defvar nerd-icons-font-family)

(defvar prosody-nerd-icons--fontset-entry-cache nil
  "Fontset entries discovered from `nerd-icons-set-font'.")

(defvar prosody-nerd-icons--default-configurations nil
  "Frame, fontset, and Nerd Font triples already configured.")

(defun prosody-nerd-icons--fontset-entries ()
  "Return fontset entries maintained by Nerd Icons."
  (or prosody-nerd-icons--fontset-entry-cache
      (progn
        (require 'nerd-icons)
        (let (entries)
          (cl-letf (((symbol-function 'set-fontset-font)
                     (lambda (_fontset characters _font-spec
                                       &optional _frame add)
                       (push (cons characters add) entries))))
            (nerd-icons-set-font))
          (unless entries
            (error "Nerd Icons provided no fontset entries"))
          (setq prosody-nerd-icons--fontset-entry-cache
                (nreverse entries))))))

(defun prosody-nerd-icons--rules (_role)
  "Return Nerd Icons fontset rules for a Prosody role."
  (require 'nerd-icons)
  (mapcar (lambda (entry)
            (list (list (car entry))
                  (list nerd-icons-font-family)
                  (cdr entry)))
          (prosody-nerd-icons--fontset-entries)))

(defun prosody-nerd-icons--setup-default-fontset (&optional frame)
  "Configure Nerd Icons once for graphical FRAME's active fontset."
  (let* ((frame (or frame (selected-frame)))
         (fontset (and (display-graphic-p frame)
                       (frame-parameter frame 'font))))
    (when fontset
      (require 'nerd-icons)
      (let ((configuration (list frame fontset nerd-icons-font-family)))
        (unless (member configuration
                        prosody-nerd-icons--default-configurations)
          (with-selected-frame frame
            (nerd-icons-set-font nil frame))
          (push configuration
                prosody-nerd-icons--default-configurations))))))

(prosody-add-fontset-rule-function #'prosody-nerd-icons--rules)
(add-hook 'prosody-setup-hook
          #'prosody-nerd-icons--setup-default-fontset)

(provide 'prosody-nerd-icons)

;;; prosody-nerd-icons.el ends here
