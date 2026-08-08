;;; prosody.el --- Semantic typography for Emacs buffers -*- lexical-binding: t; -*-

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

;; Prosody assigns semantic font roles to major modes and faces.  Roles resolve
;; through reusable font stacks into per-role fontsets, so Latin, CJK, symbols,
;; mathematics, and emoji can follow the same buffer-local typography preset.

;;; Code:

(require 'cl-lib)
(require 'face-remap)
(require 'seq)
(require 'subr-x)

(defgroup prosody nil
  "Semantic typography for Emacs buffers."
  :group 'faces
  :prefix "prosody-")

(defvar prosody-rule-alist nil
  "Registered mode and face rules in declaration order.")

(defvar prosody-stacks)
(defvar prosody-roles)
(defvar prosody-presets)
(defvar prosody-mode-rules)

(defun prosody-register (owner rule)
  "Register OWNER's font RULE, replacing its previous declaration."
  (when (fboundp 'prosody-validate-rule)
    (prosody-validate-rule owner rule))
  (if-let* ((entry (assq owner prosody-rule-alist)))
      (setcdr entry rule)
    (setq prosody-rule-alist
          (append prosody-rule-alist (list (cons owner rule))))))

(defun prosody-rules ()
  "Return a copy of all registered font rules."
  (copy-tree prosody-rule-alist))

(defun prosody-clear-rules ()
  "Remove all registered font rules."
  (setq prosody-rule-alist nil))

(defun prosody-restore-rules (rules)
  "Replace registered font rules with a copy of RULES."
  (setq prosody-rule-alist (copy-tree rules)))

(defconst prosody--role-override-properties
  '(:stack :fonts :height :weight :slant :width)
  "Properties accepted in named and buffer-local role overrides.")

(defvar prosody--configuration-version 0
  "Generation number of the current font configuration.")

(defun prosody--valid-face-attribute-value-p (property value)
  "Return non-nil when VALUE is valid for face PROPERTY."
  (let ((table (pcase property
                 (:weight font-weight-table)
                 (:slant font-slant-table)
                 (:width font-width-table))))
    (and (symbolp value)
         (seq-some (lambda (entry) (seq-contains-p entry value)) table))))

(defun prosody--validate-role-overrides
    (owner overrides &optional preset stacks)
  "Validate OWNER's role OVERRIDES against PRESET and STACKS."
  (let ((preset (or preset prosody-roles))
        (stacks (or stacks prosody-stacks))
        roles)
    (unless (proper-list-p overrides)
      (error "Font role overrides for %S must be a list" owner))
    (dolist (entry overrides)
      (unless (and (consp entry)
                   (symbolp (car entry))
                   (proper-list-p (cdr entry))
                   (zerop (% (length (cdr entry)) 2)))
        (error "Invalid font role override for %S: %S" owner entry))
      (let ((role (car entry))
            (properties (cdr entry)))
        (unless (assq role preset)
          (error "Unknown font role %S in preset %S" role owner))
        (when (memq role roles)
          (error "Duplicate font role %S in preset %S" role owner))
        (push role roles)
        (cl-loop for (property value) on properties by #'cddr
                 unless (memq property prosody--role-override-properties)
                 do (error "Unsupported font role property %S in preset %S"
                           property owner)
                 when (and (eq property :stack)
                           (not (assq value stacks)))
                 do (error "Unknown font stack %S in preset %S" value owner)
                 when (and (eq property :fonts)
                           (not (and (proper-list-p value)
                                     (seq-every-p #'stringp value))))
                 do (error ":fonts must be a list of strings in preset %S"
                           owner)
                 when (and (eq property :height)
                           (not (and (numberp value) (> value 0))))
                 do (error ":height must be positive in preset %S" owner)
                 when (and (memq property '(:weight :slant :width))
                           (not (prosody--valid-face-attribute-value-p
                                 property value)))
                 do (error "Invalid %S value %S in preset %S"
                           property value owner)))))
  overrides)

(defun prosody--validate-presets (presets &optional preset stacks)
  "Validate named PRESETS against the base PRESET and STACKS."
  (unless (proper-list-p presets)
    (error "Font presets must be a list"))
  (let (names)
    (dolist (entry presets)
      (unless (and (consp entry) (symbolp (car entry)))
        (error "Invalid named font preset: %S" entry))
      (when (memq (car entry) names)
        (error "Duplicate font preset %S" (car entry)))
      (push (car entry) names)
      (prosody--validate-role-overrides
       (car entry) (cdr entry) preset stacks)))
  presets)

(defun prosody--configuration-changed ()
  "Invalidate generated font state and refresh visible buffers."
  (cl-incf prosody--configuration-version)
  (when (boundp 'prosody--fontset-signatures)
    (clrhash prosody--fontset-signatures))
  (when (boundp 'prosody--role-face-signatures)
    (clrhash prosody--role-face-signatures))
  (when (fboundp 'prosody-setup)
    (if (display-graphic-p)
        (prosody-setup)
      (when (fboundp 'prosody--refresh-mode-fonts)
        (prosody--refresh-mode-fonts)))))

(defun prosody--rule-roles (rule)
  "Return semantic font roles referenced by RULE."
  (delq nil
        (cons (when (symbolp (plist-get rule :font))
                (plist-get rule :font))
              (mapcar (lambda (face-rule)
                        (when (symbolp (cadr face-rule))
                          (cadr face-rule)))
                      (plist-get rule :faces)))))

(defun prosody-validate-rule (owner rule &optional preset)
  "Validate roles and stepped faces in OWNER's font RULE against PRESET."
  (let ((preset (or preset prosody-roles)))
    (dolist (role (prosody--rule-roles rule))
      (unless (assq role preset)
        (error "Unknown font role %S in font rule for %S" role owner)))
    (dolist (face-rule (plist-get rule :faces))
      (let ((attributes (cddr face-rule)))
        (unless (zerop (% (length attributes) 2))
          (error "Invalid face attributes in font rule for %S" owner))
        (dolist (property '(:height-step :weight-step))
          (when (and (plist-member attributes property)
                     (not (numberp (plist-get attributes property))))
            (error "%S must be numeric in font rule for %S"
                   property owner))))))
  rule)

(defun prosody--set-preset (symbol value)
  "Set SYMBOL to VALUE after validating registered font rules."
  (dolist (entry prosody-rule-alist)
    (prosody-validate-rule (car entry) (cdr entry) value))
  (when (boundp 'prosody-mode-rules)
    (dolist (rule prosody-mode-rules)
      (prosody-validate-rule 'prosody-mode-rules rule value)))
  (when (boundp 'prosody-presets)
    (prosody--validate-presets prosody-presets value))
  (set-default symbol value)
  (prosody--configuration-changed))

(defun prosody--set-presets (symbol value)
  "Set named font preset SYMBOL to VALUE after validation."
  (prosody--validate-presets value)
  (set-default symbol value)
  (prosody--configuration-changed))

(defun prosody--set-buffer-preset (symbol value)
  "Set the default buffer-local preset SYMBOL to VALUE."
  (unless (or (null value) (assq value prosody-presets))
    (error "Unknown font preset %S" value))
  (set-default symbol value)
  (prosody--configuration-changed))

(defun prosody--set-buffer-role-overrides (symbol value)
  "Set default buffer-local role override SYMBOL to VALUE."
  (prosody--validate-role-overrides symbol value)
  (set-default symbol value)
  (prosody--configuration-changed))

(defun prosody--set-mode-rules (symbol value)
  "Set SYMBOL to mode font rules VALUE after validating their roles."
  (dolist (rule value)
    (prosody-validate-rule symbol rule))
  (set-default symbol value))

(defun prosody--set-stacks (symbol value)
  "Set SYMBOL to font stacks VALUE and refresh configured fonts."
  (when (boundp 'prosody-roles)
    (dolist (entry prosody-roles)
      (when-let* ((stack (plist-get (cdr entry) :stack)))
        (unless (assq stack value)
          (error "Unknown font stack %S for role %S" stack (car entry)))))
    (when (boundp 'prosody-presets)
      (prosody--validate-presets prosody-presets prosody-roles value)))
  (set-default symbol value)
  (prosody--configuration-changed))

(defcustom prosody-stacks
  '((fallback
     :symbol ("Apple Symbols" "Symbola")
     :mathematical ("STIX Two Math"
                    "DejaVu Math TeX Gyre"
                    "Noto Sans Math")
     :emoji ("Apple Color Emoji"))
    (sans-serif
     :extends fallback
     :ascii ("Inter" "Avenir Next" "DejaVu Sans")
     :cjk ("LXGW Neo XiHei" "Source Han Sans SC" "PingFang SC" "Noto Sans CJK SC"
           "Hiragino Sans GB" "Microsoft YaHei"))
    (serif
     :extends fallback
     :ascii ("EB Garamond" "Athelas" "Iowan Old Style" "Baskerville"
             "Roboto Serif" "DejaVu Serif" "Georgia")
     :cjk ("Zhuque Fangsong (technical preview)" "LXGW Neo ZhiSong"
           "Source Han Serif SC VF" "Songti SC" "STFangsong"
           "LXGW WenKai" "Noto Serif CJK SC"))
    (slab-serif
     :extends serif
     :ascii ("Roboto Slab" "American Typewriter"))
    (cursive
     :extends serif
     :ascii ("Snell Roundhand" "Apple Chancery" "Zapfino")
     :cjk ("Xingkai SC" "Kaiti SC" "STKaiti"))
    (quasi-proportional
     :extends serif
     :ascii ("Iosevka Etoile" "Iosevka Aile")
     :cjk ("LXGW WenKai TC" "LXGW WenKai"
           "Source Han Serif SC VF" "Noto Serif CJK SC"))
    (monospace-narrow
     :extends fallback
     :ascii ("Iosevka" "Iosevka Term")
     :symbol ("Iosevka")
     :cjk ("LXGW WenKai Mono" "Sarasa Mono SC"))
    (monospace-align
     :extends monospace-narrow
     :ascii ("Maple Mono")
     :cjk ("Maple Mono CN"))
    (monospace-code
     :extends monospace-align
     :ascii ("Source Code Pro"))
    (monospace-sans-serif
     :extends monospace-narrow
     :ascii ("Roboto Mono" "DejaVu Sans Mono")))
  "Physical font candidates grouped into reusable stacks.
Each entry has the form (STACK :CATEGORY FONTS...).  Categories include
:ascii, :cjk, :symbol, :mathematical, and :emoji.  A stack can inherit
missing properties from another stack with :extends.  Font categories defined
by both stacks are combined with the child candidates first and duplicates
removed."
  :type 'sexp
  :group 'prosody
  :set #'prosody--set-stacks)

(defcustom prosody-roles
  `((default :stack monospace-narrow
             :height ,(if (eq system-type 'darwin) 160 140))
    (title :stack serif :weight heavy :height 2.0)
    (heading :stack serif :height 1.5)
    (body :stack monospace-sans-serif)
    (documentation :extends body)
    (prose :stack quasi-proportional)
    (decorative :stack cursive)
    (ui :stack sans-serif)
    (metadata-label :stack monospace-sans-serif)
    (metadata-value :stack monospace-narrow)
    (mono :stack monospace-sans-serif)
    (code :stack monospace-code)
    (table :stack monospace-align)
    (code-jvm :extends code :fonts ("JetBrains Mono"))
    (code-python :extends code :fonts ("Cascadia Code"))
    (code-diagram :extends code :fonts ("Fira Code"))
    (code-apple :extends code :fonts ("SF Mono"))
    (code-config :extends code :fonts ("IBM Plex Mono"))
    (terminal :extends mono :fonts ("Menlo")))
  "Typography preset organized by semantic role.
Each role has the form (ROLE :stack STACK &rest ATTRIBUTES).  STACK
names an entry in `prosody-stacks'.  The
`default' role uses an absolute face height in tenths of a point;
content roles use heights relative to it.  A role can use :extends and
:fonts to prepend concrete families to its inherited stack."
  :type 'sexp
  :group 'prosody
  :set #'prosody--set-preset)

(defcustom prosody-presets
  '((modern
     (title :stack sans-serif :fonts ("Inter Display") :weight bold)
     (heading :stack sans-serif :fonts ("Inter") :weight semi-bold)
     (body :stack sans-serif :fonts ("Inter"))
     (prose :stack sans-serif :fonts ("Inter"))
     (decorative :stack sans-serif :fonts ("Inter") :slant italic)
     (ui :stack sans-serif :fonts ("Inter"))
     (metadata-label :stack sans-serif :fonts ("Inter")
                     :weight semi-bold))
    (classical
     (title :stack serif :fonts ("EB Garamond"))
     (heading :stack serif :fonts ("Athelas"))
     (body :stack serif :fonts ("Iowan Old Style"))
     (prose :stack serif :fonts ("Iowan Old Style"))
     (decorative :stack cursive)
     (metadata-label :stack slab-serif))
    (technical
     (title :stack sans-serif :fonts ("DIN Condensed") :weight bold)
     (heading :stack sans-serif :fonts ("Avenir Next") :weight semi-bold)
     (body :stack serif :fonts ("STIX Two Text"))
     (prose :stack serif :fonts ("STIX Two Text"))
     (decorative :stack slab-serif :fonts ("Roboto Slab"))
     (ui :stack sans-serif :fonts ("Avenir Next"))
     (metadata-label :stack sans-serif :fonts ("Avenir Next")
                     :weight semi-bold)
     (mono :stack monospace-narrow :fonts ("SF Mono"))
     (code :stack monospace-code :fonts ("SF Mono"))))
  "Named role overrides applied on top of `prosody-roles'.
Each entry has the form (NAME (ROLE PROPERTY VALUE ...) ...).  Supported
properties are :stack, :fonts, :height, :weight, :slant, and :width."
  :type 'sexp
  :group 'prosody
  :set #'prosody--set-presets)

(defcustom prosody-buffer-preset nil
  "Named font preset selected for the current buffer.
Nil uses the base `prosody-roles'."
  :type '(choice (const :tag "Base preset" nil) symbol)
  :group 'prosody
  :set #'prosody--set-buffer-preset)
(make-variable-buffer-local 'prosody-buffer-preset)

(defcustom prosody-buffer-overrides nil
  "Role overrides applied only to the current buffer.
The value uses the same role override format as `prosody-presets'."
  :type 'sexp
  :group 'prosody
  :set #'prosody--set-buffer-role-overrides)
(make-variable-buffer-local 'prosody-buffer-overrides)

(defun prosody--safe-buffer-preset-p (value)
  "Return non-nil when VALUE names a configured font preset."
  (or (null value)
      (and (symbolp value) (assq value prosody-presets))))

(defun prosody--safe-buffer-role-overrides-p (value)
  "Return non-nil when VALUE is a valid local role override list."
  (condition-case nil
      (progn
        (prosody--validate-role-overrides 'file-local value)
        t)
    (error nil)))

(put 'prosody-buffer-preset 'safe-local-variable
     #'prosody--safe-buffer-preset-p)
(put 'prosody-buffer-overrides 'safe-local-variable
     #'prosody--safe-buffer-role-overrides-p)

(defcustom prosody-script-rules
  '(((han kana hangul bopomofo cjk-misc) cjk)
    (symbol symbol)
    (mathematical mathematical)
    (emoji emoji))
  "Map fontset character targets to stack categories.
Each rule has the form (CHARACTERS CATEGORY &optional ADD).  CHARACTERS
can be a script symbol or a list of script symbols.  CATEGORY names a
font category configured in `prosody-stacks'."
  :type 'sexp
  :group 'prosody)

(defcustom prosody-mode-rules
  `((:modes (nxml-mode sgml-mode toml-ts-mode conf-mode)
            :font code-config)
    (:modes (prog-mode)
            :font code)
    (:modes (text-mode)
            :font prose))
  "Fallback rules for buffer-local font selection.
Matching module rules are layered in declaration order, while the first one
providing :font or :rescale owns that setting.  The first matching rule here
is used only when no module rule matches.  Each rule is a plist.  Supported
keys are:

:modes       A mode or list of modes matched with `derived-mode-p'.
:buffer-name A regexp matched against `buffer-name'.
:font        Font role, concrete family, or ordered family list.
:faces       Face rules in the form (FACE FONTS-OR-ROLE &rest ATTRIBUTES).
             A FACE ending in * matches every face with that prefix, in
             version-aware name order.  :height-step adds a numeric delta and
             :weight-step moves through standard font weights for each match
             after the first.  Role attributes provide their starting values.
:rescale     Buffer-local `face-font-rescale-alist' value."
  :type 'sexp
  :group 'prosody
  :set #'prosody--set-mode-rules)

(dolist (entry prosody-rule-alist)
  (prosody-validate-rule (car entry) (cdr entry)))
(dolist (rule prosody-mode-rules)
  (prosody-validate-rule 'prosody-mode-rules rule))

(defvar prosody-setup-hook nil
  "Hook run after Prosody configures a frame.
Each function receives the configured frame.")

(defun prosody--merge-font-attributes (base overrides)
  "Return face attributes from BASE with OVERRIDES applied."
  (let ((attributes (copy-sequence base)))
    (while overrides
      (setq attributes
            (plist-put attributes (pop overrides) (pop overrides))))
    attributes))

(defvar-local prosody--effective-preset-cache nil
  "Cached effective font preset for the current buffer.")

(defun prosody--merge-role-overrides (preset overrides)
  "Return PRESET with role OVERRIDES merged into it."
  (let ((result (copy-tree preset)))
    (dolist (entry overrides)
      (let ((role (assq (car entry) result)))
        (setcdr role
                (prosody--merge-font-attributes (cdr role) (cdr entry)))))
    result))

(defun prosody--effective-preset ()
  "Return the effective role preset for the current buffer."
  (let ((key (list prosody--configuration-version
                   prosody-buffer-preset
                   prosody-buffer-overrides)))
    (if (equal key (car prosody--effective-preset-cache))
        (cadr prosody--effective-preset-cache)
      (let* ((named
              (when prosody-buffer-preset
                (or (cdr (assq prosody-buffer-preset prosody-presets))
                    (error "Unknown font preset %S"
                           prosody-buffer-preset))))
             (_ (prosody--validate-role-overrides
                 'buffer-local prosody-buffer-overrides))
             (preset (prosody--merge-role-overrides
                      prosody-roles named))
             (preset (prosody--merge-role-overrides
                      preset prosody-buffer-overrides)))
        (setq prosody--effective-preset-cache
              (list (copy-tree key) preset))
        preset))))

(defun prosody--role-spec-from-preset (role preset &optional seen)
  "Return ROLE's inherited specification from PRESET."
  (when (symbolp role)
    (when (memq role seen)
      (error "Circular font role inheritance involving %S" role))
    (when-let* ((spec (alist-get role preset)))
      (let ((parent (plist-get spec :extends)))
        (if parent
            (prosody--merge-font-attributes
             (or (prosody--role-spec-from-preset
                  parent preset (cons role seen))
                 (error "Unknown parent font role %S for %S" parent role))
             spec)
          spec)))))

(defun prosody--role-spec (role)
  "Return ROLE's effective font specification for the current buffer."
  (prosody--role-spec-from-preset role (prosody--effective-preset)))

(defun prosody--base-font-role-spec (role)
  "Return ROLE's specification from the base font preset."
  (prosody--role-spec-from-preset role prosody-roles))

(defun prosody--merge-font-stack-specs (parent child)
  "Merge PARENT and CHILD stack specs with child candidates first."
  (let ((result (copy-sequence parent)))
    (cl-loop for (property value) on child by #'cddr
             do (setq result
                      (plist-put
                       result property
                       (if (memq property
                                 '(:ascii :cjk :symbol :mathematical :emoji))
                           (delete-dups
                            (append value (plist-get result property) nil))
                         value))))
    result))

(defun prosody--stack-spec (stack)
  "Return the inherited specification for font STACK."
  (when (symbolp stack)
    (let* ((spec (alist-get stack prosody-stacks))
           (parent (plist-get spec :extends)))
      (unless spec
        (error "Unknown font stack: %S" stack))
      (if parent
          (prosody--merge-font-stack-specs (prosody--stack-spec parent) spec)
        spec))))

(defun prosody--role-candidates (role script)
  "Return ordered font candidates for ROLE and SCRIPT category."
  (let* ((role-spec (prosody--role-spec role))
         (stack (plist-get role-spec :stack))
         (stack-spec (prosody--stack-spec stack))
         (property (intern (format ":%s" script)))
         (fonts (copy-sequence (plist-get stack-spec property))))
    (when (eq script 'ascii)
      (setq fonts (append (plist-get role-spec :fonts) fonts)))
    (unless (and fonts (seq-every-p #'stringp fonts))
      (error "No %s fonts configured for role %S" script role))
    (delete-dups fonts)))

(defun prosody--list (fonts)
  "Return concrete FONTS or a font role as an ordered list."
  (cond
   ((prosody--role-spec fonts)
    (prosody--role-candidates fonts 'ascii))
   ((null fonts) nil)
   ((stringp fonts) (list fonts))
   ((and (listp fonts) (seq-every-p #'stringp fonts)) fonts)
   (t (error "Invalid font value: %S" fonts))))

(defun prosody--role-attributes (role)
  "Return face attributes associated with font ROLE."
  (cl-loop for (attribute value) on (prosody--role-spec role) by #'cddr
           unless (memq attribute '(:stack :fonts :extends))
           append (list attribute value)))

(defun prosody--data-hash (data)
  "Return a short stable hash for font configuration DATA."
  (substring (secure-hash 'sha1 (prin1-to-string data)) 0 12))

(defun prosody--role-face-name (role spec)
  "Return the face name for ROLE with effective SPEC."
  (intern
   (if (equal spec (prosody--base-font-role-spec role))
       (format "prosody-role-%s" role)
     (format "prosody-role-%s-%s" role (prosody--data-hash spec)))))

(defvar prosody--role-face-signatures (make-hash-table :test 'equal)
  "Last configured signature for each Prosody role face and frame.")

(defun prosody--role-face (role &optional frame)
  "Return ROLE's face for the current buffer, creating it on FRAME."
  (when-let* ((spec (prosody--role-spec role)))
    (let* ((face (prosody--role-face-name role spec))
           (frame (or frame (selected-frame))))
      (unless (facep face)
        (make-empty-face face))
      (let* ((fontset (and (fboundp 'prosody--fontset-for-role)
                           (prosody--fontset-for-role role frame)))
             (signature (list spec fontset))
             (key (cons face frame)))
        (unless (equal signature
                       (gethash key prosody--role-face-signatures))
          (dolist (attribute (mapcar #'car face-attribute-name-alist))
            (set-face-attribute face frame attribute 'unspecified))
          ;; `:font' resolves Latin; `:fontset' restores script mappings.
          (apply #'set-face-attribute face frame
                 (append (when fontset
                           (list :font fontset :fontset fontset))
                         (prosody--role-attributes role)))
          (puthash key signature prosody--role-face-signatures)))
      face)))

(defvar prosody--fontset-signatures (make-hash-table :test 'equal)
  "Last configured signature for each Prosody fontset name.")

(defvar prosody--default-fontset-signature nil
  "Last Prosody configuration applied to the default fontset.")

(defcustom prosody-fontset-rule-functions nil
  "Functions that return additional fontset rules for a semantic role.
Each function receives one role and returns rules in the resolved form
(CHARACTERS FONTS &optional ADD)."
  :type 'hook
  :group 'prosody)

(defun prosody-add-fontset-rule-function (function)
  "Register FUNCTION as a fontset rule provider and invalidate fontsets."
  (add-hook 'prosody-fontset-rule-functions function)
  (prosody--configuration-changed))

(defun prosody--fontset-rules (role)
  "Return resolved script and extension fontset rules for ROLE."
  (append
   (mapcar (lambda (rule)
             (list (car rule)
                   (prosody--role-candidates role (cadr rule))
                   (caddr rule)))
           prosody-script-rules)
   (apply #'append
          (mapcar (lambda (function) (funcall function role))
                  prosody-fontset-rule-functions))))

(defun prosody--fontset-name (role signature)
  "Return the fontset name for ROLE and resolved SIGNATURE."
  (format "-*-prosody-*-*-*-*-*-*-*-*-*-*-fontset-prosody_%s_%s"
          (replace-regexp-in-string "-" "_" (symbol-name role))
          (prosody--data-hash signature)))

(defun prosody--fontset-signature (role &optional frame)
  "Return ROLE's available fontset configuration on FRAME."
  (let* ((graphical (display-graphic-p frame))
         (families (and graphical (font-family-list frame)))
         (available
          (lambda (fonts)
            (if graphical
                (seq-filter (lambda (font)
                              (member font families))
                            fonts)
              fonts))))
    (list
     (funcall available (prosody--role-candidates role 'ascii))
     (mapcar (lambda (rule)
               (list (nth 0 rule)
                     (funcall available (nth 1 rule))
                     (nth 2 rule)))
             (prosody--fontset-rules role)))))

(defun prosody--set-fontset-candidates (fontset characters fonts &optional add)
  "Set ordered FONTS for CHARACTERS in FONTSET."
  (let ((specs (mapcar (lambda (family)
                         (font-spec :family family
                                    :registry "iso10646-1"))
                       fonts)))
    (cond
     ((null specs)
      (set-fontset-font fontset characters nil))
     (add
      (dolist (spec specs)
        (set-fontset-font fontset characters spec nil add)))
     (t
      ;; Replace with the least preferred candidate, then prepend the rest.
      ;; This preserves the configured order ahead of inherited fallbacks.
      (setq specs (nreverse specs))
      (set-fontset-font fontset characters (pop specs))
      (dolist (spec specs)
        (set-fontset-font fontset characters spec nil 'prepend))))))

(defun prosody--configure-role-fontset (fontset signature)
  "Configure FONTSET from a role SIGNATURE."
  (prosody--set-fontset-candidates fontset 'ascii (nth 0 signature))
  (pcase-dolist (`(,characters ,fonts ,add) (nth 1 signature))
    (dolist (character (ensure-list characters))
      (prosody--set-fontset-candidates fontset character fonts add))))

(defun prosody--fontset-for-role (role &optional frame)
  "Return the configured fontset for ROLE, or nil for a non-role."
  (when (prosody--role-spec role)
    (let* ((signature (prosody--fontset-signature role frame))
           (fontset (prosody--fontset-name role signature)))
      (cond
       ((display-graphic-p frame)
        (let ((created (not (query-fontset fontset))))
          (when created
            (create-fontset-from-fontset-spec fontset))
          (when (or created
                    (not (equal signature
                                (gethash fontset prosody--fontset-signatures))))
            (prosody--configure-role-fontset fontset signature)
            (puthash fontset signature prosody--fontset-signatures)))
        fontset)
       ;; `query-fontset' signals before any graphical backend is initialized.
       ((condition-case nil
            (query-fontset fontset)
          (error nil))
        fontset)))))

(defun prosody--first-existing-font (fonts &optional frame)
  "Return the first font from FONTS available on FRAME."
  (when (display-graphic-p frame)
    (let* ((candidates (prosody--list fonts))
           (families (font-family-list frame))
           (font (seq-find (lambda (candidate)
                             (member candidate families))
                           candidates)))
      (unless font
        (warn "No candidate font found: %s"
              (string-join candidates ", ")))
      font)))

(defun prosody-font-family (fonts &optional frame)
  "Return the first installed family from FONTS on FRAME.
FONTS may be a semantic role, a family name, or an ordered list of family
names.  Return nil when FRAME is not graphical or no candidate is installed."
  (prosody--first-existing-font fonts frame))

(defun prosody--face-family (fonts &optional frame)
  "Return the first installed family for FONTS on FRAME."
  (prosody-font-family fonts frame))

(defun prosody--resolved-face-spec (fonts &optional overrides)
  "Return a face spec for FONTS with role attributes and OVERRIDES."
  (if-let* ((face (prosody--role-face fonts)))
      (if overrides
          (prosody--merge-font-attributes (list :inherit face) overrides)
        face)
    (let ((attributes (copy-sequence overrides)))
      (when-let* ((family (prosody--face-family fonts)))
        (setq attributes (plist-put attributes :family family)))
      attributes)))

(defun prosody--set-available-fonts (fontset characters font-list &optional frame add)
  "Safely set fontset fonts.
If ADD is non-nil, all fonts in FONT-LIST are set with given ADD parameter.
If ADD is nil, use the existing fonts as an ordered replacement."
  (when (display-graphic-p frame)
    (let ((fonts (prosody--list font-list))
          (families (font-family-list frame))
          available)
      (dolist (font fonts)
        (if (member font families)
            (push font available)
          (warn "Font %s not found" font)))
      (setq available (nreverse available))
      (when available
        (prosody--set-fontset-candidates fontset characters available add)
        (dolist (font available)
          (message "Set %s fontset font to %s" characters font))))))


(defun prosody-set-face-font (face fonts &optional frame)
  "Safely set FACE family from FONTS or a font role."
  (let ((prosody-buffer-preset nil)
        (prosody-buffer-overrides nil))
    (if-let* ((role-face (prosody--role-face fonts frame)))
        (progn
          (set-face-attribute face frame
                              :inherit (list role-face 'fixed-pitch))
          (message "Set %s face font role to %s" face fonts)
          role-face)
      (when-let* ((family (prosody--face-family fonts frame)))
        (set-face-attribute face frame :family family :inherit 'fixed-pitch)
        (message "Set %s face font to %s" face family)
        family))))

(defun prosody--set-buffer-font (fonts)
  "Set the current graphical buffer face from FONTS or a font role."
  (when (display-graphic-p)
    (when-let* ((spec (prosody--resolved-face-spec fonts)))
      (buffer-face-set spec)
      (message "Set buffer %s face to %s" (current-buffer) fonts)
      spec)))

(defun prosody--configure-default-fontset (signature frame)
  "Apply default fontset SIGNATURE once using graphical FRAME."
  (unless (equal signature prosody--default-fontset-signature)
    (pcase-dolist (`(,scripts ,fonts . ,args) (nth 1 signature))
      (dolist (script (ensure-list scripts))
        (prosody--set-available-fonts t script fonts frame (car args))))
    (setq prosody--default-fontset-signature signature)))

(defun prosody-setup (&optional frame)
  "Configure Prosody fonts on graphical FRAME."
  (when (display-graphic-p frame)
    (let ((prosody-buffer-preset nil)
          (prosody-buffer-overrides nil))
      ;; Configure the base role fontsets before scripts are first displayed.
      (dolist (role prosody-roles)
        (prosody--role-face (car role) frame))
      (when-let* ((fontset (prosody--fontset-for-role 'default frame)))
        (apply #'set-face-attribute 'default frame
               :font fontset :fontset fontset
               (prosody--role-attributes 'default)))
      (prosody--configure-default-fontset
       (prosody--fontset-signature 'default frame) frame)
      (run-hook-with-args 'prosody-setup-hook frame))))

(add-hook 'after-init-hook #'prosody-setup)
(add-hook 'after-make-frame-functions #'prosody-setup)

(defun prosody--rule-matches-mode-p (rule mode)
  "Return non-nil when font RULE applies to major MODE."
  (when-let* ((modes (plist-get rule :modes)))
    (or (memq mode (ensure-list modes))
        (provided-mode-derived-p mode modes))))

(defun prosody--mode-font-rule-matches-p (rule)
  "Return non-nil when RULE applies to the current buffer."
  (or (prosody--rule-matches-mode-p rule major-mode)
      (when-let* ((regexp (plist-get rule :buffer-name)))
        (string-match-p regexp (buffer-name)))))

(defun prosody-role-for-mode (mode)
  "Return the font selected for major MODE by Prosody's font rules."
  (let ((rules
         (seq-filter
          (lambda (rule) (prosody--rule-matches-mode-p rule mode))
          (mapcar #'cdr prosody-rule-alist))))
    (unless rules
      (when-let* ((fallback
                   (seq-find (lambda (rule)
                               (prosody--rule-matches-mode-p rule mode))
                             prosody-mode-rules)))
        (setq rules (list fallback))))
    (when-let* ((rule (seq-find (lambda (candidate)
                                  (plist-get candidate :font))
                                rules)))
      (plist-get rule :font))))

(defun prosody-apply-to-region (mode start end)
  "Prepend the font selected for MODE to the region from START to END."
  (when-let* ((font (prosody-role-for-mode mode))
              (spec (prosody--resolved-face-spec font)))
    (with-silent-modifications
      (add-face-text-property start end spec))
    font))

(defvar-local prosody--mode-font-state nil
  "Mode font state last applied to the current buffer.")

(defvar-local prosody--mode-face-remap-cookies nil
  "Face remapping cookies owned by Prosody in the current buffer.")

(defvar-local prosody--mode-buffer-face nil
  "Buffer face specification owned by Prosody in the current buffer.")

(defvar-local prosody--mode-font-rescale-state nil
  "Previous buffer-local rescale state saved by Prosody.")

(defun prosody--clear-mode-font ()
  "Remove mode font settings owned by Prosody from the current buffer."
  (mapc #'face-remap-remove-relative prosody--mode-face-remap-cookies)
  (setq prosody--mode-face-remap-cookies nil)
  (when prosody--mode-buffer-face
    (when (equal buffer-face-mode-face prosody--mode-buffer-face)
      (buffer-face-set))
    (setq prosody--mode-buffer-face nil))
  (when prosody--mode-font-rescale-state
    (pcase-let ((`(,local-p ,value) prosody--mode-font-rescale-state))
      (if local-p
          (setq-local face-font-rescale-alist value)
        (kill-local-variable 'face-font-rescale-alist)))
    (setq prosody--mode-font-rescale-state nil)))

(defun prosody--step-weight (weight step index)
  "Move WEIGHT by STEP for wildcard match INDEX."
  (let* ((entry (seq-find (lambda (candidate)
                            (seq-contains-p candidate weight))
                          font-weight-table))
         (position (and entry (seq-position font-weight-table entry))))
    (unless position
      (error "Cannot step unknown font weight %S" weight))
    (aref (aref font-weight-table
                (max 0
                     (min (1- (length font-weight-table))
                          (floor (+ position (* step index) 0.5)))))
          1)))

(defun prosody--stepped-attributes (fonts attributes index)
  "Resolve stepped face ATTRIBUTES for FONTS at wildcard match INDEX."
  (let* ((role-spec (prosody--role-spec fonts))
         (height-step (plist-get attributes :height-step))
         (weight-step (plist-get attributes :weight-step))
         (attributes
          (cl-loop for (property value) on attributes by #'cddr
                   unless (memq property '(:height-step :weight-step))
                   append (list property value))))
    (when height-step
      (let* ((role-height (plist-get role-spec :height))
             (base (or (plist-get attributes :height)
                       role-height
                       1.0))
             (height (+ base (* height-step index))))
        (unless (and (numberp base)
                     (> height 0)
                     (or (floatp base) (integerp height)))
          (error "Invalid stepped face height %S from base %S" height base))
        ;; Relative heights inherited from the role already participate in
        ;; face remapping, so apply only the ratio needed to reach HEIGHT.
        (setq attributes
              (plist-put attributes :height
                         (if (and (floatp role-height) (floatp height))
                             (/ height role-height)
                           height)))))
    (when weight-step
      (let ((base (or (plist-get attributes :weight)
                      (plist-get role-spec :weight)
                      'normal)))
        (setq attributes
              (plist-put attributes :weight
                         (prosody--step-weight base weight-step index)))))
    attributes))

(defun prosody--rule-faces (face)
  "Return faces matched by FACE or its trailing wildcard."
  (let ((name (symbol-name face)))
    (if (and (> (length name) 0)
             (eq (aref name (1- (length name))) ?*))
        (let ((prefix (substring name 0 -1)))
          (sort (seq-filter
                 (lambda (candidate)
                   (string-prefix-p prefix (symbol-name candidate)))
                 (face-list))
                (lambda (left right)
                  (string-version-lessp (symbol-name left)
                                        (symbol-name right)))))
      (and (facep face) (list face)))))

(defun prosody--matching-mode-font-rules ()
  "Return the module font rules for the current buffer.
Use the first matching fallback rule only when no module rule matches."
  (let ((rules (seq-filter #'prosody--mode-font-rule-matches-p
                           (mapcar #'cdr prosody-rule-alist))))
    (or rules
        (when-let* ((fallback
                     (seq-find #'prosody--mode-font-rule-matches-p
                               prosody-mode-rules)))
          (list fallback)))))

(defun prosody--apply-mode-font-rules (rules)
  "Apply matching mode font RULES to the current buffer."
  (prosody--clear-mode-font)
  (when-let* ((rule (seq-find (lambda (candidate)
                                (plist-get candidate :font))
                              rules))
              (font (plist-get rule :font))
              (spec (prosody--set-buffer-font font)))
    (setq prosody--mode-buffer-face spec))
  (dolist (rule rules)
    (pcase-dolist (`(,face ,fonts . ,attributes) (plist-get rule :faces))
      (cl-loop for matched-face in (prosody--rule-faces face)
               for index from 0
               for stepped = (prosody--stepped-attributes
                              fonts attributes index)
               for spec = (prosody--resolved-face-spec fonts stepped)
               when spec
               do (push (face-remap-add-relative matched-face spec)
                        prosody--mode-face-remap-cookies))))
  (when-let* ((rule (seq-find (lambda (candidate)
                                (plist-get candidate :rescale))
                              rules))
              (rescale (plist-get rule :rescale)))
    (setq prosody--mode-font-rescale-state
          (list (local-variable-p 'face-font-rescale-alist)
                face-font-rescale-alist))
    (setq-local face-font-rescale-alist rescale)))

(defun prosody--setup-mode-font (&optional force)
  "Set fonts according to the current major mode.
With FORCE, reapply the configured fonts.  Respect a `buffer-face-mode'
owned by other configuration."
  (unless (and (bound-and-true-p buffer-face-mode)
               (not (equal buffer-face-mode-face prosody--mode-buffer-face)))
    (let* ((rules (prosody--matching-mode-font-rules))
           (state (list major-mode rules
                        prosody--configuration-version
                        prosody-buffer-preset
                        (copy-tree prosody-buffer-overrides))))
      (when (or force (not (equal state prosody--mode-font-state)))
        (if rules
            (prosody--apply-mode-font-rules rules)
          (prosody--clear-mode-font))
        (setq prosody--mode-font-state state)))))

(defun prosody--setup-window-fonts (window-or-frame)
  "Apply mode fonts to buffers visible in WINDOW-OR-FRAME."
  (dolist (window (if (windowp window-or-frame)
                      (list window-or-frame)
                    (window-list window-or-frame 'no-minibuf)))
    (with-current-buffer (window-buffer window)
      (prosody--setup-mode-font))))

(defun prosody--setup-visible-mode-font (&rest _)
  "Apply mode fonts when the current buffer is visible."
  (when (get-buffer-window (current-buffer) 'visible)
    (prosody--setup-mode-font)))

(defun prosody--refresh-mode-fonts (&rest _)
  "Reapply Prosody's mode font rules to visible buffers."
  (let (buffers)
    (walk-windows (lambda (window)
                    (cl-pushnew (window-buffer window) buffers))
                  'no-minibuf 'visible)
    (dolist (buffer buffers)
      (with-current-buffer buffer
        (prosody--setup-mode-font t)))))

(defun prosody-refresh-buffer ()
  "Refresh font settings and native fontification in the current buffer."
  (interactive)
  (setq prosody--effective-preset-cache nil
        prosody--mode-font-state nil)
  (when (get-buffer-window (current-buffer) 'visible)
    (prosody--setup-mode-font t))
  (when (bound-and-true-p font-lock-mode)
    (font-lock-flush)
    (when (get-buffer-window (current-buffer) 'visible)
      (font-lock-ensure))))

(defun prosody-select-preset (selection)
  "Select a named font preset for the current buffer.
SELECTION may also restore the default value or select the base preset."
  (interactive
   (let* ((choices
           (append '("<default>" "<base>")
                   (mapcar (lambda (entry) (symbol-name (car entry)))
                           prosody-presets)))
          (current (if (local-variable-p 'prosody-buffer-preset)
                       (if prosody-buffer-preset
                           (symbol-name prosody-buffer-preset)
                         "<base>")
                     "<default>")))
     (list (completing-read "Buffer font preset: " choices nil t
                            nil nil current))))
  (pcase selection
    ("<default>" (kill-local-variable 'prosody-buffer-preset))
    ("<base>" (setq-local prosody-buffer-preset nil))
    ((pred symbolp)
     (unless (assq selection prosody-presets)
       (user-error "Unknown font preset %S" selection))
     (setq-local prosody-buffer-preset selection))
    ((pred stringp)
     (let ((preset (intern selection)))
       (unless (assq preset prosody-presets)
         (user-error "Unknown font preset %S" preset))
       (setq-local prosody-buffer-preset preset))))
  (prosody-refresh-buffer)
  (message "Buffer font preset: %s"
           (or prosody-buffer-preset "base")))

(defun prosody--refresh-after-local-variables ()
  "Refresh explicit buffer-local font configuration after loading it."
  (when (or (local-variable-p 'prosody-buffer-preset)
            (local-variable-p 'prosody-buffer-overrides))
    (prosody-refresh-buffer)))

(add-hook 'prosody-setup-hook #'prosody--refresh-mode-fonts)
(add-hook 'window-buffer-change-functions #'prosody--setup-window-fonts)
(add-hook 'after-change-major-mode-hook #'prosody--setup-visible-mode-font)
(add-hook 'after-revert-hook #'prosody--setup-visible-mode-font)
(add-hook 'hack-local-variables-hook
          #'prosody--refresh-after-local-variables)

(when (and after-init-time (display-graphic-p))
  (prosody-setup))

(provide 'prosody)

;;; prosody.el ends here
