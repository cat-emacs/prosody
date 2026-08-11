;;; prosody-test.el --- Tests for Prosody -*- lexical-binding: t; -*-

(require 'ert)
(require 'prosody)
(require 'prosody-use-package)

(ert-deftest prosody-register-preserves-order-and-replaces-owner ()
  (let ((prosody-rule-alist nil))
    (prosody-register 'alpha '(:modes alpha-mode :font code))
    (prosody-register 'beta '(:modes beta-mode :font prose))
    (prosody-register 'alpha '(:modes alpha-mode :font mono))
    (should (equal prosody-rule-alist
                   '((alpha :modes alpha-mode :font mono)
                     (beta :modes beta-mode :font prose))))))

(ert-deftest prosody-stack-inheritance-prepends-and-deduplicates ()
  (let ((prosody-stacks
         '((base :ascii ("Base" "Shared") :cjk ("CJK"))
           (child :extends base :ascii ("Child" "Shared")))))
    (should (equal (prosody--stack-spec 'child)
                   '(:ascii ("Child" "Shared" "Base")
                     :cjk ("CJK")
                     :extends base)))))

(ert-deftest prosody-buffer-preset-and-overrides-compose ()
  (let ((prosody-roles
         '((default :stack mono :height 140)
           (prose :stack serif :height 1.1)))
        (prosody-presets
         '((large (prose :height 1.3 :weight bold)))))
    (with-temp-buffer
      (setq-local prosody-buffer-preset 'large)
      (setq-local prosody-buffer-overrides
                  '((prose :fonts ("Charter") :slant italic)))
      (should
       (equal (prosody--role-spec 'prose)
              '(:stack serif :height 1.3 :weight bold
                :fonts ("Charter") :slant italic))))))

(ert-deftest prosody-role-for-mode-prefers-registered-rules ()
  (let ((prosody-rule-alist
         '((example :modes text-mode :font documentation)))
        (prosody-mode-rules
         '((:modes text-mode :font prose))))
    (should (eq (prosody-role-for-mode 'text-mode) 'documentation))))

(ert-deftest prosody-font-family-resolves-role-candidates ()
  (let ((prosody-stacks
         '((serif :ascii ("Missing" "Installed"))))
        (prosody-roles
         '((prose :stack serif))))
    (cl-letf (((symbol-function 'display-graphic-p)
               (lambda (&optional _frame) t))
              ((symbol-function 'font-family-list)
               (lambda (&optional _frame) '("Installed"))))
      (should (equal (prosody-font-family 'prose) "Installed")))))

(ert-deftest prosody-use-package-normalizes-short-rule ()
  (should
   (equal (use-package-normalize/:font-rule
           'example nil '(code))
          '(:font code :modes example-mode))))

(ert-deftest prosody-use-package-normalizes-face-only-rule ()
  (should
   (equal (use-package-normalize/:font-rule
           'example nil
           '((:modes text-mode :faces ((example-face prose)))))
          '(:modes text-mode :faces ((example-face prose))))))

(ert-deftest prosody-use-package-normalizes-buffer-name-rule ()
  (should
   (equal (use-package-normalize/:font-rule
           'example nil
           '((code :buffer-name "Example")))
          '(:font code :buffer-name "Example"))))

(provide 'prosody-test)

;;; prosody-test.el ends here
