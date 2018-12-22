;; melpa (from melpa.org get started)
(require 'package)
(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                    (not (gnutls-available-p))))
       (proto (if no-ssl "http" "https")))
  ;; Comment/uncomment these two lines to enable/disable MELPA and MELPA Stable as desired
  (add-to-list 'package-archives (cons "melpa" (concat proto "://melpa.org/packages/")) t)
  ;;(add-to-list 'package-archives (cons "melpa-stable" (concat proto "://stable.melpa.org/packages/")) t)
  (when (< emacs-major-version 24)
    ;; For important compatibility libraries like cl-lib
    (add-to-list 'package-archives '("gnu" . (concat proto "://elpa.gnu.org/packages/")))))
(package-initialize)

;; enable this and disable next snippet to update org
;; (require 'package)
;; (add-to-list 'package-archives '("org" . "https://orgmode.org/elpa/") t)

;; load my configuration
(require 'org)
(org-babel-load-file
 (expand-file-name "config.org"
                user-emacs-directory))

;; load custom.el for themes
(load "~/.emacs.d/custom.el")
;; (require 'custom)

;; ----------------------------------------------------------------
;; Idealy there should be nothing below this line
;; as long as there is no `customize.el' used in the configuration
;; ----------------------------------------------------------------
