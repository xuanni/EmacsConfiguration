(cond ((eq system-type 'windows-nt)
       (set-face-attribute 'default nil :font "Consolas" :height 143))
      ((eq system-type 'darwin)
       ;; (set-face-attribute 'default nil :font "Menlo" :height 143))
       (set-face-attribute 'default nil :family "Source Code Pro" :foundry "ADBO" :height 160)) ; t for default case
      ;; (t (set-face-attribute 'default nil :font "Courier 10 Pitch" :height 136)) ; t for default case
      ;; looks like gnu/linux is not working in system-type
      (t (set-face-attribute 'default nil :family "Source Code Pro" :foundry "ADBO" :height 136)) ; t for default case
      )

(prefer-coding-system 'utf-8-unix)
(blink-cursor-mode -1)
(column-number-mode 1)                  ; display column number in modeline
;; (display-time-mode 1)                ; disable this to save some modeline space for 2 column views
(cond ((eq system-type 'darwin))
      (t (menu-bar-mode -1)))
(scroll-bar-mode -1)
(show-paren-mode 1)                     ; highlight matching parenthese
(tool-bar-mode -1)
(which-function-mode 1)                 ; display function name in modeline
(fset 'yes-or-no-p 'y-or-n-p)           ; simplify this
(setq-default indent-tabs-mode nil)     ; use space instead of tabs (setq: buffer local var, setq-default global)
(setq initial-scratch-message ";; Put your mess here\n\n")
(setq inhibit-startup-message nil)
(setq inhibit-startup-screen t)
(setq large-file-warning-threshold 100000000)
(setq scroll-step 1)                    ; don't suddenly roll up a lot of lines
(setq tab-width 4)
(setq visible-bell t)                   ; flash screen instead of audible ding
(if (eq system-type 'darwin)
    (progn
      (setq ns-command-modifier (quote control))
      (setq ns-right-command-modifier (quote meta))))
(put 'narrow-to-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(global-auto-revert-mode 1)             ;could be more traffic/network

(add-hook 'org-mode-hook 'org-indent-mode)

(setq org-startup-truncated nil)        ; tell org mode to wrap line instead of shift
(setq org-use-speed-commands t)         ; use single key to navigate

(global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c c") 'org-capture)
(global-set-key (kbd "C-c b") 'org-switchb)

;; refile across files, otherwise can only refile within the same file
(setq org-refile-targets '((org-agenda-files :maxlevel . 3)))
;; refile as the top heading, otherwise can only refile as the child of existing headings
(setq org-refile-use-outline-path 'file)
;; refile list all possibilities at once, not the default step by step, for helm
(setq org-outline-path-complete-in-steps nil)
;; refile can create new parent, must add "/New Heading" at the end
(setq org-refile-allow-creating-parent-nodes 'confirm)

(cond
 ((string-equal system-type "windows-nt")
  (setq org-agenda-path "u:/Documents/org/")) ; this is Windows map of /usr2/xni
 (t
  (setq org-agenda-path "~/Documents/org/")))

(setq org-todo-file (concat org-agenda-path "todo.org"))
(setq org-notes-file (concat org-agenda-path "notes.org"))
(setq org-log-file (concat org-agenda-path "log.org"))
(setq org-finance-file (concat org-agenda-path "finance.org"))
(setq org-travel-file (concat org-agenda-path "travel.org"))

;; (setq org-agenda-files (list
;;                         org-todo-file
;;                         org-notes-file
;;                         org-log-file
;;                         ))

(setq org-agenda-files (directory-files-recursively org-agenda-path "\.org$"))

(setq org-capture-templates
      (quote (
              ("t" "TODO" entry (file org-todo-file)
               "* TODO %^{Title} %^G\n:PROPERTIES:\n:INCEPTION: %T\n:END:\n%?" :prepend t)

              ("l" "Work Log" entry (file org-log-file)
               "* Week %t \t:worklog:\n%?")

              ("n" "Notes" entry (file org-notes-file)
              "* %^{Title} %^G\n:PROPERTIES:\n:INCEPTION: %U\n:END:\n%?")
              )))

(setq org-agenda-include-diary t)

(setq org-log-done 'time)

;; (setq org-todo-keywords '(
;;                           (sequence "TODO" "|" "DONE" "POSTPONE" "ABANDONED") ; for normal stuff
;;                           (sequence "CR" "|" "FIXED" "NOT-A-BUG") ; for bug
;;                           ))
(setq org-todo-keywords
      '(;; Sequence for TASKS
        ;; TODO means it's an item that needs addressing
        ;; WAITING means it's dependent on something else happening
        ;; DELEGATED means someone else is doing it and I need to follow up with them
        ;; ASSIGNED means someone else has full, autonomous responsibility for it
        ;; CANCELLED means it's no longer necessary to finish
        ;; DONE means it's complete
        (sequence "TODO(t@/!)" "WAITING(w@/!)" "DELEGATED(e@/!)" "|" "ASSIGNED(a@/!)" "CANCELLED(c@/!)" "DONE(d@/!)")
        ))
;; put - STATE change from xx to xx into drawer so when export,
;; these state changes won't be exported
(setq org-log-into-drawer t)

(setq org-babel-load-languages
      (quote
       ((emacs-lisp . t)
        (shell . t)
        (python . t)
        (awk . t)
        (C . t)
        (java . t)
        (latex . t)
        (makefile . t)
        (octave . t))))

(setq org-export-backends (quote (ascii html latex man md odt groff)))

(require 'org)
(define-key org-mode-map (kbd "C-c %") 'org-mark-ring-goto)

;; Remove all backup files
;; (setq make-backup-files nil)
;; (setq backup-inhibited t)
;; (setq auto-save-default nil)
;; or alternatively
(setq backup-directory-alist '(("." . "~/.saves")))

(setq-default c-default-style "linux")
(setq-default c-basic-offset 4)
(c-set-offset 'case-label '+)           ; fix case index

(setq python-indent-offset 4)

(add-to-list 'auto-mode-alist '("\\.scons\\'" . python-mode))
(add-to-list 'auto-mode-alist '("\\.api\\'" . python-mode))

(add-to-list 'auto-mode-alist '("\\.mak\\'" . makefile-mode))

(require 'dired-x)

(setq dired-dwim-target t)

(setq nxml-slash-auto-complete-flag t)

(add-hook 'before-save-hook 'whitespace-cleanup)
(add-hook 'before-save-hook (lambda() (delete-trailing-whitespace)))

(global-set-key (kbd "C-x C-b") 'buffer-menu) ; not use default list-buffers because it will not focus on the buffer list

(add-hook 'org-mode-hook 'abbrev-mode)
(add-hook 'shell-mode 'abbrev-mode)

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))

(global-set-key (kbd "C-x t") 'eshell)
(global-set-key (kbd "C-x y") 'shell)

(use-package all-the-icons)
(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1))

(require 'helm-config)

;; The default "C-x c" is quite close to "C-x C-c", which quits Emacs.
;; Changed to "C-c h". Note: We must set "C-c h" globally, because we
;; cannot change `helm-command-prefix-key' once `helm-config' is loaded.
(global-set-key (kbd "C-c h") 'helm-command-prefix)
(global-unset-key (kbd "C-x c"))

(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "M-y") 'helm-show-kill-ring)
(global-set-key (kbd "C-x b") 'helm-mini)
(global-set-key (kbd "C-x C-f") 'helm-find-files)

;; for search, still use c-s
;; for read only helm-occur (C-c h M-s o), I bind it to replace occur (M-s o)
(global-set-key (kbd "M-s o") 'helm-occur)
;; for multi editing search, use helm-swoop (M-i) see below section
;; no more, helm-swoop is slow for large files, helm-occur is much faster
;; swiper is in between in terms of performance
;; this is to enable edit mode automatically (could bind it to C-x C-q to manually
;; use it) see https://github.com/emacs-helm/helm/issues/2146
(add-hook 'helm-occur-mode-hook 'wgrep-change-to-wgrep-mode t)

;; make helm open at bottom with full frame width
(add-to-list 'display-buffer-alist
             `(,(rx bos "*helm" (* not-newline) "*" eos)
               (display-buffer-in-side-window)
               (inhibit-same-window . t)
               (window-height . 0.4)))
;; (define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action) ; rebind tab to run persistent action
;; (define-key helm-map (kbd "C-i") 'helm-execute-persistent-action) ; make TAB works in terminal
;; (define-key helm-map (kbd "C-z") 'helm-select-action) ; list actions using C-z

;; (when (executable-find "curl")
;;   (setq helm-google-suggest-use-curl-p t)) ;not sure what this is for

(setq
 ;; helm-split-window-in-side-p           t ; open helm buffer inside current window, not occupy whole other window
      ;; helm-move-to-line-cycle-in-source     t ; move to end or beginning of source when reaching top or bottom of source.
      helm-ff-search-library-in-sexp        t ; search for library in `require' and `declare-function' sexp.
      helm-scroll-amount                    8 ; scroll 8 lines other window using M-<next>/M-<prior>
      ;; helm-ff-file-name-history-use-recentf t
      )

(helm-mode 1)                           ; turn on helm mode for kill buffer etc

(require 'helm-gtags)
;; (add-hook 'dired-mode-hook 'helm-gtags-mode)
;; (add-hook 'eshell-mode-hook 'helm-gtags-mode)
(add-hook 'c-mode-hook 'helm-gtags-mode)
(add-hook 'c++-mode-hook 'helm-gtags-mode)
(add-hook 'java-mode-hook 'helm-gtags-mode)
(add-hook 'asm-mode-hook 'helm-gtags-mode)
;; for python, use elpy-goto-definition (same key)
;; (add-hook 'python-mode-hook 'helm-gtags-mode)

(setq helm-gtags-auto-update t)
(setq helm-gtags-suggested-key-mapping t)

;; key bindings
(eval-after-load "helm-gtags"
  '(progn
     (define-key helm-gtags-mode-map (kbd "M-.") 'helm-gtags-dwim)
     ;; (define-key helm-gtags-mode-map (kbd "M-t") 'helm-gtags-find-tag)
     ;; (define-key helm-gtags-mode-map (kbd "M-r") 'helm-gtags-find-rtag)
     ;; (define-key helm-gtags-mode-map (kbd "M-s") 'helm-gtags-find-symbol)
     ;; (define-key helm-gtags-mode-map (kbd "M-g M-p") 'helm-gtags-parse-file)
     (define-key helm-gtags-mode-map (kbd "C-c <") 'helm-gtags-previous-history)
     (define-key helm-gtags-mode-map (kbd "C-c >") 'helm-gtags-next-history)
     (define-key helm-gtags-mode-map (kbd "M-,") 'helm-gtags-pop-stack)))

;; key bindings (old)
;; (define-key helm-gtags-mode-map (kbd "M-.") 'helm-gtags-dwim)
;; (define-key helm-gtags-mode-map (kbd "M-,") 'helm-gtags-pop-stack)
;; (define-key helm-gtags-mode-map (kbd "C-c <") 'helm-gtags-previous-history)
;; (define-key helm-gtags-mode-map (kbd "C-c >") 'helm-gtags-next-history)
;; (define-key helm-gtags-mode-map (kbd "C-x k") 'kill-buffer)
(provide 'setup-helm-gtags)

(helm-projectile-on)
(projectile-mode t)
(setq projectile-enable-caching t)
(setq projectile-globally-ignored-directories
 (quote
  (".idea" ".eunit" ".git" ".hg" ".fslckout" ".bzr" "_darcs" ".tox" ".svn" ".stack-work" "Debug*")))
(setq projectile-globally-ignored-file-suffixes
 (quote
  ("o" "so" "lib" "a" "pyc" "elf" "lst" "suo" "sdf" "vtg" "mdt" "bin")))
(setq projectile-globally-ignored-files (quote ("TAGS" "GTAGS" "GPATH" "GRTAGS")))

(setq projectile-completion-system 'helm)
;; after upgrade packages need to add this
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)

;; change projectile indexing method for Windows
;; (cond ((eq system-type 'windows-nt)
;;        (setq projectile-indexing-method 'alien)))
;; now change indexing to hybrid for all platforms, for new version of
;; projectile this is fast enough even on Windows, and also alien method
;; won't allow using .projectile file
;; I am using .projectile file to exclude some files/dirs
;; I am using .dir-locals.el to customize compilation dir and cmd2
(setq projectile-indexing-method 'hybrid)

(global-set-key (kbd "C-c h g") 'helm-ag)
(global-set-key (kbd "C-c h d") 'helm-do-ag)
;; automatically turn to wgrep mode if C-c C-s if pressed
(add-hook 'helm-ag-mode-hook 'wgrep-change-to-wgrep-mode t)

(setq auto-package-update-delete-old-version t)

(window-numbering-mode 1)

(if (eq system-type 'windows-nt)
    (setq magit-git-executable "c:\\Program Files\\Git\\bin\\git.exe")
  )
(global-set-key (kbd "C-x g") 'magit-status)
(global-set-key (kbd "C-x M-g") 'magit-dispatch-popup)
;; turn off diff when doing commit, use C-c C-d to see the diff manually
;; if not turned off, for large commits, it will be very slow, alternative
;; way is to C-g to cancel diff when committing, but will end up with an
;; broken diff buffer
(remove-hook 'server-switch-hook 'magit-commit-diff)

;; (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))
(require 'org-superstar)
(add-hook 'org-mode-hook (lambda () (org-superstar-mode 1)))

(require 'yasnippet)
(yas-reload-all)
(add-hook 'org-mode-hook #'yas-minor-mode)
(add-hook 'c-mode-hook #'yas-minor-mode)
(add-hook 'c++-mode-hook #'yas-minor-mode)
(add-hook 'python-mode-hook #'yas-minor-mode)
(add-hook 'sh-mode-hook #'yas-minor-mode)
(add-hook 'nroff-mode-hook #'yas-minor-mode)
(add-hook 'LaTeX-mode-hook #'yas-minor-mode)
(add-hook 'nxml-mode-hook #'yas-minor-mode)

;; (add-hook 'after-init-hook 'global-company-mode)
(add-hook 'c-mode-hook 'company-mode)
(add-hook 'c++-mode-hook 'company-mode)
(add-hook 'text-mode-hook 'company-mode)
(add-hook 'sh-mode-hook 'company-mode)
(add-hook 'python-mode-hook 'company-mode)
(add-hook 'emacs-lisp-mode-hook 'company-mode)
(add-hook 'org-mode-hook 'company-mode)
(add-hook 'lisp-mode-hook 'company-mode)

(add-hook 'c-mode-hook 'irony-mode)
(add-hook 'c++-mode-hook 'irony-mode)

;; Windows performance tweaks
;;
(when (boundp 'w32-pipe-read-delay)
  (setq w32-pipe-read-delay 0))
;; Set the buffer size to 64K on Windows (from the original 4K)
(when (boundp 'w32-pipe-buffer-size)
  (setq irony-server-w32-pipe-buffer-size (* 64 1024)))

(eval-after-load 'company
  '(add-to-list 'company-backends '(company-irony-c-headers company-irony)))
(add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)

(require 'delight)
;; (delight 'helm-mode)
;; (delight 'abbrev-mode)
;; (delight 'projectile-mode)
;; (delight 'company-mode)
;; (delight 'irony-mode)
;; (delight 'yas-minor-mode)
;; (delight 'helm-gtags-mode)
;; (delight 'org-src-mode)
;; (delight 'smartparens-mode)
;; (delight 'undo-tree-mode)
;; (delight 'which-key-mode)
;; (delight 'eldoc-mode')
(delight '((helm-mode nil helm)
           (abbrev-mode nil abbrev)
           (projectile-mode nil projectile)
           (company-mode nil company)
           (irony-mode nil irony)
           (yas-minor-mode nil yasnippet)
           (helm-gtags-mode nil helm-gtags)
           (org-src-mode nil org-src)
           (smartparens-mode nil smartparens)
           (undo-tree-mode nil undo-tree)
           (which-key-mode nil which-key)
           (eldoc-mode nil eldoc)
           (captain-mode nil captain)
           (org-indent-mode nil org-indent)
           (counsel-mode nil counsel)
           (ivy-mode nil ivy)
           ))

(cond ((eq system-type 'darwin)
       (exec-path-from-shell-initialize)))

(require 'bm)
(global-set-key (kbd "<C-f2>") 'bm-toggle)
(global-set-key (kbd "<f2>")   'bm-next)
(global-set-key (kbd "<S-f2>") 'bm-show-all)
;; (global-set-key (kbd "<S-f2>") 'bm-previous)
;; Click on fringe to toggle bookmarks, and use mouse wheel to move between them.
(global-set-key (kbd "<left-fringe> <mouse-5>") 'bm-next-mouse)
(global-set-key (kbd "<left-fringe> <mouse-4>") 'bm-previous-mouse)
(global-set-key (kbd "<left-fringe> <mouse-1>") 'bm-toggle-mouse)
;; If you would like to cycle through bookmarks in all open buffers, add the following line:
(setq bm-cycle-all-buffers t)

(setq TeX-save-query nil)
(cond ((eq system-type 'windows-nt)
       (setq TeX-view-program-list
             '(("Sumatra PDF"
                ("\"c:/Program Files/SumatraPDF/SumatraPDF.exe\" -reuse-instance"
                 (mode-io-correlate " -forward-search \"%b\" %n ") " %o"))))
       (setq TeX-view-program-selection
             '(((output-dvi style-pstricks) "dvips and start") (output-dvi "Yap")
               (output-pdf "Sumatra PDF") (output-html "start"))))
      )

(advice-add 'python-mode :before 'elpy-enable)
;; (add-hook 'elpy-mode-hook
;;           '(lambda () (local-unset-key (kbd "M-.") 'xref-find-definitions)))
(add-hook 'elpy-mode-hook
          '(lambda () (local-set-key (kbd "M-.") 'elpy-goto-definition)))
(add-hook 'elpy-mode-hook
          '(lambda () (local-set-key (kbd "M-]") 'xref-find-definitions)))

(which-key-mode)

(require 'undo-tree)
(global-undo-tree-mode)

(require 'smartparens-config)

(add-hook 'c-mode-hook 'smartparens-mode)
(add-hook 'c++-mode-hook 'smartparens-mode)
;; (add-hook 'text-mode-hook 'smartparens-mode)
(add-hook 'sh-mode-hook 'smartparens-mode)
(add-hook 'python-mode-hook 'smartparens-mode)
(add-hook 'emacs-lisp-mode-hook 'smartparens-mode)
(add-hook 'org-mode-hook 'smartparens-mode)
;; (add-hook 'lisp-mode-hook 'smartparens-mode)
;; (add-hook 'nxml-mode-hook 'smartparens-mode)

(define-key dired-mode-map (kbd "/") 'dired-narrow)

(global-captain-mode t)

;; don't capitalize in programming modes. Only in comment
;; Now I still have a problem, which is in comment, the first
;; word is not capitalized. Second sentence is fine.
(add-hook 'prog-mode-hook
   (lambda ()
     (setq captain-predicate
           (lambda () (nth 8 (syntax-ppss (point)))))))

;; Or for text modes, work all the time:
(add-hook 'text-mode-hook
          (lambda ()
            (setq captain-predicate (lambda () t))))

;; Or don't work in source blocks in Org mode:
(add-hook
 'org-mode-hook
 (lambda ()
   (setq captain-predicate
         (lambda () (not (org-in-src-block-p))))))


(setq sentence-end-double-space nil)

(global-set-key (kbd "C-:") 'avy-goto-char)   ;input 1 char
(global-set-key (kbd "C-'") 'avy-goto-char-2) ;input 2 chars
(global-set-key (kbd "M-g f") 'avy-goto-line) ;input chars at line start
(global-set-key (kbd "M-g w") 'avy-goto-word-1) ;input 1 char, goto word
(global-set-key (kbd "M-g e") 'avy-goto-word-0) ;input 0 char(many more options)
(avy-setup-default)
(global-set-key (kbd "C-c C-j") 'avy-resume)

(defun insert-date (prefix)
  "Insert the current date. With prefix-argument, use ISO format. With
   two prefix arguments, write out the day and month name."
  (interactive "P")
  (let ((format (cond
                 ;; ((not prefix) "%d.%m.%Y")
                 ;; ((equal prefix '(4)) "%Y-%m-%d")
                 ((not prefix) "%b. %d, %Y")))
        (system-time-locale "en_US"))
    (insert (format-time-string format))))

(defun toggle-window-split ()
  (interactive)
  (if (= (count-windows) 2)
      (let* ((this-win-buffer (window-buffer))
             (next-win-buffer (window-buffer (next-window)))
             (this-win-edges (window-edges (selected-window)))
             (next-win-edges (window-edges (next-window)))
             (this-win-2nd (not (and (<= (car this-win-edges)
                                         (car next-win-edges))
                                     (<= (cadr this-win-edges)
                                         (cadr next-win-edges)))))
             (splitter
              (if (= (car this-win-edges)
                     (car (window-edges (next-window))))
                  'split-window-horizontally
                'split-window-vertically)))
        (delete-other-windows)
        (let ((first-win (selected-window)))
          (funcall splitter)
          (if this-win-2nd (other-window 1))
          (set-window-buffer (selected-window) this-win-buffer)
          (set-window-buffer (next-window) next-win-buffer)
          (select-window first-win)
          (if this-win-2nd (other-window 1))))))

(defun my-find-file-check-make-large-file-read-only-hook ()
  "If a file is over a given size, make the buffer read only."
  (when (> (buffer-size) (* 1024 1024)) ; 1M size
    ;; (setq buffer-read-only t)
    (buffer-disable-undo)
    (fundamental-mode)))

(add-hook 'find-file-hook 'my-find-file-check-make-large-file-read-only-hook)

(define-key special-event-map [config-changed-event] #'ignore)

(server-start)
