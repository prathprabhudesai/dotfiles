;; ---- EMACS CONFIGURATION ----

(setq package-check-signature 'nil)
(setq user-full-name "Prathamesh Prabhudesai")
(setq user-mail-address "prathprabhudesai@gmail.com")

;; ---- PACKAGES ----

;; add repositories
(when (>= emacs-major-version 24)
  (require 'package)
  (unless package--initialized (package-initialize t))
  (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
  (add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/") t)
  )

;; refresh package contents
(when (not package-archive-contents) (package-refresh-contents))

;; install packages from the list if not installed
(defvar install-package-list '(
     auto-complete
     flex-autopair
     xcscope
     emamux
     markdown-mode
     dracula-theme
     company
     ido-vertical-mode
     smex
     company
     color-theme-sanityinc-tomorrow
     srcery-theme
     dashboard
     spacemacs-theme
     spaceline
     flycheck
     use-package
     ;; python support
     elpy
     py-autopep8
     ;; latex support
     auctex
     auto-complete-auctex
     pdf-tools
     auctex-latexmk
     latex-extra
     )
  )

(dolist (p install-package-list)
  (when (not (package-installed-p p))
    (package-refresh-contents)
    (package-install p)))

;; configure installed and exising packages

;; auto-pair-mode
(require 'flex-autopair)
(flex-autopair-mode 1)

;; auto-complete-mode
;;(require 'auto-complete-config)
;;(ac-config-default)
(add-hook 'after-init-hook 'global-company-mode)

;; xcscope: Cscope for emacs
(require 'xcscope)

;; emamux: emacs + tmux
(require 'emamux)

;; Ido vertical mode configuration
(ido-vertical-mode)
(setq ido-vertical-define-keys 'C-n-C-p-up-and-down)

;; ibuffer setup
(autoload 'ibuffer "ibuffer" "List buffers." t)

(setq ibuffer-formats '((mark modified read-only " " (name 40 40) " " (size 6 -1 :right) " " (mode 16 16 :center) " " (process 8 -1) " " filename)
			(mark " " (name 16 -1) " " filename))
      ibuffer-saved-filter-groups '(("default"
				     ("c" (mode . c-mode))
				     ("c++" (mode . c++-mode))
				     ("go" (mode . go-mode))
				     ("python" (mode . python-mode))
				     ("haskell" (mode . haskell-mode))
				     ("emacs" (or (name . "^\\*scratch\\*$") (name . "^\\*Messages\\*$"))))
				     ("dired" (mode . dired-mode)))
      ibuffer-elide-long-columns t
      ibuffer-eliding-string "&")

(add-hook 'ibuffer-mode-hook (lambda () (ibuffer-switch-to-saved-filter-groups "default")))

;; smex setup
(require 'smex)
(smex-initialize)

;; ---- BASIC EDITOR CONFIGURATION ----

;; Desktop mode
(desktop-save-mode 1)

;; Show matching paren
(show-paren-mode t)

;; Remove the splash screen
(setq inhibit-splash-screen t)

;; Remove the menu bar
(customize-set-variable 'menu-bar-mode nil)

;; Remove the tool bar
(customize-set-variable 'tool-bar-mode nil)

;; Remove the scroll bar
(customize-set-variable 'scroll-bar-mode nil)

;; Toggle fullscreen
(defun toggle-fullscreen()
  (interactive)
  (set-frame-parameter nil 'fullscreen (if (frame-parameter nil 'fullscreen) nil 'fullboth)))

;; Scroll one line at a time
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1))) ;; one line at a time
(setq mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling
(setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse~
(setq scroll-step 1)

;; Show line and column numbers on minibuffer
(line-number-mode 1)
(column-number-mode 1)

;; Enable delete selection mode
(delete-selection-mode 1)

;; Line numbers on left side
(autoload 'linum-mode "linum" "toggle line numbers on/off" t)
(if (display-graphic-p)
  (setq linum-format " %d")
  (setq linum-format "%4d | "))

;; Horizontal selection line
;;(global-hl-line-mode 1)

;; Enable Elpy
(elpy-enable)

;; Flycheck
(when (require 'flycheck nil t)
    (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
    (add-hook 'elpy-mode-hook 'flycheck-mode))

;; Enable autopep8
(require 'py-autopep8)
(add-hook 'elpy-mode-hook 'py-autopep8-enable-on-save)

;; Setup pdf-tools
(require 'use-package)
(use-package pdf-tools
  :pin manual
  :init (pdf-tools-install)
  :bind (:map pdf-view-mode-map
			  ("T" . pdf-annot-add-text-annotation)
			  ("D" . pdf-annot-delete)
			  ("t" . pdf-annot-add-highlight-markup-annotation)
			  ("j" . image-next-line)
			  ("k" . image-previous-line)
			  ("l" . image-forward-hscroll)
			  ("h" . image-backward-hscroll)
			  ("G" . pdf-view-last-page)
			  ("g" . nil)
			  ("gg" . pdf-view-first-page)
			  ("C-c C-c" . image-toggle-display)
			  ("C-s" . isearch-forward))
  :config
  (setq-default pdf-view-display-size 'fit-page)
  :custom
  (yas-minor-mode nil)
  (pdf-cache-image-limit 32)
  (pdf-view-max-image-width 2048)
  (pdf-view-resize-factor 1.8)
  (pdf-isearch-batch-mode t)
  (pdf-annot-activate-created-annotations t))

;;C style 

(defun config-indent-80andNoTrail()
  (setq whitespace-line-column 80) ;; limit line length
  (setq whitespace-style '(face lines-tail))
  ;;(add-hook 'prog-mode-hook 'whitespace-mode)
  (setq show-trailing-whitespace t)
  )

(setq auto-mode-alist (cons '("\\.c$" . c-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.h$" . c++-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.tcc$" . c++-mode) auto-mode-alist))

(c-add-style "my-cc-style" 
	     '("bsd"
	       (tab-width . 2)
	       (indent-tabs-mode . nil)
	       (c-basic-offset . 2)
	       (c-tab-always-indent . nil)
	       (c-hanging-braces-alist . ((brace-list-open)
									  (brace-list-close)
									  (brace-entry-open)
									  (brace-entry-close)
									  (statement-cont)
									  (block-close . c-snug-do-while)
									  (extern-lang-open after)
									  (namespace-open after)))
	       (c-hanging-colons-alist . ((member-init-intro before)
									  (inher-intro)
									  (case-label after)
									  (label after)
									  (access-label after)))
	       (c-cleanup-list . (scope-operator
							  defun-close-semi))))

(add-hook 'c-mode-common-hook (lambda () (progn
					   (c-set-style "my-cc-style")
					   (define-key c-mode-base-map "\C-m" 'c-context-line-break)
					   (c-toggle-auto-hungry-state t))))
(add-hook 'c-mode-common-hook 'config-indent-80andNoTrail)

(when window-system
  (setq frame-title-format '(buffer-file-name "%f" ("%b")))
  (tooltip-mode -1)
  (mouse-wheel-mode t)
  (blink-cursor-mode -1))

(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(prefer-coding-system 'utf-8)
(ansi-color-for-comint-mode-on)

(setq visible-bell t
      echo-keystrokes 0.1
      font-lock-maximum-decoration t
      inhibit-startup-message t
      transient-mark-mode t
      color-theme-is-global t
      shift-select-mode nil
      mouse-yank-at-point t
      next-line-add-newlines nil
      truncate-partial-width-windows nil
      uniquify-buffer-name-style 'forward
      ediff-window-setup-function 'ediff-setup-windows-plain
      xterm-mouse-mode t
      )

(add-to-list 'safe-local-variable-values '(lexical-binding . t))
(add-to-list 'safe-local-variable-values '(whitespace-line-column . 80))

;; Transparently open compressed files
(auto-compression-mode t)

;; Enable syntax highlighting for older Emacsen that have it off
(global-font-lock-mode t)

;; Highlight matching parentheses when the point is on them.
(show-paren-mode 1)

;; ido-mode is like magic pixie dust!
(when (> emacs-major-version 21)
  (ido-mode t)
  (setq ido-enable-prefix nil
        ido-enable-flex-matching t
        ido-create-new-buffer 'always
        ido-use-filename-at-point 'guess
        ido-max-prospects 10))

;;(set-default 'indicate-empty-lines t)
(set-default 'imenu-auto-rescan t)

(add-hook 'text-mode-hook 'turn-on-auto-fill)

(defvar coding-hook nil
  "Hook that gets run on activation of any programming mode.")

(defalias 'yes-or-no-p 'y-or-n-p)
(random t) ;; Seed the random-number generator

;; Hippie expand: at times perhaps too hip
(delete 'try-expand-line hippie-expand-try-functions-list)
(delete 'try-expand-list hippie-expand-try-functions-list)

;; nxhtml stuff
(setq mumamo-chunk-coloring 'submode-colored
      nxhtml-skip-welcome t
      indent-region-mode t
      rng-nxml-auto-validate-flag nil)

;; Associate modes with file extensions
(add-to-list 'auto-mode-alist '("COMMIT_EDITMSG$" . diff-mode))
(add-to-list 'auto-mode-alist '("\\.css$" . css-mode))
(add-to-list 'auto-mode-alist '("\\.ya?ml$" . yaml-mode))
(add-to-list 'auto-mode-alist '("\\.rb$" . ruby-mode))
(add-to-list 'auto-mode-alist '("Rakefile$" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.js\\(on\\)?$" . js2-mode))
(add-to-list 'auto-mode-alist '("\\.xml$" . nxml-mode))

(eval-after-load 'grep
  '(when (boundp 'grep-find-ignored-files)
    (add-to-list 'grep-find-ignored-files "target")
    (add-to-list 'grep-find-ignored-files "*.class")))

;; Default to unified diffs
(setq diff-switches "-u")

;; Cosmetics
(set-face-background 'vertical-border "white")
(set-face-foreground 'vertical-border "white")

(eval-after-load 'diff-mode
  '(progn
     (set-face-foreground 'diff-added "green4")
     (set-face-foreground 'diff-removed "red3")))

(eval-after-load 'mumamo
  '(eval-after-load 'zenburn
     '(ignore-errors (set-face-background
                      'mumamo-background-chunk-submode "gray22"))))

;; Platform-specific stuff
(when (eq system-type 'darwin)
  ;; Work around a bug on OS X where system-name is FQDN
  (setq system-name (car (split-string system-name "\\."))))

;; make emacs use the clipboard
(setq x-select-enable-clipboard t)

;; Get around the emacswiki spam protection
(add-hook 'oddmuse-mode-hook
          (lambda ()
            (unless (string-match "question" oddmuse-post)
              (setq oddmuse-post (concat "uihnscuskc=1;" oddmuse-post)))))

;; do not make backup files
(setq make-backup-files nil)

(setq-default org-export-html-postamble nil)

(put 'set-goal-column 'disabled nil)

(defun make-capture-frame ()
  "Create a new frame and run org-capture."
  (interactive)
  (make-frame '((name . "capture")))
  (select-frame-by-name "capture")
  (delete-other-windows)
  (org-capture)
  (delete-other-windows)
  )

;; For CamelCase Editing
(add-hook 'c-mode-common-hook
          (lambda () (subword-mode 1)))

;; ---- KEYBINDINGS ----

;; Full-screen
(global-set-key [f11] 'toggle-fullscreen)

;; White-space mode
(global-set-key (kbd "C-c W") 'whitespace-mode)

;; linum-mode
(global-set-key [C-:] 'linum-mode)

;; Comment lines
(global-set-key (kbd "C-c /") 'comment-or-uncomment-region)
(global-set-key (kbd "C-c *") 'comment-region)
(global-set-key (kbd "C-c C-u") 'uncomment-region)

;; Cscope
(global-set-key (kbd "\C-c s s") 'cscope-find-this-symbol)
(global-set-key (kbd "\C-c s d") 'cscope-find-global-definition)
(global-set-key (kbd "\C-c s g") 'cscope-find-global-definition)
(global-set-key (kbd "\C-c s G") 'cscope-find-global-definition-no-prompting)
(global-set-key (kbd "\C-c s c") 'cscope-find-functions-calling-this-function)
(global-set-key (kbd "\C-c s C") 'cscope-find-called-functions)
(global-set-key (kbd "\C-c s t") 'cscope-find-this-text-string)
(global-set-key (kbd "\C-c s e") 'cscope-find-egrep-pattern)
(global-set-key (kbd "\C-c s f") 'cscope-find-this-file)
(global-set-key (kbd "\C-c s i") 'cscope-find-files-including-file)
;; --- (The '---' indicates that this line corresponds to a menu separator.)
(global-set-key (kbd "\C-c s b") 'cscope-display-buffer)
(global-set-key (kbd "\C-c s B") 'cscope-display-buffer-toggle)
(global-set-key (kbd "\C-c s n") 'cscope-next-symbol)
(global-set-key (kbd "\C-c s N") 'cscope-next-file)
(global-set-key (kbd "\C-c s p") 'cscope-prev-symbol)
(global-set-key (kbd "\C-c s P") 'cscope-prev-file)
(global-set-key (kbd "\C-c s u") 'cscope-pop-mark)
;; ---
(global-set-key (kbd "\C-c s a") 'cscope-set-initial-directory)
(global-set-key (kbd "\C-c s A") 'cscope-unset-initial-directory)
;; ---
(global-set-key (kbd "\C-c s L") 'cscope-create-list-of-files-to-index)
(global-set-key (kbd "\C-c s I") 'cscope-index-files)
(global-set-key (kbd "\C-c s E") 'cscope-edit-list-of-files-to-index)
(global-set-key (kbd "\C-c s W") 'cscope-tell-user-about-directory)
(global-set-key (kbd "\C-c s S") 'cscope-tell-user-about-directory)
(global-set-key (kbd "\C-c s T") 'cscope-tell-user-about-directory)
(global-set-key (kbd "\C-c s D") 'cscope-dired-directory)

;; buffer
(global-set-key [C-tab] 'next-buffer)
(global-set-key [C-iso-lefttab] 'previous-buffer)
(global-set-key (kbd "C-x C-b") 'ibuffer)

;; goto line
(global-set-key (kbd "M-g") 'goto-line)

;; git blame
(global-set-key (kbd "C-c g b") 'vc-annotate)

;; smex keys
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)
;;------old M-x
(global-set-key (kbd "C-c M-x") 'execute-extended-command)

;; window movements
(when (fboundp 'windmove-default-keybindings)
  (windmove-default-keybindings))

;; 80 coloumn indicator
(defun sanityinc/fci-enabled-p () (symbol-value 'fci-mode))

(defvar sanityinc/fci-mode-suppressed nil)
(make-variable-buffer-local 'sanityinc/fci-mode-suppressed)

(defadvice popup-create (before suppress-fci-mode activate)
  "Suspend fci-mode while popups are visible"
  (let ((fci-enabled (sanityinc/fci-enabled-p)))
    (when fci-enabled
      (setq sanityinc/fci-mode-suppressed fci-enabled)
      (turn-off-fci-mode))))

(defadvice popup-delete (after restore-fci-mode activate)
  "Restore fci-mode when all popups have closed"
  (when (and sanityinc/fci-mode-suppressed
	     (null popup-instances))
    (setq sanityinc/fci-mode-suppressed nil)
    (turn-on-fci-mode)))

;; Dashboard Setup
(require 'dashboard)
(dashboard-setup-startup-hook)

;;---------- THEMES ---------------

;;(require 'spacemacs-theme)
(load-theme 'spacemacs-dark t)
(set-face-attribute 'region nil :background "#aaaaaa" :foreground "#000000")


;; Customizing colors used in diff mode
(defun custom-diff-colors ()
  "update the colors for diff faces"
  (set-face-attribute
   'diff-added nil :background nil :foreground "green")
  (set-face-attribute
   'diff-removed nil :background nil :foreground "red")
  (set-face-attribute
   'diff-changed nil :background nil :foreground "purple"))
(eval-after-load "diff-mode" '(custom-diff-colors))
