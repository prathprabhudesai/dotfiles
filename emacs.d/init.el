;; ---- EMACS CONFIGURATION ----

(package-initialize)
(setq package-check-signature 'nil)
(setq user-full-name "Prathamesh Prabhudesai")
(setq user-mail-address "prathprabhudesai@gmail.com")

;; ---- PACKAGES ----

;; add repositories
(when (>= emacs-major-version 24)
  (require 'package)
  (package-initialize)
  (add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)
  (add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/") t)
  )

;; refresh package contents
(when (not package-archive-contents) (package-refresh-contents))

;; install packages from the list if not installed
(defvar install-package-list '(
    color-theme
     auto-complete
     autopair
     xcscope
     markdown-mode
     dracula-theme
     company
     ido-vertical-mode
     smex
     )
  )

(dolist (p install-package-list)
  (when (not (package-installed-p p))
    (package-refresh-contents)
    (package-install p)))

;; configure installed and exising packages

;; auto-pair-mode
(require 'autopair)
(autopair-global-mode)
(setq autopair-autowrap t)

;; auto-complete-mode
(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories (concat dotfiles-dir "/extensions/auto-complete/ac-dict"))
(ac-config-default)

;; xcscope: Cscope for emacs
(require 'xcscope)

;; emamux: emacs + tmux
(require 'emamux)

;; Ido vertical mode configuration
(ido-vertical-mode)
(setq ido-vertical-define-keys 'C-n-C-p-up-and-down)

;; ---- BASIC EDITOR CONFIGURATION ----

;; Desktop mode
(desktop-save-mode 1)

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
  (setq linum-format "%4d | ")
  )

(global-hl-line-mode 1)
;;C indentation 

(defun config-indent-80andNoTrail()
  (setq whitespace-line-column 80) ;; limit line length
  (setq whitespace-style '(face lines-tail))
  (add-hook 'prog-mode-hook 'whitespace-mode)
  (setq show-trailing-whitespace t)
  )

(defun config-indent-linux()
  (setq c-default-style "linux")
  ;; Use TABs of length of 8
  (setq indent-tabs-mode 1
	tab-width 4
	c-basic-offset 4)
  )
(add-hook 'c-mode-hook 'config-indent-linux)
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
      require-final-newline t
      truncate-partial-width-windows nil
      uniquify-buffer-name-style 'forward
      ediff-window-setup-function 'ediff-setup-windows-plain
      oddmuse-directory (concat dotfiles-dir "oddmuse")
      xterm-mouse-mode t
      save-place-file (concat dotfiles-dir "places"))

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

;; Don't clutter up directories with files~
(setq backup-directory-alist `(("." . ,(expand-file-name
                                        (concat dotfiles-dir "backups")))))

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

;;goto line
(global-set-key (kbd "M-g") 'goto-line)

;;git blame
(global-set-key (kbd "C-c g b") 'vc-annotate)
