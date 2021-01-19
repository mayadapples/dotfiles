;;; ------------------------------
;;; Theo's Dotfile
;;; It's probably not very good!
;;; ------------------------------
;;; lb -*- lexical-binding: t; -*-

;;; TODO List:
;;;    - Change faces and behavior of smartparens or just get rid of it and do something else.
;;;      At the minute smartparens is just disabled.
;;;    - Change some keybindings with the advent of new packages.

;;; ------------------------------
;;; GARBAGE COLLECTION
;;; ):
;;; ------------------------------
(setq startup/gc-cons-threshold most-positive-fixnum)
(setq gc-cons-threshold most-positive-fixnum)
(defun startup/reset-gc () (setq gc-cons-threshold startup/gc-cons-threshold))
(add-hook 'emacs-startup-hook 'startup/reset-gc)

;;; ------------------------------
;;; OPERATING SYSTEM SPECIFIC VARIABLES
;;; ------------------------------
(defvar windows nil)
(defvar debian t)
(defvar void nil)
(defvar laptop nil)

(setq my-font "Monospace 12")
(when windows
  (setq ring-bell-function 'ignore)
  (setq default-directory "c:/Sources")
  (setq my-font "Consolas"))
(when debian
  (setq my-font "Fira Code 12"))
(when void
  (setq my-font "Fixed 12"))

;; Use mousepad to scroll left and right on the laptop.
(when laptop
  (global-set-key (kbd "<mouse-6>") (lambda ()
				      (interactive)
				      (if truncate-lines (scroll-right 1))))
  (global-set-key (kbd "<mouse-7>") (lambda ()
				      (interactive)
				      (if truncate-lines (scroll-left 1)))))

;;; ------------------------------
;;; STRAIGHT.EL THINGS
;;; ------------------------------
(setq straight-use-package-by-default 1)
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'ivy)
(straight-use-package 'swiper)
(straight-use-package 'ivy-hydra)
(straight-use-package 'treemacs)
(straight-use-package 'highlight-numbers)
(straight-use-package 'restart-emacs)
(straight-use-package 'smartparens)
(when debian
  (straight-use-package 'fireplace)
  (straight-use-package 'powerline))

(require 'cc-mode)
(straight-use-package 'cc-mode)
(straight-use-package 'rust-mode)
(straight-use-package 'lua-mode)
(straight-use-package 'python-mode)
(straight-use-package 'ruby-mode)
(straight-use-package 'markdown-mode)

;;; ------------------------------
;;; CONFIG VARIABLES
;;; ------------------------------
(setq inhibit-splash-screen t)
(setq inhibit-startup-message t)
(scroll-bar-mode 0)
(tool-bar-mode 0)
(fringe-mode 0)
(menu-bar-mode 0)
(setq ivy-mode t)
(setq auto-save-mode 0)
(blink-cursor-mode 0)
(setq c-basic-offset 4)
(column-number-mode)
(display-time-mode 1)
(setq display-time-24hr-format 1)
(icomplete-mode)
(defvar initial-buffer-choice t)
(setq initial-major-mode (quote text-mode))
(setq initial-scratch-message "")
(setq make-backup-files 0)
(setq mode-line-percent-position 0)
(setq mouse-wheel-follow-mouse 0)
(setq scroll-step 3)
(size-indication-mode 0)
(setq tab-width 4)
(fset 'yes-or-no-p 'y-or-n-p)
(highlight-numbers-mode 1)
(smartparens-global-mode 1)

(when (or debian void)
  (powerline-center-theme))
(cua-mode)
(ivy-mode)

;;; ------------------------------
;;; MY FUNCTIONS
;;; ------------------------------
(defun insert-timeofday ()
   (interactive "*")
   (insert (format-time-string "%A, %D, %T")))
 
(defun split-4-ways ()
	"Split the window in 4 directions from the origin."
	(interactive)
	(split-window-right)
	(split-window-below)
	(windmove-right)
	(split-window-below)
	(windmove-left))

;; Change keybindings.
;; These may be weird idk.
(defun duplicate-line ()
  (interactive)
  (move-beginning-of-line 1)
  (kill-line)
  (yank)
  (open-line 1)
  (next-line 1)
  (yank))

(defun previous-blank-line ()
	"Go to next whitespace."
	(interactive)
	(search-backward-regexp "^[ \t]*\n"))

(defun next-blank-line ()
  "Goes to next whitespace"
  (interactive)
  (forward-line)
  (search-forward-regexp "^[ \t]*\n")
  (forward-line -1))

; I copied this from Casey Muratori's .emacs file.
(defun casey-replace-string (FromString ToString)
  "Replace String without moving the pointer."
  (interactive "sReplace: \nsReplace: %s  With: ")
  (save-excursion
    (replace-string FromString ToString)))

;;; ------------------------------
;;; KEYBOARD SHORTCUTS
;;; ------------------------------

;; Buffers
(define-key global-map "\el" 'ivy-switch-buffer)
(global-set-key (kbd "M-.") 'next-buffer)
(global-set-key (kbd "C-.") 'previous-buffer)
(define-key global-map [?\C-,] 'other-window)
(global-set-key [M-return] 'save-buffer)
;;               C-x C-s
(global-set-key (kbd "C-x C-c") 'save-buffers-kill-emacs)
(define-key global-map "\eb" 'kill-current-buffer)
(define-key global-map "\em" 'delete-other-windows)
(global-set-key "\eh" 'split-window-horizontally)
(global-set-key "\ev" 'split-window-vertically)
(global-set-key "\ef" 'treemacs)

;; isearch
(define-key global-map (kbd "C-c") 'abort-isearch)
(define-key global-map (kbd "C-s") 'swiper-isearch)
(define-key global-map (kbd "C-M-s") 'swiper-isearch-thing-at-point)

;; Lines
(global-set-key (kbd "C-k") 'kill-whole-line)
(global-set-key (kbd "M-k") 'duplicate-line)

;; Navigation 
(define-key global-map [C-up] 'previous-blank-line)
(define-key global-map [C-down] 'next-blank-line)
(define-key global-map [C-right] 'forward-word)
(define-key global-map [C-left] 'backward-word)
(define-key global-map [M-up] 'previous-blank-line)
(define-key global-map [M-down] 'next-blank-line)
(define-key global-map [M-right] 'forward-word)
(define-key global-map [M-left] 'backward-word)
(global-set-key (kbd "C-x C-l") 'goto-line)

;; Misc
(define-key global-map "\et" 'toggle-truncate-lines)
(define-key global-map (kbd "<f8>") 'casey-replace-string)
(define-key global-map [?\C--] 'undo)

;;; ------------------------------
;;; FACES
;;; ------------------------------

;; User-defined face functions.
(defun toggle-cursor-color ()
  "Change the cursor color when in overwrite mode."
  (if (equal "#f72a48" (face-background 'cursor))
      (set-cursor-color "#ffe600")
    (set-cursor-color "#f72a48")))

(setq fix-modes '(c++-mode c-mode rust-mode lisp-mode elisp-mode))
(make-face 'font-lock-fix-todo-face)
(make-face 'font-lock-fix-note-face)
(make-face 'font-lock-fix-hack-face)
(make-face 'font-lock-fix-temp-face)
(mapc (lambda (mode)
	(font-lock-add-keywords
	mode
	'(("\\<\\(TODO\\)" 1 'font-lock-fix-todo-face t)
	  ("\\<\\(NOTE\\)" 1 'font-lock-fix-note-face t)
	  ("\\<\\(HACK\\)" 1 'font-lock-fix-note-face t)
	  ("\\<\\(TEMP\\)" 1 'font-lock-fix-temp-face t)
	  ("#\\(TODO\\)" 1 'font-lock-fix-todo-face t)
	  ("#\\(NOTE\\)" 1 'font-lock-fix-note-face t)
	  ("#\\(HACK\\)" 1 'font-lock-fix-note-face t)
	  ("#\\(TEMP\\)" 1 'font-lock-fix-temp-face t))))
      fix-modes)

(add-hook 'overwrite-mode-hook 'toggle-cursor-color)
(add-hook 'foo-mode-hook 'highlight-numbers-mode)
(add-hook 'prog-mode-hook 'highlight-numbers-mode)

;; Emacs faces.
(set-face-attribute 'escape-glyph nil
					:foreground "#fb4934"
					:slant 'italic
					:inverse-video t)
(set-face-attribute 'highlight-numbers-number nil
					:inherit nil
					:foreground "#ea6c73"
					:weight 'normal)
(set-face-attribute 'mode-line nil
					:inherit nil
					:background "#cedece"
					:foreground "#202420"
					:weight 'bold)
(set-face-attribute 'mode-line-inactive nil
					:inherit 'mode-line
					:background "dark slate gray"
					:foreground "#f5f5f5"
					:weight 'light)
(set-face-attribute 'region nil
					:background "blue")
(set-face-attribute 'error nil
					:foreground "fb4934"
					:weight 'semi-bold)
(set-face-attribute 'ivy-current-match nil
					:foreground "#cedece"
					:background "blue"
					:weight 'semi-bold)

;; Major mode faces.
(set-face-attribute 'font-lock-builtin-face nil :foreground "#91b362")                          ;; Builtin
(set-face-attribute 'font-lock-comment-face nil :foreground "#928374" :slant 'italic)           ;; Comment
(set-face-attribute 'font-lock-constant-face nil :foreground "#cedece" :weight 'bold)           ;; Constant
(set-face-attribute 'font-lock-doc-face nil :foreground "#ffe600" :slant 'italic :weight 'bold) ;; Doc
(set-face-attribute 'font-lock-function-name-face nil :foreground "#cedece")                    ;; Function Name
(set-face-attribute 'font-lock-keyword-face nil :foreground "#53bdfa")                          ;; Keyword
(set-face-attribute 'font-lock-string-face nil :foreground "#ffb454")                           ;; String
(set-face-attribute 'font-lock-type-face nil :foreground "#91b362")                             ;; Type
(set-face-attribute 'font-lock-variable-name-face nil :foreground "#cedece")                    ;; Variable Name
(set-face-attribute 'font-lock-preprocessor-face nil :foreground "#91b362")                     ;; C/C++ Preprocessor

;; Custom settings for fixme comment modifiers,
;; such as TODO, HACK, NOTE, and TEMP.
(set-face-attribute 'font-lock-fix-todo-face nil
					:slant 'italic
					:weight 'bold
					:inverse-video t)
(set-face-attribute 'font-lock-fix-note-face nil
					:slant 'italic
					:weight 'bold
					:inverse-video t)
(set-face-attribute 'font-lock-fix-hack-face nil
					:slant 'italic
					:weight 'bold
					:foreground "#fb4934"
					:background "#282828")
(set-face-attribute 'font-lock-fix-temp-face nil
					:slant 'italic
					:weight 'bold
					:inverse-video t)

(set-frame-font my-font nil t)                                                                  ;; Set Font
(set-foreground-color "#cedece")                                                                ;; Foreground
(set-background-color "#202420")                                                                ;; Background
(set-cursor-color "#f72a48")                                                                    ;; Cursor
