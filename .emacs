;;; --------------------------------------------------------
;;; Theo's Dotfile
;;; It's probably not written very well
;;; but oh well it works at the very least.
;;; --------------------------------------------------------

(package-initialize)

(setq win32 nil)
(setq linux t)
(setq laptop nil)

(when win32
  (setq ring-bell-function 'ignore)
  (setq default-directory "c:/Sources")
  (setq my-font "Consolas"))
(when linux
  (setq default-directory "~/Sources/")
  (setq my-font "Fira Code 12"))


(when laptop
  ;; Use the mousepad to scroll left or right.
  (global-set-key (kbd "<mouse-6>") (lambda ()
									  (interactive)
									  (if truncate-lines (scroll-right 1))))
  (global-set-key (kbd "<mouse-7>") (lambda ()
									  (interactive)
									  (if truncate-lines (scroll-left 1)))))

;; Highlight TODOs, NOTEs, and HACKs
;; fix-modes contains the editing modes that we actually want to hightlight
;; these things.

(setq fix-modes '(c++-mode ruby-mode c-mode rust-mode lisp-mode julia-mode python-mode))
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
	
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(auto-save-default nil)
 '(c-basic-offset 4)
 '(column-number-mode t)
 '(completion-auto-help nil)
 '(confirm-kill-emacs nil)

 '(confirm-kill-processes nil)
 '(custom-enabled-themes nil)
 '(display-time-24hr-format t)
 '(display-time-format nil)
 '(display-time-mode nil)
 '(enable-local-variables nil)
 '(fringe-mode 0 nil (fringe))
 '(icomplete-mode t)
 '(initial-major-mode (quote text-mode))
 '(initial-scratch-message "")
 '(ivy-mode t)
 '(ivy-wrap t)
 '(make-backup-files nil)
 '(menu-bar-mode nil)
 '(mode-line-percent-position nil)
 '(mouse-wheel-follow-mouse nil)
 '(package-archives
   (quote
	(("gnu" . "https://elpa.gnu.org/packages/")
	 ("melpa-stable" . "https://stable.melpa.org/packages/")
	 ("melpa" . "https://melpa.org/packages/"))))
 '(package-enable-at-startup t)
 '(package-selected-packages
   (quote
	(restart-emacs json-mode yaml-mode kotlin-mode julia-mode python-mode markdown-mode lua-mode ivy-hydra highlight-numbers rust-mode ivy notmuch)))
 '(rainbow-delimiters-max-face-count 10)
 '(scroll-bar-mode nil)
 '(scroll-step 3)
 '(size-indication-mode nil)
 '(tab-width 4)
 '(tool-bar-mode nil))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(error ((t (:foreground "#fb4934" :weight semi-bold))))
 '(escape-glyph ((t (:foreground "#fb4934"))))
 '(highlight-numbers-number ((t (:inherit nil :foreground "#ea6c73"))))
 '(ivy-current-match ((t (:background "deep sky blue" :foreground "black"))))
 '(mode-line ((t (:background "#cedece" :foreground "black" :box nil))))
 '(mode-line-highlight ((t (:box (:line-width 2 :color "grey" :style released-button)))))
 '(mode-line-inactive ((t (:inherit mode-line :background "dark slate gray" :foreground "#f5f5f5" :box (:line-width -1 :color "grey40") :weight light))))
 '(region ((t (:background "blue")))))
 
(defun insert-timeofday ()
   (interactive "*")
   (insert (format-time-string "%A, %D, %T")))

;; Assign modes to specific filetypes.
;; TODO: Add more programming languages.
(setq auto-mode-alist
      (append
       '(("\\.cpp$" . c++-mode)
		 ("\\.hpp$" .c++-mode)
         ("\\.h$" . c++-mode)
		 ("\\.hh" . c++-mode)
         ("\\.c$" . c++-mode)
         ("\\.cc$" . c++-mode)
		 ("\\.rs$" . rust-mode)
		 ("\\.jl$" . julia-mode)
		 ("\\.lua$" . lua-mode)
		 ("\\.py$" . python-mode)
         ("\\.txt$" . indented-text-mode)
         ("\\.el$" . emacs-lisp-mode)
		 ("\\.md$" . markdown-mode))
	   auto-mode-alist))
 
(defun split-4-ways ()
	"Split the window in 4 directions from the origin."
	(interactive)
	(split-window-right)
	(split-window-below)
	(windmove-right)
	(split-window-below)
	(windmove-left))

;;; Change keybindings.
;;; These may be weird idk.
(defun duplicate-line ()
  (interactive)
  (move-beginning-of-line 1)
  (kill-line)
  (yank)
  (open-line 1)
  (next-line 1)
  (yank))

(define-key global-map [C-c] 'abort-isearch)
(global-set-key (kbd "C-x C-c") 'save-buffers-kill-emacs)
(global-set-key (kbd "C-k") 'kill-whole-line)
(global-set-key (kbd "M-k") 'duplicate-line)
(global-set-key [M-return] 'save-buffer)
(global-set-key (kbd "M-.") 'next-buffer)
(global-set-key (kbd "C-.") 'previous-buffer)
(define-key global-map "\el" 'ivy-switch-buffer)
(define-key global-map "\eb" 'kill-current-buffer)
(define-key global-map "\em" 'delete-other-windows)
(define-key global-map [?\C-,] 'other-window)
(global-set-key "\eo" 'split-window-horizontally)
(global-set-key "\ev" 'split-window-vertically)

;; Cursor Movement
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

(define-key global-map [C-up] 'previous-blank-line)
(define-key global-map [C-down] 'next-blank-line)
(define-key global-map [C-right] 'forward-word)
(define-key global-map [C-left] 'backward-word)
(define-key global-map [M-up] 'previous-blank-line)
(define-key global-map [M-down] 'next-blank-line)
(define-key global-map [M-right] 'forward-word)
(define-key global-map [M-left] 'backward-word)
(define-key global-map [?\C--] 'undo)
(global-set-key (kbd "C-x C-l") 'goto-line)
(define-key global-map "\et" 'toggle-truncate-lines)

(defun surround-brackets ()
  "Surround current region with brackets"
  (interactive)
  (when (use-region-p)
    (save-excursion
      (let ((beg (region-beginning))
            (end (region-end)))
        (goto-char end)
        (insert ")")
        (goto-char beg)
        (insert "(")))))

(defun surround-brackets-curly ()
  "Surround current region with brackets"
  (interactive)
  (when (use-region-p)
    (save-excursion
      (let ((beg (region-beginning))
            (end (region-end)))
        (goto-char end)
        (insert "}")
        (goto-char beg)
        (insert "{")))))

(defun surround-single-quote ()
  "Surround current region with quotes"
  (interactive)
  (when (use-region-p)
    (save-excursion
      (let ((beg (region-beginning))
            (end (region-end)))
        (goto-char end)
        (insert "'")
        (goto-char beg)
        (insert "'")))))

(defun surround-double-quote ()
  "Surround current region with quotes"
  (interactive)
  (when (use-region-p)
    (save-excursion
      (let ((beg (region-beginning))
            (end (region-end)))
        (goto-char end)
        (insert "\"")
        (goto-char beg)
        (insert "\"")))))

(define-key global-map (kbd "C-(") 'surround-brackets)
(define-key global-map (kbd "C-)") 'surround-brackets)
(define-key global-map (kbd "C-{") 'surround-brackets-curly)
(define-key global-map (kbd "C-}") 'surround-brackets-curly)
(define-key global-map (kbd "C-\"") 'surround-double-quote)
(define-key global-map (kbd "C-\"") 'surround-double-quote)
(define-key global-map (kbd "C-'") 'surround-single-quote)
(define-key global-map (kbd "C-'") 'surround-single-quote)

; I copied this from Casey Muratori's .emacs file.
(defun casey-replace-string (FromString ToString)
  "Replace String without moving the pointer."
  (interactive "sReplace: \nsReplace: %s  With: ")
  (save-excursion
	(replace-string FromString ToString)))

(define-key global-map (kbd "<f8>") 'casey-replace-string)

(defun toggle-cursor-color ()
  "Change the cursor color when in overwrite mode."
  (if (equal "#f72a48" (face-background 'cursor))
	  (set-cursor-color "#ffe600")
	(set-cursor-color "#f72a48")))

(add-hook 'overwrite-mode-hook 'toggle-cursor-color)
(add-hook 'foo-mode-hook 'highlight-numbers-mode)
(add-hook 'prog-mode-hook 'highlight-numbers-mode)

;; Set all the face attributes.	
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
(set-face-attribute 'font-lock-fix-todo-face nil :slant 'italic :weight 'bold :inverse-video t)
(set-face-attribute 'font-lock-fix-note-face nil :slant 'italic :weight 'bold :inverse-video t)
(set-face-attribute 'font-lock-fix-hack-face nil :slant 'italic :weight 'bold :foreground "#fb4934" :background "#282828")
(set-face-attribute 'font-lock-fix-temp-face nil :slant 'italic :weight 'bold :inverse-video t)

(set-frame-font my-font nil t)                                                           ;; Set Font
(set-foreground-color "#cedece")                                                                ;; Foreground
(set-background-color "#202420")                                                                ;; Background
(set-cursor-color "#f72a48")                                                                       ;; Cursor
(add-to-list 'default-frame-alist '(fullscreen . maximized))
(cua-mode)
(blink-cursor-mode)
