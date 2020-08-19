;;; --------------------------------------------------------
;;; Theo's Dotfile
;;; Written by Theodore Lovinski
;;; It's probably not written very well but oh well.
;;; --------------------------------------------------------

(package-initialize)

(setq win32 t)
(setq linux nil)
(when win32
	(setq ring-bell-function 'ignore))
(when linux
  ;; Use the mousepad to scroll left or right.
  (global-set-key (kbd "<mouse-6>") (lambda ()
									  (interactive)
									  (if truncate-lines (scroll-right 1))))
  (global-set-key (kbd "<mouse-7>") (lambda ()
									  (interactive)
									  (if truncate-lines (scroll-left 1)))))

(setq scroll-step 3)
(setq enable-local-variables nil)
(setq initial-scratch-message "")
(setq initial-major-mode 'text-mode)

(when win32 (setq default-directory "c:/Sources"))
(when linux (setq default-directory "~/Sources"))

(setq-default c-basic-offset 4)

;; Highlight TODOs, NOTEs, and HACKs
;; fix-modes contains the editing modes that we actually want to hightlight
;; these things.
(setq fix-modes '(c++-mode c-mode rust-mode elisp-mode julia-mode python-mode))
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
	  ("\\<\\(TEMP\\)" 1 'font-lock-fix-temp-face t))))
	fix-modes)
	
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector
   ["#2d3743" "#ff4242" "#74af68" "#dbdb95" "#34cae2" "#008b8b" "#00ede1" "#e1e1e0"])
 '(auto-save-default nil)
 '(column-number-mode t)
 '(completion-auto-help nil)
 '(confirm-kill-emacs nil)
 '(confirm-kill-processes nil)
 '(custom-enabled-themes nil)
 '(display-time-24hr-format t)
 '(display-time-format nil)
 '(display-time-mode t)
 '(fringe-mode 0 nil (fringe))
 '(icomplete-mode t)
 '(ivy-mode t)
 '(ivy-wrap t)
 '(make-backup-files nil)
 '(menu-bar-mode nil)
 '(mode-line-percent-position nil)
 '(mouse-wheel-follow-mouse nil)
 '(package-archives
   (quote
	(("gnu" . "https://elpa.gnu.org/packages/")
	 ("melpa" . "https://stable.melpa.org/packages/"))))
 '(package-selected-packages
   (quote
	(highlight-escape-sequences charmap highlight-numbers which-key swiper-helm smooth-scrolling rust-mode rainbow-delimiters python-mode pfuture markdown-mode lua-mode julia-mode js3-mode hydra ht helm-ag helm-ack flx elm-mode d-mode csv-mode csharp-mode counsel bind-key ahk-mode ace-window)))
 '(rainbow-delimiters-max-face-count 10)
 '(scroll-bar-mode nil)
 '(size-indication-mode nil)
 '(tab-width 4)
 '(tool-bar-mode nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :background "#282828" :foreground "#ebdbb2" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 110 :width condensed :foundry "outline" :family "Consolas"))))
 '(error ((t (:foreground "#fb4934" :weight semi-bold))))
 '(escape-glyph ((t (:foreground "#fb4934"))))
 '(fringe ((t nil)))
 '(highlight-numbers-number ((t (:inherit nil :foreground "#d3869b"))))
 '(ivy-current-match ((t (:background "deep sky blue" :foreground "black"))))
 '(mode-line ((t (:background "#ebdbb2" :foreground "black" :box nil))))
 '(mode-line-highlight ((t (:box (:line-width 2 :color "grey" :style released-button)))))
 '(mode-line-inactive ((t (:inherit mode-line :background "dark slate gray" :foreground "#f5f5f5" :box (:line-width -1 :color "grey40") :weight light))))
 '(region ((t (:background "blue"))))
 '(vertical-border ((((type w32 tty)) (:inherit mode-line-inactive))))
 '(warning ((t (:foreground "DarkOrange" :weight semi-bold)))))

(defun insert-timeofday ()
   (interactive "*")
   (insert (format-time-string "%A, %D, %T")))

(defun header-format ()
	"Add header guard for .h/.hpp file."
	(interactive)
	(setq basefilename (file-name-sans-extension (file-name-no-directory buffer-file-name)))
	(insert "#ifndef _")
	(push-mark)
	(insert basefilename)
	(upcase-region (mark) (point))
	(pop-mark)
	(insert "\n#define _")
	(push-mark)
	(insert basefilename)
	(upcase-region (mark) (point))
	(pop-mark)
	(insert "\n\n\n\n")
	(insert "#endif"))

;; Assign modes to specific filetypes.
;; TODO: Add more programming languages.
(setq auto-mode-alist
      (append
       '(("\\.cpp$" . c++-mode)
		 ("\\.hpp$" .c++-mode)
         ("\\.h$" . c++-mode)
         ("\\.c$" . c++-mode)
         ("\\.cc$" . c++-mode)
		 ("\\.rs$" . rust-mode)
		 ("\\.jl$" . julia-mode)
		 ("\\.lua$" . lua-mode)
		 ("\\.py$" . python-mode)
         ("\\.txt$" . indented-text-mode)
         ("\\.el$" . emacs-lisp-mode)
         )
	   auto-mode-alist))
 
(defun split-4-ways ()
	"Split the window in 4 directions from the origin."
	(interactive)
	(split-window-right)
	(split-window-below)
	(windmove-right)
	(split-window-below)
	(windmove-left))

(defun replace-string-in-place (FromString ToString)
  "Replace a string without moving point."
  (interactive "sReplace: \nsReplace: %s  With: ")
  (save-excursion
    (replace-string FromString ToString)
  ))
(define-key global-map [f8] 'replace-string-in-place)
	
;;; Change keybindings.
;; Quitting
(define-key global-map [C-c] 'abort-isearch)
(define-key global-map [C-x C-c] 'save-buffers-kill-emacs)

;; Files
(define-key global-map [C-x C-f] 'find-file)
(define-key global-map [C-x C-s] 'safe-buffer)
(define-key global-map [C-x C-w] 'write-file)
(define-key global-map [C-x C-i] 'insert-file)

;; Buffers
(define-key global-map [?\M-,] 'next-buffer)
(define-key global-map [?\M-.] 'previous-buffer)
(define-key global-map [C-x C-b] 'list-buffers)
(define-key global-map "\eb" 'kill-current-buffer)
(define-key global-map "\en" 'kill-current-buffer-and-window)
(define-key global-map "\em" 'delete-other-windows)

(define-key global-map [?\C-,] 'other-window)
(define-key global-map "\eo" 'split-window-horizontally)
(define-key global-map "\el" 'split-window-vertically)

;; Cursor Movement
(define-key global-map [C-right] 'forward-word)
(define-key global-map [C-left] 'backward-word)
(defun previous-blank-line ()
	"Go to next whitespace."
	(interactive)
	(search-backward-regexp "^[ \t]*\n"))
(define-key global-map [C-up] 'previous-blank-line)
(defun next-blank-line ()
  "Goes to next whitespace"
  (interactive)
  (forward-line)
  (search-forward-regexp "^[ \t]*\n")
  (forward-line -1))
(define-key global-map [C-down] 'next-blank-line)
(define-key global-map [M-up] 'previous-blank-line)
(define-key global-map [M-down] 'next-blank-line)
(define-key global-map [M-right] 'forward-word)
(define-key global-map [M-left] 'backward-word)
;; TODO: Replace M-> and M-<

;; Copy, Undo, and such.
(define-key global-map [?\C--] 'undo)
(define-key global-map [C-w] 'clipboard-copy)
(define-key global-map [M-w] 'clipboard-cut)
(define-key global-map [C-e] 'clipboard-yank)
(define-key global-map "\ez" 'kill-region)
(global-unset-key [C-x C-l])
(define-key global-map [C-x C-l] 'goto-line)

;; Toggle truncate lines.
(define-key global-map "\et" 'toggle-truncate-lines)

;; Delimiter Wrapping
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

(define-key global-map (kbd "C-(") 'surround-brackets)
(define-key global-map (kbd "C-{") 'surround-brackets-curly)

;; Word Case
;; This will retain the way it is defined in emacs.
	
;; Set all the face attributes.	
(set-face-attribute 'font-lock-builtin-face nil :foreground "#689d6a")                          ;; Builtin
(set-face-attribute 'font-lock-comment-face nil :foreground "#928374" :slant 'italic)           ;; Comment
(set-face-attribute 'font-lock-constant-face nil :foreground "#ebdbb2")                         ;; Constant
(set-face-attribute 'font-lock-doc-face nil :foreground "#ffe600" :slant 'italic :weight 'bold) ;; Doc
(set-face-attribute 'font-lock-function-name-face nil :foreground "#ebdbb2")                    ;; Function Name
(set-face-attribute 'font-lock-keyword-face nil :foreground "#689d6a")                          ;; Keyword
(set-face-attribute 'font-lock-string-face nil :foreground "#83a598")                           ;; String
(set-face-attribute 'font-lock-type-face nil :foreground "#8ec07c")                             ;; Type
(set-face-attribute 'font-lock-variable-name-face nil :foreground "#ebdbb2")                    ;; Variable Name
(add-hook 'foo-mode-hook 'highlight-numbers-mode)
(add-hook 'prog-mode-hook 'highlight-numbers-mode)

;(set-face-attribute 'font-lock-preprocessor-face nil :foreground "#689dba")   DISABLED         ;; C/C++ Preprocessor

;; Custom settings for fixme comment modifiers,
;; such as TODO, HACK, NOTE, and TEMP.
(set-face-attribute 'font-lock-fix-todo-face nil :slant 'italic :weight 'bold :inverse-video t)
(set-face-attribute 'font-lock-fix-note-face nil :slant 'italic :weight 'bold :inverse-video t)
(set-face-attribute 'font-lock-fix-hack-face nil :slant 'italic :weight 'bold :foreground "#fb4934" :background "#282828")
(set-face-attribute 'font-lock-fix-temp-face nil :slant 'italic :weight 'bold :inverse-video t)

(add-to-list 'default-frame-alist '(fullscreen . maximized))
(set-foreground-color "#ebdbb2")                                                                ;; Foreground
(set-background-color "#282828")                                                                ;; Background
(set-cursor-color "#ebdbb2")                                                                    ;; Cursor
