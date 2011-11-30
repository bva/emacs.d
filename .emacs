; layman -a emacs
;
; app-emacs/css-mode
; app-emacs/js2-mode
; app-emacs/python-mode
; dev-python/ropemode
; app-emacs/pymacs
; dev-python/pylint
; app-emacs/yasnippet

(defun maximize-frame (&optional frame)
"Maximize the selected FRAME."
(interactive)
(or frame
    (setq frame (selected-frame)))
(let ((pixels-per-col (/ (float (frame-pixel-width))
                         (frame-width)))
      (pixels-per-row (/ (float
                          (frame-pixel-height)) (frame-height))))
  (set-frame-size frame
                  ;; truncate or round?
                  (truncate (/
                             (x-display-pixel-width) pixels-per-col))
                  ;; reduce size to account for the toolbar
                  (truncate (/
                             (x-display-pixel-height) pixels-per-row)))
  (set-frame-position frame 0 0)))


;; Сохраняем десктоп между запусками
(setq desktop-save 't)
(desktop-save-mode 1)

(show-paren-mode 1)
(setq inhibit-startup-message t)
(setq initial-scratch-message nil)

(require 'cua-base)
(cua-mode t)

;; always end a file with a newline
(setq require-final-newline t)

(global-set-key (kbd "C-c c") 'compile)

;; Software that beeps just sucks
(setq visible-bell t)

;; Display full path name in status bar...
(add-hook 'find-file-hooks
	  '(lambda ()
	     (setq mode-line-buffer-identification 'buffer-file-truename)
	     )
	  )

;; Disable scrollbars
(set-scroll-bar-mode nil)

;; Disable menu bar
;(menu-bar-mode -1)

;; turn off the toolbar
(tool-bar-mode -1)

;; In every buffer, the line which contains the cursor will be fully
;; highlighted
(global-hl-line-mode 1)

;; format the title-bar to always include the buffer name
(setq frame-title-format "emacs - %b")

;; This makes the buffer scroll by only a single line when the up or
;; down cursor keys push the cursor (tool-bar-mode) outside the
;; buffer. The standard emacs behaviour is to reposition the cursor in
;; the center of the screen, but this can make the scrolling confusing

(setq scroll-step 1)

;; Show column-number in the mode line
(column-number-mode 1)

;; ===== Make Text mode the default mode for new buffers =====
;;
(setq default-major-mode 'text-mode)

;; ============================
;; Setup syntax, background, and foreground coloring
;; ============================
(set-background-color "Black")
(set-foreground-color "White")
(set-cursor-color "LightSkyBlue")
(blink-cursor-mode 'off)

(set-mouse-color "LightSkyBlue")

(set-face-foreground 'modeline "Yellow")
(set-face-background 'modeline "Black")
(global-font-lock-mode t)
(setq font-lock-maximum-decoration t)

;; alias y to yes and n to no
(fset 'yes-or-no-p 'y-or-n-p)

(set-language-environment 'UTF-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(setq x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING))
(prefer-coding-system 'utf-8-unix)

(require 'whitespace)
(setq whitespace-style '(trailing lines-tail empty)) ;turns on white space mode only for tabs
(global-whitespace-mode 1)

(display-time-mode)      ;; Show current time in status line

(global-set-key "\C-l" 'goto-line)
(setq django-mode-path (concat user-emacs-directory "django-mode"))

(setq erlang-root-dir "/usr/local/erlang")
(setq erlang-lib-dir
      (concat erlang-root-dir "/lib/erlang/lib"))

(setq erlang-emacs-dir
      (concat
       erlang-lib-dir
       "/"
       (file-name-completion
	"tools"
	erlang-lib-dir)
       "emacs"
       ))

(add-to-list 'load-path django-mode-path)
(add-to-list 'load-path erlang-emacs-dir)

(require 'site-gentoo)
(require 'django-html-mode)
(require 'django-mode)
(require 'erlang-start)

(yas/initialize)
(yas/load-directory "/usr/share/emacs/etc/yasnippet/snippets")
(yas/load-directory (concat django-mode-path "/snippets"))

(setq erlang-root-dir "/usr/local/erlang")
(add-to-list 'exec-path "/usr/local/erlang/bin")
(setq erlang-man-root-dir "/usr/local/erlang/man")

(add-to-list 'auto-mode-alist '("\.js$" . js2-mode))
(add-to-list 'auto-mode-alist '("\.html$" . django-html-mode))

(pymacs-load "ropemacs" "rope-")

(when (load "flymake" t)
  (defun flymake-pylint-init ()
    (let* ((temp-file (flymake-init-create-temp-buffer-copy 'flymake-create-temp-inplace))
	  (local-file (file-relative-name temp-file (file-name-directory buffer-file-name))))
     (list "epylint" (list local-file))))
  (add-to-list 'flymake-allowed-file-name-masks '("\.py$" flymake-pylint-init)))

(require 'ecb)
(ecb-activate)
(maximize-frame)
