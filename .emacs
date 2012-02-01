; layman -a emacs
;
; app-emacs/css-mode
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

(defun toggle-fullscreen (&optional f)
  (interactive)
  (let ((current-value (frame-parameter nil 'fullscreen)))
    (set-frame-parameter nil 'fullscreen
			 (if (equal 'fullboth current-value)
			     (if (boundp 'old-fullscreen) old-fullscreen nil)
			   (progn (setq old-fullscreen current-value)
				  'fullboth)))))
(global-set-key [f11] 'toggle-fullscreen)
;; Make new frames fullscreen by default. Note: this hook doesn't do
;; anything to the initial frame if it's in your .emacs, since that file is
;; read _after_ the initial frame is created.
(add-hook 'after-make-frame-functions 'toggle-fullscreen)

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

(setq cssm-indent-level 2)
(setq cssm-newline-before-closing-bracket t)
(setq cssm-indent-function #'cssm-c-style-indenter)
(setq cssm-mirror-mode t)

(display-time-mode)      ;; Show current time in status line

(global-set-key "\C-l" 'goto-line)

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

(setq django-mode-path (concat user-emacs-directory "django-mode"))
(setq js2-mode-path (concat user-emacs-directory "js2-mode"))
(add-to-list 'load-path django-mode-path)
(add-to-list 'load-path js2-mode-path)
(add-to-list 'load-path erlang-emacs-dir)

(require 'site-gentoo)
(require 'django-html-mode)
(require 'django-mode)
(require 'js2-mode)
(require 'erlang-start)
(require 'ecb)

(yas/initialize)
(yas/load-directory "/usr/share/emacs/etc/yasnippet/snippets")
(yas/load-directory (concat django-mode-path "/snippets"))

(setq erlang-root-dir "/usr/local/erlang")
(add-to-list 'exec-path "/usr/local/erlang/bin")
(setq erlang-man-root-dir "/usr/local/erlang/man")

(add-to-list 'auto-mode-alist '("\.js$" . js2-mode))
(add-to-list 'auto-mode-alist '("\.html$" . django-html-mode))

(pymacs-load "ropemacs" "rope-")

(setq js2-mode-hook
      '(lambda () (progn
		    (set-variable 'indent-tabs-mode nil))))

(when (load "flymake" t)
  (defun flymake-pylint-init ()
    (let* ((temp-file (flymake-init-create-temp-buffer-copy 'flymake-create-temp-inplace))
	  (local-file (file-relative-name temp-file (file-name-directory buffer-file-name))))
     (list "epylint" (list local-file))))
  (add-to-list 'flymake-allowed-file-name-masks '("\.py$" flymake-pylint-init)))

(maximize-frame)
(semantic-load-enable-minimum-features)
(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(ecb-auto-activate t)
 '(ecb-layout-name "left15")
 '(ecb-layout-window-sizes (quote (("left15" (0.2088607594936709 . 0.4918032786885246) (0.2088607594936709 . 0.4918032786885246)))))
 '(ecb-options-version "2.40")
 '(ecb-source-path (quote (("/home/impweb/impweb" "/impweb"))))
 '(ecb-tip-of-the-day nil)
 '(ecb-tree-indent 2)
 '(ecb-use-speedbar-instead-native-tree-buffer (quote dir))
 '(imenu-auto-rescan nil)
 '(js2-allow-member-expr-as-function-name t)
 '(js2-basic-offset 2)
 '(js2-cleanup-whitespace t)
 '(js2-dynamic-idle-timer-adjust 2)
 '(js2-idle-timer-delay 0.2))
(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 )
