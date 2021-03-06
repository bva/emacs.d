; layman -a emacs
;
; app-emacs/css-mode
; app-emacs/python-mode
; dev-python/ropemode
; app-emacs/pymacs
; dev-python/pylint
; app-emacs/yasnippet

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

(setq user-full-name "Vitaliy Batichko")
(setq user-login-name "vbatichko")
(setq mail-host-address "gmail.com")

(setq x-select-enable-clipboard t)

(show-paren-mode 1)
(setq inhibit-startup-message t)
(setq initial-scratch-message nil)

(autoload 'ibuffer "ibuffer" "List buffers." t)
(global-set-key (kbd "C-x C-b") 'ibuffer)

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
(setq major-mode 'text-mode)

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

(setq erlang-root-dir
      (shell-command-to-string
       "erl -noshell -eval 'io:format(\"~s\", [code:root_dir()]), erlang:halt().'"
       )
      )

(setq erlang-lib-dir
      (concat erlang-root-dir "/lib"))

(setq erlang-emacs-dir
      (concat
       erlang-lib-dir "/"
       (file-name-completion "tools" erlang-lib-dir)
       "emacs"
       )
      )

(add-to-list 'exec-path (concat erlang-root-dir "/bin"))

(setq django-mode-path (concat user-emacs-directory "django-mode"))
(setq js2-mode-path (concat user-emacs-directory "js2-mode"))
(setq distel-mode-path (concat user-emacs-directory "distel/elisp"))
(setq soy-mode-path (concat user-emacs-directory "soy-mode"))
(setq gjslint-mode-path (concat user-emacs-directory "closure-lint-mode"))

(add-to-list 'load-path django-mode-path)
(add-to-list 'load-path js2-mode-path)
(add-to-list 'load-path distel-mode-path)
(add-to-list 'load-path erlang-emacs-dir)
(add-to-list 'load-path soy-mode-path)
(add-to-list 'load-path gjslint-mode-path)
(add-to-list 'load-path user-emacs-directory)

(require 'cl)
(require 'saveplace)
(require 'ffap)
(require 'uniquify)
(require 'ansi-color)
(require 'recentf)
(require 'tramp)
(require 'adoc-mode)
(require 'site-gentoo)
(require 'django-html-mode)
(require 'django-mode)
(require 'js2-mode)
(require 'erlang-start)
(require 'ecb)
(require 'distel)
(require 'whitespace)
(require 'soy-mode)
(require 'closure-lint-mode)
(require 'auto-complete-config)

(global-auto-complete-mode t)
(ac-config-default)
(distel-setup)

(setq whitespace-style
      '(face trailing empty lines-tail trailing))

(yas/initialize)
(yas/load-directory "/usr/share/emacs/etc/yasnippet/snippets")
(yas/load-directory (concat django-mode-path "/snippets"))

(add-to-list 'auto-mode-alist '("\.js$" . js2-mode))
(add-to-list 'auto-mode-alist '("\.html$" . django-html-mode))
(add-to-list 'auto-mode-alist '("\.adoc$" . adoc-mode))

(pymacs-load "ropemacs" "rope-")

(defadvice flymake-start-syntax-check-process
  (after
   cheeso-advice-flymake-start-syntax-check-1
   (cmd args dir)
   activate compile)
  ;; set flag to allow exit without query on any
  ;;active flymake processes
  (set-process-query-on-exit-flag ad-return-value nil))

(setq js2-mode-hook
      '(lambda () (progn
		    (set-variable 'indent-tabs-mode nil))))
(add-hook 'js2-mode-hook
	  (lambda () (flymake-mode t)))

(when (load "flymake" t)
  (defun flymake-pylint-init ()
    (let* ((temp-file (flymake-init-create-temp-buffer-copy 'flymake-create-temp-inplace))
	   (local-file (file-relative-name temp-file (file-name-directory buffer-file-name))))
      (list "epylint" (list local-file))))
  (add-to-list 'flymake-allowed-file-name-masks '("\.py$" flymake-pylint-init)))

(semantic-load-enable-minimum-features)
(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(closure-lint-gjs-lint "gjslint")
 '(css-indent-offset 2)
 '(ecb-auto-activate t)
 '(ecb-auto-expand-tag-tree (quote all))
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
 '(js2-bounce-indent-p t)
 '(js2-cleanup-whitespace t)
 '(js2-dynamic-idle-timer-adjust 2)
 '(js2-enter-indents-newline t)
 '(js2-idle-timer-delay 0.2)
 '(vc-follow-symlinks t))
(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 )
