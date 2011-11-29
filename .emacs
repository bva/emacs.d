; layman -a emacs
;
; app-emacs/css-mode
; app-emacs/js2-mode
; app-emacs/python-mode
; dev-python/ropemode
; app-emacs/pymacs
; dev-python/pylint
; app-emacs/yasnippet

(setq django-mode-path (concat user-emacs-directory "django-mode"))
(add-to-list 'load-path django-mode-path)
(add-to-list 'load-path
	     "/usr/local/erlang/lib/erlang/lib/tools-2.6.6.5/emacs")
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
