; layman -a emacs
;
; app-emacs/css-mode
; app-emacs/js2-mode
; app-emacs/python-mode
; dev-python/ropemode
; app-emacs/pymacs
; dev-python/pylint
; app-emacs/yasnippet

(require 'site-gentoo)
(add-to-list
  	'load-path "/usr/local/erlang/lib/erlang/lib/tools-2.6.6.5/emacs")

(require 'django-html-mode)
(require 'django-mode)

(add-to-list 'auto-mode-alist '("\.html$" . django-html-mode))

(yas/initialize)
(yas/load-directory "/usr/share/emacs/etc/yasnippet/snippets")
(yas/load-directory "path-to/django-mode/snippets")

(setq erlang-root-dir "/usr/local/erlang")
(add-to-list 'exec-path "/usr/local/erlang/bin")
(setq erlang-man-root-dir "/usr/local/erlang/man")
(require 'erlang-start)

(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
(pymacs-load "ropemacs" "rope-")

(when (load "flymake" t)
 (defun flymake-pylint-init ()
   (let* ((temp-file
	    (flymake-init-create-temp-buffer-copy 'flymake-create-temp-inplace))
	            (local-file (file-relative-name temp-file (file-name-directory buffer-file-name))))
                    (list "epylint" (list local-file))))
		    (add-to-list 'flymake-allowed-file-name-masks
			                  '("\\.py\\'" flymake-pylint-init)))
(require 'ecb)
(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(ecb-options-version "2.40"))
(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 )
