;; -*- emacs-lisp -*-

;; Personal elisp directory.
(add-to-list 'load-path "~/.emacs.d/lisp/")

;; Some enabled operations.
(put 'narrow-to-region 'disabled nil)
(put 'eval-expression 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

;; Don't auto insert empty lines at file end.
(setq-default next-line-add-newlines nil)

;; uniquify.
(require 'uniquify)

;; ido mode.
(require 'ido)
(ido-mode t)

;; Add our emacs Python directory to PYTHONPATH. It contains the
;; Pymacs and rope packages.
(setenv "PYTHONPATH"
  (concat
   (getenv "HOME") "/.emacs.d/python" path-separator
   (getenv "PYTHONPATH")
  )
)

;; Pymacs.
(autoload 'pymacs-apply "pymacs")
(autoload 'pymacs-call "pymacs")
(autoload 'pymacs-eval "pymacs" nil t)
(autoload 'pymacs-exec "pymacs" nil t)
(autoload 'pymacs-load "pymacs" nil t)
(autoload 'pymacs-autoload "pymacs")

;; Ropemacs.
(pymacs-load "ropemacs" "rope-")

;; YASnippet.
(require 'yasnippet)
(yas/initialize)
(yas/load-directory "~/.emacs.d/lisp/snippets")

;; nXhtml
(load "~/.emacs.d/lisp/nxhtml/autostart.el")
(setq debug-on-error nil)

;; CoffeeScript mode.
(require 'coffee-mode)
(add-hook 'coffee-mode-hook
  '(lambda() (set (make-local-variable 'tab-width) 4)))

;; Flymake:

;; Flymake for JavaScript (http://lapin-bleu.net/riviera/?p=191).
(when (load "flymake" t)
  (defun flymake-jslint-init ()
    (let* ((temp-file (flymake-init-create-temp-buffer-copy
		       'flymake-create-temp-inplace))
           (local-file (file-relative-name
                        temp-file
                        (file-name-directory buffer-file-name))))
      (list "~/.emacs.d/node_modules/.bin/jslint" (list "--terse" local-file))))

  (setq flymake-err-line-patterns
	(cons '("^\\(.*\\)(\\([[:digit:]]+\\)):\\(.*\\)$"
		1 2 nil 3)
	      flymake-err-line-patterns))

  (add-to-list 'flymake-allowed-file-name-masks
               '("\\.js\\'" flymake-jslint-init))

  (require 'flymake-cursor)
)

(add-hook 'js-mode-hook
	  (lambda ()
            (flymake-mode 1)
            (define-key js-mode-map "\C-c\C-n" 'flymake-goto-next-error)))

;; ;; Docstring edit
;; (load "/home/soto/Desktop/Projects/rdf-devel/docedit/docedit.el")

;; ;; Python + ReST with nxHtml's MuMaMo
;; (defun python-long-string (pos min max)
;;   "Find Python long strings.  Return range and 'mumamo-comment-mode.
;; See `mumamo-find-possible-chunk' for POS, MIN and MAX."
;;   (mumamo-quick-static-chunk pos min max "#<<<" "#>>>" t 'rst-mode t))

;; (define-mumamo-multi-major-mode python-rst-mumamo-mode
;;   "Turn on multiple major modes for Python with RestructuredText docstrings."
;;   ("Python ReST Family" python-mode
;;    (python-long-string)))

;; (defun python-docstring-rst ()
;;   "Mark the docstring containing the point as a ReST region."
;;   (interactive)
;;   (unless (eq major-mode 'python-rst-mumamo-mode)
;;     (python-rst-mumamo-mode))
;;   (save-excursion
;;     (search-backward "\"\"\"")
;;     (beginning-of-line)
;;     (insert "#<<<\n"))
;;   (save-excursion
;;     (search-forward "\"\"\"")
;;     (insert "#>>>")))

;; (defun python-docstring-py ()
;;   "Remove the ReST region marks around a docstring."
;;   (interactive)
;;   (unless (eq major-mode 'python-rst-mumamo-mode)
;;     (python-rst-mumamo-mode))
;;   (save-excursion
;;     (search-backward "#<<<\n")
;;     (delete-char 5))
;;   (save-excursion
;;     (search-forward "#>>>")
;;     (delete-backward-char 4)))

;; (defun add-python-mumamo-keys ()
;;   (define-key python-mode-map (kbd "<f7>") 'python-docstring-rst)
;;   (define-key python-mode-map (kbd "<f8>") 'python-docstring-py))
;; (add-hook 'python-mode-hook 'add-python-mumamo-keys)

;; (define-key python-rst-mumamo-mode-map (kbd "<f7>") 'python-docstring-rst)
;; (define-key python-rst-mumamo-mode-map (kbd "<f8>") 'python-docstring-py)

(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(add-log-full-name "Martin Soto")
 '(add-log-mailing-address "martinsoto@users.sourceforge.net")
 '(c-offsets-alist (quote ((statement-case-intro . 4) (statement-case-open . 0) (case-label . 0) (arglist-intro . 4) (arglist-cont-nonempty . 4))))
 '(compilation-scroll-output (quote first-error))
 '(cperl-indent-level 4)
 '(crypt-auto-decode-buffer t)
 '(crypt-auto-decode-insert nil)
 '(crypt-auto-write-buffer nil)
 '(crypt-auto-write-buffer-encrypted nil)
 '(crypt-bind-insert-file t)
 '(crypt-buffer-encoding-type nil t)
 '(crypt-buffer-encryption-key nil t)
 '(crypt-buffer-save-encoded nil t)
 '(crypt-buffer-save-encrypted nil t)
 '(crypt-compact-vs-C++ nil)
 '(crypt-confirm-password nil)
 '(crypt-default-encoding "gzip")
 '(crypt-encoded-disable-auto-save t)
 '(crypt-encrypted-disable-auto-save t)
 '(crypt-encryption-file-extension nil)
 '(crypt-encryption-type (quote gpg))
 '(crypt-freeze-vs-fortran t)
 '(crypt-ignored-filenames nil)
 '(crypt-never-ever-decrypt nil)
 '(crypt-no-extension-implies-plain t)
 '(crypt-query-if-interactive t)
 '(debian-changelog-mailing-address "donsoto@gmail.com")
 '(desktop-save (quote if-exists))
 '(desktop-save-mode t)
 '(global-whitespace-mode nil)
 '(indent-tabs-mode nil)
 '(nil nil t)
 '(show-trailing-whitespace t)
 '(uniquify-buffer-name-style (quote post-forward-angle-brackets) nil (uniquify))
 '(x-select-enable-clipboard t))
(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :background "white" :foreground "black" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 95 :width normal :foundry "unknown" :family "DejaVu Sans Mono"))))
 '(mumamo-background-chunk-major ((((class color) (min-colors 88) (background light)) (:background "white")))))
