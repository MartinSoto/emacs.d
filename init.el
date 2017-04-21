;; -*- emacs-lisp -*-

;; Fix broken encoding.
(define-coding-system-alias 'UTF-8 'utf-8)

;; Useful elisp functions
;; TODO: Move to a separate file.

(require 'package)
(add-to-list 'package-archives
         '("melpa" . "http://melpa.org/packages/") t)
(add-to-list 'package-archives
         '("gnu" . "http://elpa.gnu.org/packages/") t)
(package-initialize)

(when (not package-archive-contents)
  (package-refresh-contents))


(defun camelcase-to-constant (s)
  (upcase (let ((case-fold-search nil))
            (replace-regexp-in-string "\\([a-z]\\)\\([A-Z]\\)" "\\1_\\2" s))))


;; Remove the toolbar.
(tool-bar-mode 0)

;; Fix dead keys.
(require 'iso-transl)

;; Fix the Mac keyboard.
(when (eq system-type 'darwin)
  (setq mac-command-modifier 'meta)
  (setq mac-right-command-modifier 'meta)
  (setq mac-right-option-modifier 'control)

  (global-set-key (kbd "<home>") 'beginning-of-line)
  (global-set-key (kbd "<end>") 'end-of-line))

;; Some enabled operations.
(put 'narrow-to-region 'disabled nil)
(put 'eval-expression 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

;; Don't auto insert empty lines at file end.
(setq-default next-line-add-newlines nil)

;; Desktop save/auto-save.
(require 'desktop)
(desktop-save-mode 1)
(defun my-desktop-save ()
  (interactive)
  ;; Don't call desktop-save-in-desktop-dir, as it prints a message.
  (if (eq (desktop-owner) (emacs-pid))
      (desktop-save desktop-dirname)))
(run-at-time "2 min" 60 'my-desktop-save)

;; Show matching parentheses.
(show-paren-mode 1)

;; Highlight trailing whitespace.
(setq-default show-trailing-whitespace t)
; ...but not in the completions buffer.
(add-hook 'completion-list-mode-hook
          (lambda ()
	    (setq show-trailing-whitespace nil)))

;; Whitespace cleanup settings.
(setq whitespace-style (quote
  (face tabs spaces trailing lines space-before-tab newline indentation:space empty
        space-after-tab space-mark tab-mark newline-mark)))

;; Scrollbar position.
;(set-scroll-bar-mode 'right)

;; Theme.
(load-theme 'leuven t)

;;
;; Helm
;;

;; From http://tuhdo.github.io/helm-intro.html.

(require 'helm)
(require 'helm-config)

;; The default "C-x c" is quite close to "C-x C-c", which quits Emacs.
;; Changed to "C-c h". Note: We must set "C-c h" globally, because we
;; cannot change `helm-command-prefix-key' once `helm-config' is loaded.
(global-set-key (kbd "C-c h") 'helm-command-prefix)
(global-unset-key (kbd "C-x c"))

(global-set-key (kbd "M-x")                          'helm-M-x)
(global-set-key (kbd "C-x C-f")                      'helm-find-files)

(define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action) ; rebind tab to run persistent action
(define-key helm-map (kbd "C-i") 'helm-execute-persistent-action) ; make TAB works in terminal
(define-key helm-map (kbd "C-z")  'helm-select-action) ; list actions using C-z

(when (executable-find "curl")
  (setq helm-google-suggest-use-curl-p t))

(setq helm-split-window-in-side-p           t ; open helm buffer inside current window, not occupy whole other window
      helm-move-to-line-cycle-in-source     t ; move to end or beginning of source when reaching top or bottom of source.
      helm-ff-search-library-in-sexp        t ; search for library in `require' and `declare-function' sexp.
      helm-scroll-amount                    8 ; scroll 8 lines other window using M-<next>/M-<prior>
      helm-ff-file-name-history-use-recentf t)

(helm-mode 1)

;; End of Helm configuration.

;; uniquify: Pick up buffer names smartly for buffers with the same
;; file name but different directories.
(require 'uniquify)

;; Electric pairing.
(electric-pair-mode)

;; Auto-complete.
(require 'auto-complete)

;; Projectile.
(require 'projectile)
(projectile-mode)

(require 'helm-projectile)
(helm-projectile-on)

;; Python Elpy.
(elpy-enable)

(setq python-check-command (concat (getenv "HOME") "/.emacs.d/python/bin/pyflakes"))

;; Add our emacs Python directory to PYTHONPATH. It contains
;; all Elpy dependencies.
(setenv "PYTHONPATH"
        (let ((python-path (getenv "PYTHONPATH"))
              (local-path (concat (getenv "HOME") "/.emacs.d/python/lib/python2.7/site-packages")))
          (if python-path
              (concat local-path path-separator python-path)
            local-path)))

;; js2-mode for JavaScript (and React´s JSX)
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
(add-to-list 'auto-mode-alist '("\\.jsx?\\'" . js2-jsx-mode))
(add-to-list 'interpreter-mode-alist '("node" . js2-mode))
(add-to-list 'interpreter-mode-alist '("node" . js2-jsx-mode))

(add-hook 'js2-mode-hook 'ac-js2-mode)

;; Tide environment for TypeScript.
(defun setup-tide-mode ()
  (interactive)
  (tide-setup)
  (flycheck-mode +1)
  (setq flycheck-check-syntax-automatically '(save mode-enabled))
  (eldoc-mode +1)
  (tide-hl-identifier-mode +1)
  (company-mode +1))

;; Global TypeScript indentation.
(setq typescript-indent-level 2)

;; aligns annotation to the right hand side
;(setq company-tooltip-align-annotations t)

;; formats the buffer before saving
;(add-hook 'before-save-hook 'tide-format-before-save)

(add-hook 'typescript-mode-hook #'setup-tide-mode)

;; format options
(setq tide-format-options '(:insertSpaceAfterFunctionKeywordForAnonymousFunctions t :placeOpenBraceOnNewLineForFunctions nil))

;; Web mode
(require 'web-mode)
(defun web-mode-hook ()
  "Hooks for Web mode."
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-css-indent-offset 4)
  (setq web-mode-code-indent-offset 4)
)
(add-hook 'web-mode-hook  'web-mode-hook)
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.pt\\'" . web-mode))


;; Flycheck:

;; http://www.flycheck.org/manual/latest/index.html
(require 'flycheck)

;; turn on flychecking globally
(add-hook 'after-init-hook #'global-flycheck-mode)

;; Use local eslint if available.
(defun setup-local-eslint ()
    (interactive)
    (let ((local-eslint (expand-file-name "./node_modules/.bin/eslint" (projectile-project-root))))
      (setq flycheck-javascript-eslint-executable
            (and (file-exists-p local-eslint) local-eslint))))
(add-hook 'projectile-after-switch-project-hook 'setup-local-eslint)
 
;; disable jshint since we prefer eslint checking
(setq-default flycheck-disabled-checkers
              (append flycheck-disabled-checkers
                      '(javascript-jshint)))

;; Use eslint with js2-mode for js and jsx files.
(flycheck-add-mode 'javascript-eslint 'js2-mode)
(flycheck-add-mode 'javascript-eslint 'js2-jsx-mode)

;; disable json-jsonlist checking for json files
(setq-default flycheck-disabled-checkers
              (append flycheck-disabled-checkers
                      '(json-jsonlist)))

;; emacs-eclim for Java.
(require 'eclim)
;(setq eclimd-autostart t)
;(global-eclim-mode)

(custom-set-variables
  '(eclim-eclipse-dirs '("~/eclipse/java-neon/Eclipse.app/Contents/Eclipse"))
  '(eclim-executable "~/eclipse/java-neon/Eclipse.app/Contents/Eclipse/eclim"))

(require 'company)
(require 'company-emacs-eclim)
(company-emacs-eclim-setup)
(global-company-mode t)

;; Ensime for Scala
(require 'ensime)
(add-hook 'scala-mode-hook 'ensime-scala-mode-hook)

;; Markdown mode.
(autoload 'markdown-mode "markdown-mode"
   "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.text\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))

;; Magit.
(setq magit-last-seen-setup-instructions "1.4.0")

;; ;; Docstring edit
;; (load "/home/soto/Desktop/Projects/rdf-devel/docedit/docedit.el")

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(add-log-full-name "Martin Soto")
 '(add-log-mailing-address "martinsoto@users.sourceforge.net")
 '(c-offsets-alist
   (quote
    ((statement-case-intro . 4)
     (statement-case-open . 0)
     (case-label . 0)
     (arglist-intro . 4)
     (arglist-cont c-lineup-gcc-asm-reg 0))))
 '(cider-lein-parameters "repl :headless :host 0.0.0.0 :port 7788")
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
 '(custom-safe-themes
   (quote
    ("9e1e2e7590c2f443c09a3f6240a05ada53f06566a6873c37eeae10d13dc372c9" default)))
 '(debian-changelog-mailing-address "donsoto@gmail.com")
 '(desktop-path (quote (".")))
 '(desktop-restore-eager 5)
 '(desktop-save-mode 1)
 '(git-commit-summary-max-length 72)
 '(global-auto-revert-mode t)
 '(global-whitespace-mode nil)
 '(indent-tabs-mode nil)
 '(js-indent-level 2)
 '(js2-basic-offset 2)
 '(nil nil t)
 '(savehist-mode t)
 '(uniquify-buffer-name-style (quote post-forward-angle-brackets) nil (uniquify))
 '(x-select-enable-clipboard t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 ;'(default ((t (:height 110 :family "Ubuntu Mono"))))
 '(flymake-errline ((((class color) (background light)) (:underline "darkred"))))
 '(flymake-warnline ((((class color) (background light)) (:underline "blue"))))
 '(mumamo-background-chunk-major ((((class color) (min-colors 88) (background light)) (:background "white"))) t)
 '(web-mode-html-attr-name-face ((t (:inherit font-lock-variable-name-face))))
 '(web-mode-html-tag-bracket-face ((t (:inherit font-lock-comment-face))))
 '(web-mode-html-tag-face ((t (:inherit font-lock-function-name-face :weight bold)))))
(put 'scroll-left 'disabled nil)
