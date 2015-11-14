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
(set-scroll-bar-mode 'right)

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

;; Autopair.
(require 'autopair)
; Fix Python triple quoting.
(add-hook 'python-mode-hook
          #'(lambda ()
              (setq autopair-handle-action-fns
                    (list #'autopair-default-handle-action
                          #'autopair-python-triple-quote-action))))
(autopair-global-mode)

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
 '(icicle-TAB/S-TAB-only-completes-flag t)
 '(icicle-top-level-key-bindings
   (quote
    (([pause]
      icicle-switch-to/from-minibuffer t)
     ("`" icicle-search-generic t)
     ("$" icicle-search-word t)
     ("^" icicle-search-keywords t)
     ("'" icicle-occur t)
     ("=" icicle-imenu t)
     ("\"" icicle-search-text-property t)
     ("/" icicle-complete-thesaurus-entry
      (fboundp
       (quote icicle-complete-thesaurus-entry)))
     ([24 134217829]
      icicle-execute-named-keyboard-macro t)
     (" " icicle-command-abbrev t)
     ("5o" icicle-select-frame t)
     ("" icicle-describe-option-of-type t)
     ([S-f4]
      icicle-kmacro t)
     (abort-recursive-edit icicle-abort-recursive-edit t)
     (apropos icicle-apropos t)
     (apropos-command icicle-apropos-command t)
     (apropos-value icicle-apropos-value t)
     (apropos-variable icicle-apropos-option
                       (fboundp
                        (quote icicle-apropos-option)))
     (apropos-variable icicle-apropos-variable
                       (not
                        (fboundp
                         (quote icicle-apropos-option))))
     (apropos-zippy icicle-apropos-zippy t)
     (bookmark-jump icicle-bookmark t)
     (bookmark-jump-other-window icicle-bookmark-other-window t)
     (bookmark-set icicle-bookmark-cmd t)
     (customize-apropos icicle-customize-apropos t)
     (customize-apropos-faces icicle-customize-apropos-faces t)
     (customize-apropos-groups icicle-customize-apropos-groups t)
     (customize-apropos-options icicle-customize-apropos-options t)
     (customize-face icicle-customize-face t)
     (customize-face-other-window icicle-customize-face-other-window t)
     (dabbrev-completion icicle-dabbrev-completion
                         (< emacs-major-version 24))
     (delete-window icicle-delete-window t)
     (delete-windows-for icicle-delete-window t)
     (dired icicle-dired t)
     (dired-other-window icicle-dired-other-window t)
     (exchange-point-and-mark icicle-exchange-point-and-mark t)
     (find-file icicle-file t)
     (find-file-other-window icicle-file-other-window t)
     (find-file-read-only icicle-find-file-read-only t)
     (find-file-read-only-other-window icicle-find-file-read-only-other-window t)
     (insert-buffer icicle-insert-buffer t)
     (kill-buffer icicle-kill-buffer t)
     (kill-buffer-and-its-windows icicle-kill-buffer t)
     (minibuffer-keyboard-quit icicle-abort-recursive-edit
                               (fboundp
                                (quote minibuffer-keyboard-quit)))
     (other-window icicle-other-window-or-frame t)
     (other-window-or-frame icicle-other-window-or-frame t)
     (pop-global-mark icicle-goto-global-marker-or-pop-global-mark t)
     (repeat-complex-command icicle-repeat-complex-command t)
     (set-mark-command icicle-goto-marker-or-set-mark-command t)
     (switch-to-buffer icicle-buffer t)
     (switch-to-buffer-other-window icicle-buffer-other-window t)
     (where-is icicle-where-is t)
     (yank icicle-yank-maybe-completing t)
     (yank-pop icicle-yank-pop-commands
               (featurep
                (quote second-sel)))
     (yank-pop-commands icicle-yank-pop-commands
                        (featurep
                         (quote second-sel)))
     (zap-to-char icicle-zap-to-char
                  (fboundp
                   (quote read-char-by-name)))
     ("jt" icicle-find-file-tagged
      (featurep
       (quote bookmark+)))
     ("4jt" icicle-find-file-tagged-other-window
      (featurep
       (quote bookmark+)))
     (bmkp-autofile-set icicle-bookmark-a-file
                        (fboundp
                         (quote bmkp-bookmark-a-file)))
     (bmkp-tag-a-file icicle-tag-a-file
                      (fboundp
                       (quote bmkp-tag-a-file)))
     (bmkp-untag-a-file icicle-untag-a-file
                        (fboundp
                         (quote bmkp-untag-a-file)))
     (bmkp-find-file icicle-find-file-handle-bookmark
                     (fboundp
                      (quote bmkp-find-file)))
     (bmkp-find-file-other-window icicle-find-file-handle-bookmark-other-window
                                  (fboundp
                                   (quote bmkp-find-file-other-window)))
     (bmkp-autofile-jump icicle-bookmark-autofile
                         (fboundp
                          (quote bmkp-autofile-jump)))
     (bmkp-autofile-jump-other-window icicle-bookmark-autofile-other-window
                                      (fboundp
                                       (quote bmkp-autofile-jump)))
     (bmkp-autonamed-jump icicle-bookmark-autonamed
                          (fboundp
                           (quote bmkp-autonamed-jump)))
     (bmkp-autonamed-jump-other-window icicle-bookmark-autonamed-other-window
                                       (fboundp
                                        (quote bmkp-autonamed-jump)))
     (bmkp-autonamed-this-buffer-jump icicle-bookmark-autonamed-this-buffer
                                      (fboundp
                                       (quote bmkp-autonamed-this-buffer-jump)))
     (bmkp-bookmark-file-jump icicle-bookmark-bookmark-file
                              (fboundp
                               (quote bmkp-bookmark-file-jump)))
     (bmkp-bookmark-list-jump icicle-bookmark-bookmark-list
                              (fboundp
                               (quote bmkp-bookmark-list-jump)))
     (bmkp-desktop-jump icicle-bookmark-desktop
                        (fboundp
                         (quote bmkp-desktop-jump)))
     (bmkp-dired-jump icicle-bookmark-dired
                      (fboundp
                       (quote bmkp-dired-jump)))
     (bmkp-dired-jump-other-window icicle-bookmark-dired-other-window
                                   (fboundp
                                    (quote bmkp-dired-jump)))
     (bmkp-file-jump icicle-bookmark-file
                     (fboundp
                      (quote bmkp-file-jump)))
     (bmkp-file-jump-other-window icicle-bookmark-file-other-window
                                  (fboundp
                                   (quote bmkp-file-jump)))
     (bmkp-file-this-dir-jump icicle-bookmark-file-this-dir
                              (fboundp
                               (quote bmkp-file-this-dir-jump)))
     (bmkp-file-this-dir-jump-other-window icicle-bookmark-file-this-dir-other-window
                                           (fboundp
                                            (quote bmkp-file-this-dir-jump)))
     (bmkp-gnus-jump icicle-bookmark-gnus
                     (fboundp
                      (quote bmkp-gnus-jump)))
     (bmkp-gnus-jump-other-window icicle-bookmark-gnus-other-window
                                  (fboundp
                                   (quote bmkp-gnus-jump)))
     (bmkp-image-jump icicle-bookmark-image
                      (fboundp
                       (quote bmkp-image-jump)))
     (bmkp-image-jump-other-window icicle-bookmark-image-other-window
                                   (fboundp
                                    (quote bmkp-image-jump)))
     (bmkp-info-jump icicle-bookmark-info
                     (fboundp
                      (quote bmkp-info-jump)))
     (bmkp-info-jump-other-window icicle-bookmark-info-other-window
                                  (fboundp
                                   (quote bmkp-info-jump)))
     (bmkp-local-file-jump icicle-bookmark-local-file
                           (fboundp
                            (quote bmkp-local-file-jump)))
     (bmkp-local-file-jump-other-window icicle-bookmark-local-file-other-window
                                        (fboundp
                                         (quote bmkp-local-file-jump)))
     (bmkp-man-jump icicle-bookmark-man
                    (fboundp
                     (quote bmkp-man-jump)))
     (bmkp-man-jump-other-window icicle-bookmark-man-other-window
                                 (fboundp
                                  (quote bmkp-man-jump)))
     (bmkp-non-file-jump icicle-bookmark-non-file
                         (fboundp
                          (quote bmkp-non-file-jump)))
     (bmkp-non-file-jump-other-window icicle-bookmark-non-file-other-window
                                      (fboundp
                                       (quote bmkp-non-file-jump)))
     (bmkp-region-jump icicle-bookmark-region
                       (fboundp
                        (quote bmkp-region-jump)))
     (bmkp-region-jump-other-window icicle-bookmark-region-other-window
                                    (fboundp
                                     (quote bmkp-region-jump)))
     (bmkp-remote-file-jump icicle-bookmark-remote-file
                            (fboundp
                             (quote bmkp-remote-file-jump)))
     (bmkp-remote-file-jump-other-window icicle-bookmark-remote-file-other-window
                                         (fboundp
                                          (quote bmkp-remote-file-jump)))
     (bmkp-specific-buffers-jump icicle-bookmark-specific-buffers
                                 (fboundp
                                  (quote bmkp-specific-buffers-jump)))
     (bmkp-specific-buffers-jump-other-window icicle-bookmark-specific-buffers-other-window
                                              (fboundp
                                               (quote bmkp-specific-buffers-jump)))
     (bmkp-specific-files-jump icicle-bookmark-specific-files
                               (fboundp
                                (quote bmkp-specific-files-jump)))
     (bmkp-specific-files-jump-other-window icicle-bookmark-specific-files-other-window
                                            (fboundp
                                             (quote bmkp-specific-files-jump)))
     (bmkp-temporary-jump icicle-bookmark-temporary
                          (fboundp
                           (quote bmkp-temporary-jump)))
     (bmkp-temporary-jump-other-window icicle-bookmark-temporary-other-window
                                       (fboundp
                                        (quote bmkp-temporary-jump)))
     (bmkp-this-buffer-jump icicle-bookmark-this-buffer
                            (fboundp
                             (quote bmkp-this-buffer-jump)))
     (bmkp-this-buffer-jump-other-window icicle-bookmark-this-buffer-other-window
                                         (fboundp
                                          (quote bmkp-this-buffer-jump)))
     (bmkp-url-jump icicle-bookmark-url
                    (fboundp
                     (quote bmkp-url-jump)))
     (bmkp-url-jump-other-window icicle-bookmark-url-other-window
                                 (fboundp
                                  (quote bmkp-url-jump)))
     (bmkp-w3m-jump icicle-bookmark-w3m
                    (fboundp
                     (quote bmkp-w3m-jump)))
     (bmkp-w3m-jump-other-window icicle-bookmark-w3m-other-window
                                 (fboundp
                                  (quote bmkp-w3m-jump)))
     (bmkp-find-file-all-tags icicle-find-file-all-tags
                              (fboundp
                               (quote bmkp-find-file-all-tags)))
     (bmkp-find-file-all-tags-other-window icicle-find-file-all-tags-other-window
                                           (fboundp
                                            (quote bmkp-find-file-all-tags)))
     (bmkp-find-file-all-tags-regexp icicle-find-file-all-tags-regexp
                                     (fboundp
                                      (quote bmkp-find-file-all-tags-regexp)))
     (bmkp-find-file-all-tags-regexp-other-window icicle-find-file-all-tags-regexp-other-window
                                                  (fboundp
                                                   (quote bmkp-find-file-all-tags-regexp-other-window)))
     (bmkp-find-file-some-tags icicle-find-file-some-tags
                               (fboundp
                                (quote bmkp-find-file-some-tags)))
     (bmkp-find-file-some-tags-other-window icicle-find-file-some-tags-other-window
                                            (fboundp
                                             (quote bmkp-find-file-some-tags-other-window)))
     (bmkp-find-file-some-tags-regexp icicle-find-file-some-tags-regexp
                                      (fboundp
                                       (quote bmkp-find-file-some-tags-regexp)))
     (bmkp-find-file-some-tags-regexp-other-window icicle-find-file-some-tags-regexp-other-window
                                                   (fboundp
                                                    (quote bmkp-find-file-some-tags-regexp-other-window)))
     (bmkp-autofile-all-tags-jump icicle-bookmark-autofile-all-tags
                                  (fboundp
                                   (quote bmkp-autofile-all-tags-jump)))
     (bmkp-autofile-all-tags-jump-other-window icicle-bookmark-autofile-all-tags-other-window
                                               (fboundp
                                                (quote bmkp-autofile-all-tags-jump)))
     (bmkp-autofile-all-tags-regexp-jump icicle-bookmark-autofile-all-tags-regexp
                                         (fboundp
                                          (quote bmkp-autofile-all-tags-regexp-jump)))
     (bmkp-autofile-all-tags-regexp-jump-other-window icicle-bookmark-autofile-all-tags-regexp-other-window
                                                      (fboundp
                                                       (quote bmkp-autofile-all-tags-regexp-jump)))
     (bmkp-autofile-some-tags-jump icicle-bookmark-autofile-some-tags
                                   (fboundp
                                    (quote bmkp-autofile-some-tags-jump)))
     (bmkp-autofile-some-tags-jump-other-window icicle-bookmark-autofile-some-tags-other-window
                                                (fboundp
                                                 (quote bmkp-autofile-some-tags-jump)))
     (bmkp-autofile-some-tags-regexp-jump icicle-bookmark-autofile-some-tags-regexp
                                          (fboundp
                                           (quote bmkp-autofile-some-tags-regexp-jump)))
     (bmkp-autofile-some-tags-regexp-jump-other-window icicle-bookmark-autofile-some-tags-regexp-other-window
                                                       (fboundp
                                                        (quote bmkp-autofile-some-tags-regexp-jump)))
     (bmkp-all-tags-jump icicle-bookmark-all-tags
                         (fboundp
                          (quote bmkp-all-tags-jump)))
     (bmkp-all-tags-jump-other-window icicle-bookmark-all-tags-other-window
                                      (fboundp
                                       (quote bmkp-all-tags-jump)))
     (bmkp-all-tags-regexp-jump icicle-bookmark-all-tags-regexp
                                (fboundp
                                 (quote bmkp-all-tags-regexp-jump)))
     (bmkp-all-tags-regexp-jump-other-window icicle-bookmark-all-tags-regexp-other-window
                                             (fboundp
                                              (quote bmkp-all-tags-regexp-jump)))
     (bmkp-some-tags-jump icicle-bookmark-some-tags
                          (fboundp
                           (quote bmkp-some-tags-jump)))
     (bmkp-some-tags-jump-other-window icicle-bookmark-some-tags-other-window
                                       (fboundp
                                        (quote bmkp-some-tags-jump)))
     (bmkp-some-tags-regexp-jump icicle-bookmark-some-tags-regexp
                                 (fboundp
                                  (quote bmkp-some-tags-regexp-jump)))
     (bmkp-some-tags-regexp-jump-other-window icicle-bookmark-some-tags-regexp-other-window
                                              (fboundp
                                               (quote bmkp-some-tags-regexp-jump)))
     (bmkp-file-all-tags-jump icicle-bookmark-file-all-tags
                              (fboundp
                               (quote bmkp-file-all-tags-jump)))
     (bmkp-file-all-tags-jump-other-window icicle-bookmark-file-all-tags-other-window
                                           (fboundp
                                            (quote bmkp-file-all-tags-jump)))
     (bmkp-file-all-tags-regexp-jump icicle-bookmark-file-all-tags-regexp
                                     (fboundp
                                      (quote bmkp-file-all-tags-regexp-jump)))
     (bmkp-file-all-tags-regexp-jump-other-window icicle-bookmark-file-all-tags-regexp-other-window
                                                  (fboundp
                                                   (quote bmkp-file-all-tags-regexp-jump)))
     (bmkp-file-some-tags-jump icicle-bookmark-file-some-tags
                               (fboundp
                                (quote bmkp-file-some-tags-jump)))
     (bmkp-file-some-tags-jump-other-window icicle-bookmark-file-some-tags-other-window
                                            (fboundp
                                             (quote bmkp-file-some-tags-jump)))
     (bmkp-file-some-tags-regexp-jump icicle-bookmark-file-some-tags-regexp
                                      (fboundp
                                       (quote bmkp-file-some-tags-regexp-jump)))
     (bmkp-file-some-tags-regexp-jump-other-window icicle-bookmark-file-some-tags-regexp-other-window
                                                   (fboundp
                                                    (quote bmkp-file-some-tags-regexp-jump)))
     (bmkp-file-this-dir-all-tags-jump icicle-bookmark-file-this-dir-all-tags
                                       (fboundp
                                        (quote bmkp-file-this-dir-all-tags-jump)))
     (bmkp-file-this-dir-all-tags-jump-other-window icicle-bookmark-file-this-dir-all-tags-other-window
                                                    (fboundp
                                                     (quote bmkp-file-this-dir-all-tags-jump)))
     (bmkp-file-this-dir-all-tags-regexp-jump icicle-bookmark-file-this-dir-all-tags-regexp
                                              (fboundp
                                               (quote bmkp-file-this-dir-all-tags-regexp-jump)))
     (bmkp-file-this-dir-all-tags-regexp-jump-other-window icicle-bookmark-file-this-dir-all-tags-regexp-other-window
                                                           (fboundp
                                                            (quote bmkp-file-this-dir-all-tags-regexp-jump)))
     (bmkp-file-this-dir-some-tags-jump icicle-bookmark-file-this-dir-some-tags
                                        (fboundp
                                         (quote bmkp-file-this-dir-some-tags-jump)))
     (bmkp-file-this-dir-some-tags-jump-other-window icicle-bookmark-file-this-dir-some-tags-other-window
                                                     (fboundp
                                                      (quote bmkp-file-this-dir-some-tags-jump)))
     (bmkp-file-this-dir-some-tags-regexp-jump icicle-bookmark-file-this-dir-some-tags-regexp
                                               (fboundp
                                                (quote bmkp-file-this-dir-some-tags-regexp-jump)))
     (bmkp-file-this-dir-some-tags-regexp-jump-other-window icicle-bookmark-file-this-dir-some-tags-regexp-other-window
                                                            (fboundp
                                                             (quote bmkp-file-this-dir-some-tags-regexp-jump)))
     (find-tag icicle-find-tag
               (fboundp
                (quote command-remapping)))
     (find-tag-other-window icicle-find-first-tag-other-window t)
     (pop-tag-mark icicle-pop-tag-mark
                   (fboundp
                    (quote command-remapping)))
     (eval-expression icicle-pp-eval-expression
                      (fboundp
                       (quote command-remapping)))
     (pp-eval-expression icicle-pp-eval-expression
                         (fboundp
                          (quote command-remapping)))
     ([27 134217848]
      lacarte-execute-command
      (fboundp
       (quote lacarte-execute-command)))
     ([134217824]
      lacarte-execute-menu-command
      (fboundp
       (quote lacarte-execute-menu-command)))
     ([f10]
      lacarte-execute-menu-command
      (fboundp
       (quote lacarte-execute-menu-command))))))
 '(indent-tabs-mode nil)
 '(nil nil t)
 '(savehist-mode t)
 '(uniquify-buffer-name-style (quote post-forward-angle-brackets) nil (uniquify))
 '(x-select-enable-clipboard t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :background "white" :foreground "black" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 95 :width normal :foundry "unknown" :family "DejaVu Sans Mono"))))
 '(flymake-errline ((((class color) (background light)) (:underline "darkred"))))
 '(flymake-warnline ((((class color) (background light)) (:underline "blue"))))
 '(mumamo-background-chunk-major ((((class color) (min-colors 88) (background light)) (:background "white"))) t)
 '(web-mode-html-attr-name-face ((t (:inherit font-lock-variable-name-face))))
 '(web-mode-html-tag-bracket-face ((t (:inherit font-lock-comment-face))))
 '(web-mode-html-tag-face ((t (:inherit font-lock-function-name-face :weight bold)))))
