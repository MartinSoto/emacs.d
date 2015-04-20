;; -*- emacs-lisp -*-

;; Useful elisp functions
;; TODO: Move to a separate file.

(defun camelcase-to-constant (s)
  (upcase (let ((case-fold-search nil))
            (replace-regexp-in-string "\\([a-z]\\)\\([A-Z]\\)" "\\1_\\2" s))))


;; Personal elisp directory.
(add-to-list 'load-path "~/.emacs.d/lisp/")

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

;; Display transient windows as "popups".
(require 'popwin)
(popwin-mode 1)

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

;; ido mode: Magical, minibuffer-based file and buffer selection.
;(require 'ido)
;(ido-mode t)

;; icicles:.
(require 'icicles)
(icy-mode 1)

;; YASnippet.
(add-to-list 'load-path
              "~/.emacs.d/lisp/yasnippet")
(require 'yasnippet)
(yas-global-mode 1)
;; Since Auto-complete calls Yasnippet expansion, don't bind TAB to
;; the Yasnippet expansion function directly.
(define-key yas-minor-mode-map [(tab)] nil)
(define-key yas-minor-mode-map (kbd "TAB") nil)

;; Autocomplete mode.
(add-to-list 'load-path "~/.emacs.d/lisp/auto-complete")
(add-to-list 'load-path "~/.emacs.d/lisp/auto-complete/lib/popup")
; Load the default configuration.
(require 'auto-complete-config)
; Make sure we can find the dictionaries.
(add-to-list 'ac-dictionary-directories "~/.emacs.d/lisp/auto-complete/dict")
(ac-config-default)
; Additional auto-completion sources (for all modes).
(setq-default ac-sources
              (append '(ac-source-dictionary ac-source-yasnippet)
                      ac-sources))
; Case sensitivity is important when finding matches.
(setq ac-ignore-case nil)

;; Fix Auto-complete's Yasnippet binding so that it works with
;; Yasnippet 0.8. Auto-complete 1.4 should fix this problem, so this
;; hack can be removed after upgrading.
(defun ac-yasnippet-candidates ()
  (with-no-warnings
(cond (;; 0.8 onwards
       (fboundp 'yas-active-keys)
       (all-completions ac-prefix (yas-active-keys)))
      (;; >0.6.0
       (fboundp 'yas/get-snippet-tables)
       (apply 'append (mapcar 'ac-yasnippet-candidate-1
                  (condition-case nil
                  (yas/get-snippet-tables major-mode)
                (wrong-number-of-arguments
                 (yas/get-snippet-tables)))))
       )
      (t
       (let ((table
          (if (fboundp 'yas/snippet-table)
          ;; <0.6.0
          (yas/snippet-table major-mode)
        ;; 0.6.0
        (yas/current-snippet-table))))
     (if table
         (ac-yasnippet-candidate-1 table)))))))

;; cython-mode.el.
(require 'cython-mode)

;; Add our emacs Python directory to PYTHONPATH. It contains the
;; Pymacs and rope packages.
(setenv "PYTHONPATH"
        (let ((python-path (getenv "PYTHONPATH"))
              (local-path (concat (getenv "HOME") "/.emacs.d/python")))
          (if python-path
              (concat local-path path-separator python-path)
            local-path)))

;; Pymacs.
(autoload 'pymacs-apply "pymacs")
(autoload 'pymacs-call "pymacs")
(autoload 'pymacs-eval "pymacs" nil t)
(autoload 'pymacs-exec "pymacs" nil t)
(autoload 'pymacs-load "pymacs" nil t)
(autoload 'pymacs-autoload "pymacs")

;; Ropemacs.
(pymacs-load "ropemacs" "rope-")
; Use rope as completion source for Python.
(ac-ropemacs-initialize)
(add-hook 'python-mode-hook
          (lambda ()
	    (add-to-list 'ac-sources 'ac-source-ropemacs)))

;; js2-mode for JavaScript (and React´s JSX)
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
(add-to-list 'auto-mode-alist '("\\.jsx\\'" . js2-mode))

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

;; CoffeeScript mode.
(require 'coffee-mode)
(add-hook 'coffee-mode-hook
  '(lambda() (set (make-local-variable 'tab-width) 4)))

;; Flymake:

;; Flymake for JavaScript (based on https://github.com/jegbjerg/flymake-node-jshint/blob/master/flymake-node-jshint.el).
(when (load "flymake" t)
  (defun flymake-jshint-init ()
    (let* ((temp-file (flymake-init-create-temp-buffer-copy
                       'flymake-create-temp-inplace))
           (local-file (file-relative-name
                        temp-file
                        (file-name-directory buffer-file-name))))
      (list "~/.emacs.d/node_modules/.bin/jsxhint" (list "--jsx-only" local-file))))

  (setq flymake-err-line-patterns
        (cons '(".*: line \\([[:digit:]]+\\), col \\([[:digit:]]+\\), \\(.*\\)$"
                nil 1 2 3)
              flymake-err-line-patterns))

  (add-to-list 'flymake-allowed-file-name-masks
               '("\\.js\\'" flymake-jshint-init))
  (add-to-list 'flymake-allowed-file-name-masks
               '("\\.jsx\\'" flymake-jshint-init))
  (add-to-list 'flymake-allowed-file-name-masks
               '("\\.json\\'" flymake-jshint-init))

  (require 'flymake-cursor)
)

(add-hook 'js2-mode-hook
	  (lambda ()
            (flymake-mode 1)
            (define-key js-mode-map "\C-c\C-n" 'flymake-goto-next-error)))

;; Markdown mode.
(autoload 'markdown-mode "markdown-mode"
   "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.text\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))

;; ;; Docstring edit
;; (load "/home/soto/Desktop/Projects/rdf-devel/docedit/docedit.el")

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
 '(desktop-path (quote (".")))
 '(desktop-restore-eager 5)
 '(desktop-save-mode 1)
 '(global-auto-revert-mode t)
 '(global-whitespace-mode nil)
 '(icicle-TAB/S-TAB-only-completes-flag t)
 '(icicle-top-level-key-bindings (quote (([pause] icicle-switch-to/from-minibuffer t) ("`" icicle-search-generic t) ("$" icicle-search-word t) ("^" icicle-search-keywords t) ("'" icicle-occur t) ("=" icicle-imenu t) ("\"" icicle-search-text-property t) ("/" icicle-complete-thesaurus-entry (fboundp (quote icicle-complete-thesaurus-entry))) ([24 134217829] icicle-execute-named-keyboard-macro t) (" " icicle-command-abbrev t) ("5o" icicle-select-frame t) ("" icicle-describe-option-of-type t) ([S-f4] icicle-kmacro t) (abort-recursive-edit icicle-abort-recursive-edit t) (apropos icicle-apropos t) (apropos-command icicle-apropos-command t) (apropos-value icicle-apropos-value t) (apropos-variable icicle-apropos-option (fboundp (quote icicle-apropos-option))) (apropos-variable icicle-apropos-variable (not (fboundp (quote icicle-apropos-option)))) (apropos-zippy icicle-apropos-zippy t) (bookmark-jump icicle-bookmark t) (bookmark-jump-other-window icicle-bookmark-other-window t) (bookmark-set icicle-bookmark-cmd t) (customize-apropos icicle-customize-apropos t) (customize-apropos-faces icicle-customize-apropos-faces t) (customize-apropos-groups icicle-customize-apropos-groups t) (customize-apropos-options icicle-customize-apropos-options t) (customize-face icicle-customize-face t) (customize-face-other-window icicle-customize-face-other-window t) (dabbrev-completion icicle-dabbrev-completion (< emacs-major-version 24)) (delete-window icicle-delete-window t) (delete-windows-for icicle-delete-window t) (dired icicle-dired t) (dired-other-window icicle-dired-other-window t) (exchange-point-and-mark icicle-exchange-point-and-mark t) (find-file icicle-file t) (find-file-other-window icicle-file-other-window t) (find-file-read-only icicle-find-file-read-only t) (find-file-read-only-other-window icicle-find-file-read-only-other-window t) (insert-buffer icicle-insert-buffer t) (kill-buffer icicle-kill-buffer t) (kill-buffer-and-its-windows icicle-kill-buffer t) (minibuffer-keyboard-quit icicle-abort-recursive-edit (fboundp (quote minibuffer-keyboard-quit))) (other-window icicle-other-window-or-frame t) (other-window-or-frame icicle-other-window-or-frame t) (pop-global-mark icicle-goto-global-marker-or-pop-global-mark t) (repeat-complex-command icicle-repeat-complex-command t) (set-mark-command icicle-goto-marker-or-set-mark-command t) (switch-to-buffer icicle-buffer t) (switch-to-buffer-other-window icicle-buffer-other-window t) (where-is icicle-where-is t) (yank icicle-yank-maybe-completing t) (yank-pop icicle-yank-pop-commands (featurep (quote second-sel))) (yank-pop-commands icicle-yank-pop-commands (featurep (quote second-sel))) (zap-to-char icicle-zap-to-char (fboundp (quote read-char-by-name))) ("jt" icicle-find-file-tagged (featurep (quote bookmark+))) ("4jt" icicle-find-file-tagged-other-window (featurep (quote bookmark+))) (bmkp-autofile-set icicle-bookmark-a-file (fboundp (quote bmkp-bookmark-a-file))) (bmkp-tag-a-file icicle-tag-a-file (fboundp (quote bmkp-tag-a-file))) (bmkp-untag-a-file icicle-untag-a-file (fboundp (quote bmkp-untag-a-file))) (bmkp-find-file icicle-find-file-handle-bookmark (fboundp (quote bmkp-find-file))) (bmkp-find-file-other-window icicle-find-file-handle-bookmark-other-window (fboundp (quote bmkp-find-file-other-window))) (bmkp-autofile-jump icicle-bookmark-autofile (fboundp (quote bmkp-autofile-jump))) (bmkp-autofile-jump-other-window icicle-bookmark-autofile-other-window (fboundp (quote bmkp-autofile-jump))) (bmkp-autonamed-jump icicle-bookmark-autonamed (fboundp (quote bmkp-autonamed-jump))) (bmkp-autonamed-jump-other-window icicle-bookmark-autonamed-other-window (fboundp (quote bmkp-autonamed-jump))) (bmkp-autonamed-this-buffer-jump icicle-bookmark-autonamed-this-buffer (fboundp (quote bmkp-autonamed-this-buffer-jump))) (bmkp-bookmark-file-jump icicle-bookmark-bookmark-file (fboundp (quote bmkp-bookmark-file-jump))) (bmkp-bookmark-list-jump icicle-bookmark-bookmark-list (fboundp (quote bmkp-bookmark-list-jump))) (bmkp-desktop-jump icicle-bookmark-desktop (fboundp (quote bmkp-desktop-jump))) (bmkp-dired-jump icicle-bookmark-dired (fboundp (quote bmkp-dired-jump))) (bmkp-dired-jump-other-window icicle-bookmark-dired-other-window (fboundp (quote bmkp-dired-jump))) (bmkp-file-jump icicle-bookmark-file (fboundp (quote bmkp-file-jump))) (bmkp-file-jump-other-window icicle-bookmark-file-other-window (fboundp (quote bmkp-file-jump))) (bmkp-file-this-dir-jump icicle-bookmark-file-this-dir (fboundp (quote bmkp-file-this-dir-jump))) (bmkp-file-this-dir-jump-other-window icicle-bookmark-file-this-dir-other-window (fboundp (quote bmkp-file-this-dir-jump))) (bmkp-gnus-jump icicle-bookmark-gnus (fboundp (quote bmkp-gnus-jump))) (bmkp-gnus-jump-other-window icicle-bookmark-gnus-other-window (fboundp (quote bmkp-gnus-jump))) (bmkp-image-jump icicle-bookmark-image (fboundp (quote bmkp-image-jump))) (bmkp-image-jump-other-window icicle-bookmark-image-other-window (fboundp (quote bmkp-image-jump))) (bmkp-info-jump icicle-bookmark-info (fboundp (quote bmkp-info-jump))) (bmkp-info-jump-other-window icicle-bookmark-info-other-window (fboundp (quote bmkp-info-jump))) (bmkp-local-file-jump icicle-bookmark-local-file (fboundp (quote bmkp-local-file-jump))) (bmkp-local-file-jump-other-window icicle-bookmark-local-file-other-window (fboundp (quote bmkp-local-file-jump))) (bmkp-man-jump icicle-bookmark-man (fboundp (quote bmkp-man-jump))) (bmkp-man-jump-other-window icicle-bookmark-man-other-window (fboundp (quote bmkp-man-jump))) (bmkp-non-file-jump icicle-bookmark-non-file (fboundp (quote bmkp-non-file-jump))) (bmkp-non-file-jump-other-window icicle-bookmark-non-file-other-window (fboundp (quote bmkp-non-file-jump))) (bmkp-region-jump icicle-bookmark-region (fboundp (quote bmkp-region-jump))) (bmkp-region-jump-other-window icicle-bookmark-region-other-window (fboundp (quote bmkp-region-jump))) (bmkp-remote-file-jump icicle-bookmark-remote-file (fboundp (quote bmkp-remote-file-jump))) (bmkp-remote-file-jump-other-window icicle-bookmark-remote-file-other-window (fboundp (quote bmkp-remote-file-jump))) (bmkp-specific-buffers-jump icicle-bookmark-specific-buffers (fboundp (quote bmkp-specific-buffers-jump))) (bmkp-specific-buffers-jump-other-window icicle-bookmark-specific-buffers-other-window (fboundp (quote bmkp-specific-buffers-jump))) (bmkp-specific-files-jump icicle-bookmark-specific-files (fboundp (quote bmkp-specific-files-jump))) (bmkp-specific-files-jump-other-window icicle-bookmark-specific-files-other-window (fboundp (quote bmkp-specific-files-jump))) (bmkp-temporary-jump icicle-bookmark-temporary (fboundp (quote bmkp-temporary-jump))) (bmkp-temporary-jump-other-window icicle-bookmark-temporary-other-window (fboundp (quote bmkp-temporary-jump))) (bmkp-this-buffer-jump icicle-bookmark-this-buffer (fboundp (quote bmkp-this-buffer-jump))) (bmkp-this-buffer-jump-other-window icicle-bookmark-this-buffer-other-window (fboundp (quote bmkp-this-buffer-jump))) (bmkp-url-jump icicle-bookmark-url (fboundp (quote bmkp-url-jump))) (bmkp-url-jump-other-window icicle-bookmark-url-other-window (fboundp (quote bmkp-url-jump))) (bmkp-w3m-jump icicle-bookmark-w3m (fboundp (quote bmkp-w3m-jump))) (bmkp-w3m-jump-other-window icicle-bookmark-w3m-other-window (fboundp (quote bmkp-w3m-jump))) (bmkp-find-file-all-tags icicle-find-file-all-tags (fboundp (quote bmkp-find-file-all-tags))) (bmkp-find-file-all-tags-other-window icicle-find-file-all-tags-other-window (fboundp (quote bmkp-find-file-all-tags))) (bmkp-find-file-all-tags-regexp icicle-find-file-all-tags-regexp (fboundp (quote bmkp-find-file-all-tags-regexp))) (bmkp-find-file-all-tags-regexp-other-window icicle-find-file-all-tags-regexp-other-window (fboundp (quote bmkp-find-file-all-tags-regexp-other-window))) (bmkp-find-file-some-tags icicle-find-file-some-tags (fboundp (quote bmkp-find-file-some-tags))) (bmkp-find-file-some-tags-other-window icicle-find-file-some-tags-other-window (fboundp (quote bmkp-find-file-some-tags-other-window))) (bmkp-find-file-some-tags-regexp icicle-find-file-some-tags-regexp (fboundp (quote bmkp-find-file-some-tags-regexp))) (bmkp-find-file-some-tags-regexp-other-window icicle-find-file-some-tags-regexp-other-window (fboundp (quote bmkp-find-file-some-tags-regexp-other-window))) (bmkp-autofile-all-tags-jump icicle-bookmark-autofile-all-tags (fboundp (quote bmkp-autofile-all-tags-jump))) (bmkp-autofile-all-tags-jump-other-window icicle-bookmark-autofile-all-tags-other-window (fboundp (quote bmkp-autofile-all-tags-jump))) (bmkp-autofile-all-tags-regexp-jump icicle-bookmark-autofile-all-tags-regexp (fboundp (quote bmkp-autofile-all-tags-regexp-jump))) (bmkp-autofile-all-tags-regexp-jump-other-window icicle-bookmark-autofile-all-tags-regexp-other-window (fboundp (quote bmkp-autofile-all-tags-regexp-jump))) (bmkp-autofile-some-tags-jump icicle-bookmark-autofile-some-tags (fboundp (quote bmkp-autofile-some-tags-jump))) (bmkp-autofile-some-tags-jump-other-window icicle-bookmark-autofile-some-tags-other-window (fboundp (quote bmkp-autofile-some-tags-jump))) (bmkp-autofile-some-tags-regexp-jump icicle-bookmark-autofile-some-tags-regexp (fboundp (quote bmkp-autofile-some-tags-regexp-jump))) (bmkp-autofile-some-tags-regexp-jump-other-window icicle-bookmark-autofile-some-tags-regexp-other-window (fboundp (quote bmkp-autofile-some-tags-regexp-jump))) (bmkp-all-tags-jump icicle-bookmark-all-tags (fboundp (quote bmkp-all-tags-jump))) (bmkp-all-tags-jump-other-window icicle-bookmark-all-tags-other-window (fboundp (quote bmkp-all-tags-jump))) (bmkp-all-tags-regexp-jump icicle-bookmark-all-tags-regexp (fboundp (quote bmkp-all-tags-regexp-jump))) (bmkp-all-tags-regexp-jump-other-window icicle-bookmark-all-tags-regexp-other-window (fboundp (quote bmkp-all-tags-regexp-jump))) (bmkp-some-tags-jump icicle-bookmark-some-tags (fboundp (quote bmkp-some-tags-jump))) (bmkp-some-tags-jump-other-window icicle-bookmark-some-tags-other-window (fboundp (quote bmkp-some-tags-jump))) (bmkp-some-tags-regexp-jump icicle-bookmark-some-tags-regexp (fboundp (quote bmkp-some-tags-regexp-jump))) (bmkp-some-tags-regexp-jump-other-window icicle-bookmark-some-tags-regexp-other-window (fboundp (quote bmkp-some-tags-regexp-jump))) (bmkp-file-all-tags-jump icicle-bookmark-file-all-tags (fboundp (quote bmkp-file-all-tags-jump))) (bmkp-file-all-tags-jump-other-window icicle-bookmark-file-all-tags-other-window (fboundp (quote bmkp-file-all-tags-jump))) (bmkp-file-all-tags-regexp-jump icicle-bookmark-file-all-tags-regexp (fboundp (quote bmkp-file-all-tags-regexp-jump))) (bmkp-file-all-tags-regexp-jump-other-window icicle-bookmark-file-all-tags-regexp-other-window (fboundp (quote bmkp-file-all-tags-regexp-jump))) (bmkp-file-some-tags-jump icicle-bookmark-file-some-tags (fboundp (quote bmkp-file-some-tags-jump))) (bmkp-file-some-tags-jump-other-window icicle-bookmark-file-some-tags-other-window (fboundp (quote bmkp-file-some-tags-jump))) (bmkp-file-some-tags-regexp-jump icicle-bookmark-file-some-tags-regexp (fboundp (quote bmkp-file-some-tags-regexp-jump))) (bmkp-file-some-tags-regexp-jump-other-window icicle-bookmark-file-some-tags-regexp-other-window (fboundp (quote bmkp-file-some-tags-regexp-jump))) (bmkp-file-this-dir-all-tags-jump icicle-bookmark-file-this-dir-all-tags (fboundp (quote bmkp-file-this-dir-all-tags-jump))) (bmkp-file-this-dir-all-tags-jump-other-window icicle-bookmark-file-this-dir-all-tags-other-window (fboundp (quote bmkp-file-this-dir-all-tags-jump))) (bmkp-file-this-dir-all-tags-regexp-jump icicle-bookmark-file-this-dir-all-tags-regexp (fboundp (quote bmkp-file-this-dir-all-tags-regexp-jump))) (bmkp-file-this-dir-all-tags-regexp-jump-other-window icicle-bookmark-file-this-dir-all-tags-regexp-other-window (fboundp (quote bmkp-file-this-dir-all-tags-regexp-jump))) (bmkp-file-this-dir-some-tags-jump icicle-bookmark-file-this-dir-some-tags (fboundp (quote bmkp-file-this-dir-some-tags-jump))) (bmkp-file-this-dir-some-tags-jump-other-window icicle-bookmark-file-this-dir-some-tags-other-window (fboundp (quote bmkp-file-this-dir-some-tags-jump))) (bmkp-file-this-dir-some-tags-regexp-jump icicle-bookmark-file-this-dir-some-tags-regexp (fboundp (quote bmkp-file-this-dir-some-tags-regexp-jump))) (bmkp-file-this-dir-some-tags-regexp-jump-other-window icicle-bookmark-file-this-dir-some-tags-regexp-other-window (fboundp (quote bmkp-file-this-dir-some-tags-regexp-jump))) (find-tag icicle-find-tag (fboundp (quote command-remapping))) (find-tag-other-window icicle-find-first-tag-other-window t) (pop-tag-mark icicle-pop-tag-mark (fboundp (quote command-remapping))) (eval-expression icicle-pp-eval-expression (fboundp (quote command-remapping))) (pp-eval-expression icicle-pp-eval-expression (fboundp (quote command-remapping))) ([27 134217848] lacarte-execute-command (fboundp (quote lacarte-execute-command))) ([134217824] lacarte-execute-menu-command (fboundp (quote lacarte-execute-menu-command))) ([f10] lacarte-execute-menu-command (fboundp (quote lacarte-execute-menu-command))))))
 '(indent-tabs-mode nil)
 '(nil nil t)
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
 '(mumamo-background-chunk-major ((((class color) (min-colors 88) (background light)) (:background "white")))))
