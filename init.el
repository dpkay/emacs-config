;; paths
(add-to-list 'load-path "~/.emacs.d")
(progn (cd "~/.emacs.d") (normal-top-level-add-subdirs-to-load-path))

;; python
(autoload 'python-mode "python-mode" "Python Mode." t)
(add-to-list 'auto-mode-alist '("\\.py\\'" . python-mode))
(add-to-list 'interpreter-mode-alist '("python" . python-mode))

;; color theme
(require 'color-theme)
(load "color-theme-tango.el")
(color-theme-initialize)
(color-theme-tango)

;; keyboard shortcuts
;;(global-set-key [f4] 'goto-line)
(global-set-key [f12] 'toggle-truncate-lines)
(global-unset-key "\C-z")
(global-set-key "\C-z" 'advertised-undo)

;; buffer switching
(require 'bs)
(global-set-key "\C-x\C-b" 'bs-show)

;; recent files
(require 'recentf)
(recentf-mode 1)

;; saving desktop
(setq desktop-save 'if-exists)
(desktop-save-mode 1)

;; saving layouts
;;(require 'layout-restore)
;;%(global-set-key [?\C-c ?l] 'layout-save-current)
;;%(global-set-key [?\C-c ?\C-l ?\C-l] 'layout-restore)
;;(global-set-key [?\C-c ?\C-l ?\C-c] 'layout-delete-current)

;; undo browsing
(load "undo-browse.el")

; shortening of often used commands
(defalias 'gf 'grep-find)
(defalias 'fd 'find-dired)
(defalias 'sh 'shell)

(defalias 'qrr 'query-replace-regexp)
(defalias 'lml 'list-matching-lines)
(defalias 'dml 'delete-matching-lines)
(defalias 'rof 'recentf-open-files)


(defalias 'eb 'eval-buffer)
(defalias 'er 'eval-region)
(defalias 'ee 'eval-expression)

(defalias 'sv 'set-variable)
(defalias 'dv 'describe-variable)

(defalias 'gl 'goto-line)


; yesorno
(defalias 'yes-or-no-p 'y-or-n-p)

; thesaurus
(load "synonyms/synonyms.el")
(setq synonyms-file "~/.emacs.d/synonyms/mthesaur.txt")
(setq synonyms-cache-file "~/.emacs.d/synonyms/cache.txt")
(require 'synonyms)

;; startup

; auctex
(setq TeX-auto-save t)
(setq TeX-parse-self t)
(setq-default TeX-master nil)
(setq-default TeX-PDF-mode nil)
(add-hook 'LaTeX-mode-hook 'auto-fill-mode)
(add-hook 'LaTeX-mode-hook 'flyspell-mode)
(add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)
(add-hook 'LaTeX-mode-hook 'turn-on-reftex)
(setq reftex-plug-into-AUCTeX t)

;; 
(global-set-key [C-up] (lambda () (interactive) (scroll-down 1)))
(global-set-key [C-down] (lambda () (interactive) (scroll-up 1)))
;;(global-set-key [C-left] (lambda () (interactive) (scroll-right tab-width t)))
;;(global-set-key [C-right] (lambda () (interactive) (scroll-left tab-width t)))


(setq custom-file "~/.emacs.d/custom.el")
(load custom-file 'noerror)

(windmove-default-keybindings 'meta)

(tool-bar-mode 0)

; ido
(ido-mode t)
(setq ido-enable-flex-matching t) ; fuzzy matching is a must have


; unfill
 (defun unfill-paragraph ()
      "Takes a multi-line paragraph and makes it into a single line of text."
      (interactive)
      (let ((fill-column (point-max)))
        (fill-paragraph nil)))
(global-set-key [C-M-q] 'unfill-paragraph)

; cedet
(load "~/.emacs.d/rc/emacs-rc-cedet.el")

; c
(load "~/.emacs.d/rc/emacs-rc-ccmode.el")
