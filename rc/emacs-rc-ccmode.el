(require 'cc-mode)

;(load "c-eldoc")
;(setq c-eldoc-includes "-I~/exp/include -I./ -I../ ")

;; ;; customisation of cc-mode
;; (defun alexott/c-mode-common-hook ()
;;   ;; style customization
;;   (c-set-offset 'member-init-intro '++)
;;   (setq tab-width 4)
;;   (setq   indent-tabs-mode t)
;;   (c-set-offset 'substatement-open 0)
;;   (c-set-style "bsd")
;;   (setq c-basic-offset 4)
;;   (c-toggle-auto-hungry-state 0)
;;   ;; minor modes
;;   (auto-fill-mode 1)
;;   (c-turn-on-eldoc-mode)
;;   (gtags-mode 1)
;;   (hs-minor-mode 1)
;;   ;; local keys
;;   (local-set-key [return] 'newline-and-indent)
;;   ;;        (local-set-key [delete]  'delete-char)
;;   )
;; (add-hook 'c-mode-common-hook 'alexott/c-mode-common-hook)
;; (add-hook 'c-mode-common-hook 'alexott/common-hook)
;; (add-hook 'c-mode-common-hook 'alexott/show-prog-keywords)

(setq-default tab-width 4) ; or any other preferred value
    (setq cua-auto-tabify-rectangles nil)
    (defadvice align (around smart-tabs activate)
      (let ((indent-tabs-mode nil)) ad-do-it))
    (defadvice align-regexp (around smart-tabs activate)
      (let ((indent-tabs-mode nil)) ad-do-it))
    (defadvice indent-relative (around smart-tabs activate)
      (let ((indent-tabs-mode nil)) ad-do-it))
    (defadvice indent-according-to-mode (around smart-tabs activate)
      (let ((indent-tabs-mode indent-tabs-mode))
        (if (memq indent-line-function
                  '(indent-relative
                    indent-relative-maybe))
            (setq indent-tabs-mode nil))
        ad-do-it))
    (defmacro smart-tabs-advice (function offset)
      (defvaralias offset 'tab-width)
      `(defadvice ,function (around smart-tabs activate)
         (cond
          (indent-tabs-mode
           (save-excursion
             (beginning-of-line)
             (while (looking-at "\t*\\( +\\)\t+")
               (replace-match "" nil nil nil 1)))
           (setq tab-width tab-width)
           (let ((tab-width fill-column)
                 (,offset fill-column))
             ad-do-it))
          (t
           ad-do-it))))
    (smart-tabs-advice c-indent-line c-basic-offset)
    (smart-tabs-advice c-indent-region c-basic-offset)

(require 'info-look)
(info-lookup-add-help
 :mode 'c-mode
 :regexp "[^][()'\" \t\n]+"
 :ignore-case t
 :doc-spec '(("(libc)Symbol Index" nil nil nil)))

(defun fp-c-mode-routine ()
  (local-set-key "\M-q" 'rebox-comment))
(add-hook 'c-mode-hook 'fp-c-mode-routine)