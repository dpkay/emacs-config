(load-file "~/.emacs.d/cedet-1.0pre7/common/cedet.el")

; initialize
(semantic-load-enable-gaudy-code-helpers)

; ede
(require 'ede)
(require 'semantic-lex-spp)
(global-ede-mode 1)

; senator
(setq senator-minor-mode-name "SN")
(global-semantic-mru-bookmark-mode 1)

; srecode
(global-srecode-minor-mode 1)

; others
(require 'semantic-decorate-include)
(require 'semantic-ia)

; eassist
(require 'eassist)
(setq-mode-local c-mode semanticdb-find-default-throttle
                 '(project unloaded system recursive))
(setq-mode-local c++-mode semanticdb-find-default-throttle
                 '(project unloaded system recursive))


;; gnu global support
;(require 'semanticdb-global)
;(semanticdb-enable-gnu-global-databases 'c-mode)
;(semanticdb-enable-gnu-global-databases 'c++-mode)

; ctags
(require 'semanticdb-ectag)
(semantic-load-enable-primary-exuberent-ctags-support)

; mode customization
(defun alexott/cedet-hook ()
  (local-set-key [(control return)] 'semantic-ia-complete-symbol-menu)
  (local-set-key "\C-c?" 'semantic-ia-complete-symbol)
  ;;
  (local-set-key "\C-c>" 'semantic-complete-analyze-inline)
  (local-set-key "\C-c=" 'semantic-decoration-include-visit)

  (local-set-key "\C-cj" 'semantic-ia-fast-jump)
  (local-set-key "\C-cq" 'semantic-ia-show-doc)
  (local-set-key "\C-cs" 'semantic-ia-show-summary)
  (local-set-key "\C-cp" 'semantic-analyze-proto-impl-toggle)
  )

(add-hook 'c-mode-common-hook 'alexott/cedet-hook)
(add-hook 'lisp-mode-hook 'alexott/cedet-hook)
(add-hook 'scheme-mode-hook 'alexott/cedet-hook)
(add-hook 'emacs-lisp-mode-hook 'alexott/cedet-hook)
(add-hook 'erlang-mode-hook 'alexott/cedet-hook)

(defun alexott/c-mode-cedet-hook ()
 ;; (local-set-key "." 'semantic-complete-self-insert)
 ;; (local-set-key ">" 'semantic-complete-self-insert)
  (local-set-key "\C-ct" 'eassist-switch-h-cpp)
  (local-set-key "\C-xt" 'eassist-switch-h-cpp)
  (local-set-key "\C-ce" 'eassist-list-methods)
  (local-set-key "\C-c\C-r" 'semantic-symref)
  )
(add-hook 'c-mode-common-hook 'alexott/c-mode-cedet-hook)

(defun alexott/semantic-hook ()
;; (semantic-tag-folding-mode 1)
  (imenu-add-to-menubar "TAGS")
 )
(add-hook 'semantic-init-hooks 'alexott/semantic-hook)

;; projects
(setq cpp-tests-project
      (ede-cpp-root-project "fatfinger"
                            :file "h:/ethz/mt/mt_fatfinger/live/main.cpp"
      )
)
;                            :system-include-path '("/home/ott/exp/include"
;                                                   boost-base-directory)
;                            :local-variables (list
;                                              (cons 'compile-command 'alexott/gen-cma;ke-debug-compile-string)
;                                              )
;                            )
;      )
