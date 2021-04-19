(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'evil)
(straight-use-package 'evil-leader)
(straight-use-package 'which-key)
(straight-use-package 'sr-speedbar)
(straight-use-package 'helm)
(straight-use-package 'browse-kill-ring)
(straight-use-package 'hide-mode-line)
(straight-use-package 'ivy)
(straight-use-package 'company)
(straight-use-package 'evil-collection)
(straight-use-package 'ggtags)
(straight-use-package 'projectile)
(straight-use-package 'counsel-projectile)
(straight-use-package 'flycheck)
(straight-use-package 'lsp-mode)
(straight-use-package 'exec-path-from-shell)
(straight-use-package 'slime)
(straight-use-package 'haskell-mode)
(straight-use-package 'paredit)
(straight-use-package 'elcord)
(straight-use-package 'evil-cleverparens)

(add-to-list 'default-frame-alist '(font . "Roboto Mono"))
(set-face-attribute 'default t :font "Roboto Mono")
(set-face-attribute 'default nil :font "Roboto Mono")
(set-frame-font "Roboto Mono" nil t)

(setq evil-want-keybinding nil)

;; Setup packages
(require 'evil)
(require 'which-key)
(require 'ivy)
(require 'company)
(require 'flycheck)

;; evil
(evil-set-leader 'normal (kbd "SPC"))
(defvar leader-map (make-sparse-keymap))
(defvar buffer-subkeymap (make-sparse-keymap))
(defvar file-subkeymap (make-sparse-keymap))
(defvar window-subkeymap (make-sparse-keymap))
(defvar lisp-subkeymap (make-sparse-keymap))
(defvar lisp-eval-subkeymap (make-sparse-keymap))
(evil-define-key 'normal 'global (kbd "SPC") leader-map)
(define-key leader-map (kbd "b") buffer-subkeymap)
(define-key leader-map (kbd "f") file-subkeymap)
(define-key leader-map (kbd "w") window-subkeymap)
(define-key leader-map (kbd "l") lisp-subkeymap)


;; "SPC b" Buffer
(define-key buffer-subkeymap "b" 'list-buffers)
(define-key buffer-subkeymap "e" 'eval-buffer)
(define-key buffer-subkeymap "l" 'evil-switch-to-windows-last-buffer)
(define-key buffer-subkeymap "k" 'kill-buffer)

;; "SPC f" File
(define-key file-subkeymap "f" 'find-file)

;; "SPC w" Window
(define-key window-subkeymap "s" 'split-window-below)
(define-key window-subkeymap "v" 'split-window-right)
(define-key window-subkeymap "j" 'windmove-down)
(define-key window-subkeymap "k" 'windmove-up)
(define-key window-subkeymap "h" 'windmove-left)
(define-key window-subkeymap "l" 'windmove-right)
(define-key window-subkeymap "d" 'delete-window)

;; "SPC l" Lisp
(define-key lisp-subkeymap (kbd "e") lisp-eval-subkeymap)
(define-key lisp-subkeymap (kbd "s") 'slime)
;; "SPC l e" Lisp-Eval
(define-key lisp-eval-subkeymap (kbd "b") 'slime-eval-buffer)
(define-key lisp-eval-subkeymap (kbd "r") 'slime-eval-region)
(define-key lisp-eval-subkeymap (kbd "e") 'slime-eval-last-expression)

;; which-key
(which-key-setup-side-window-right)
(which-key-add-key-based-replacements
    "<SPC> b" "Buffer"
    "<SPC> e" "Eval"
    "<SPC> f" "File"
    "<SPC> p" "Project"
    "<SPC> w" "Window"
    "<SPC> l" "Lisp"
    "<SPC> l e" "Lisp-Eval"
    )

;; sr-speedbar
(setq speedbar-use-images nil)

;; company
(add-hook 'after-init-hook 'global-company-mode)

;; slime
(setq inferior-lisp-program "sbcl")
(add-hook 'common-lisp-lisp-mode-hook
	  (lambda ()
	    (slime)
	    (slime-mode)
	    ))
(add-hook 'slime-mode-hook
	  (lambda ()
	    ))
(add-to-list 'auto-mode-alist
	     '("\\.cl\\'" . common-lisp-mode)
	     )

;; smartparens
(smartparens-global-mode)
(setq sp-highlight-pair-overlay nil)
(setq sp-highlight-wrap-overlay nil)
(setq sp-highlight-wrap-tag-overlay nil)
(sp-pair "'" nil :actions :rem)
(sp-pair "\"" nil :actions :rem)

;; enable modes
(exec-path-from-shell-initialize)
(global-evil-leader-mode)
(evil-mode 1)
(which-key-mode)
(ivy-mode 1)
(evil-collection-init)
(counsel-projectile-mode)
(global-flycheck-mode)
(elcord-mode)

;; default emacs configuration
(tool-bar-mode -1)
(scroll-bar-mode -1)
(set-default 'truncate-lines t)
(setq ispell-program-name "/opt/homebrew/bin/ispell")
(setq browse-url-browser-function 'eww-browse-url)

(if mode-line-format
    (progn
      (setq-default header-line-format mode-line-format)
      (setq-default mode-line-format nil)
      (setq mode-line-format nil)
      )
  )

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages '(evil)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
