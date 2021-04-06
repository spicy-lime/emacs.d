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

(straight-use-package 'haskell-mode)

;; ---------------------------------------------------------------------
;; GNU Emacs / N Λ N O - Emacs made simple
;; Copyright (C) 2020 - N Λ N O developers
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses/>.
;; ---------------------------------------------------------------------


;; Path to nano emacs modules (mandatory)
;;(add-to-list 'load-path "/Users/mike/.emacs.d/nano-emacs")
;;(add-to-list 'load-path ".")
;;
;;;; Default layout (optional)
;;(require 'nano-layout)
;;
;;;; Theming Command line options (this will cancel warning messages)
;;(add-to-list 'command-switch-alist '("-dark"   . (lambda (args))))
;;(add-to-list 'command-switch-alist '("-light"  . (lambda (args))))
;;(add-to-list 'command-switch-alist '("-default"  . (lambda (args))))
;;(add-to-list 'command-switch-alist '("-no-splash" . (lambda (args))))
;;(add-to-list 'command-switch-alist '("-no-help" . (lambda (args))))
;;(add-to-list 'command-switch-alist '("-compact" . (lambda (args))))
;;
;;
;;(cond
;; ((member "-default" command-line-args) t)
;; ((member "-dark" command-line-args) (require 'nano-theme-dark))
;; (t (require 'nano-theme-light)))
;;
;;;; Customize support for 'emacs -q' (Optional)
;;;; You can enable customizations by creating the nano-custom.el file
;;;; with e.g. `touch nano-custom.el` in the folder containing this file.
;;(let* ((this-file  (or load-file-name (buffer-file-name)))
;;       (this-dir  (file-name-directory this-file))
;;       (custom-path  (concat this-dir "nano-custom.el")))
;;  (when (and (eq nil user-init-file)
;;             (eq nil custom-file)
;;             (file-exists-p custom-path))
;;    (setq user-init-file this-file)
;;    (setq custom-file custom-path)
;;    (load custom-file)))
;;
;;;; Theme
;;(require 'nano-faces)
;;(nano-faces)
;;
;;(require 'nano-theme)
;;(nano-theme)
;;
;;;; Nano default settings (optional)
;;(require 'nano-defaults)
;;
;;;; Nano session saving (optional)
;;(require 'nano-session)
;;
;;;; Nano header & mode lines (optional)
;;(require 'nano-modeline)
;;
;;;; Nano key bindings modification (optional)
;;(require 'nano-bindings)
;;
;;;; Compact layout (need to be loaded after nano-modeline)
;;(when (member "-compact" command-line-args)
;;  (require 'nano-compact))
;;  
;;;; Nano counsel configuration (optional)
;;;; Needs "counsel" package to be installed (M-x: package-install)
;;;; (require 'nano-counsel)
;;
;;;; Welcome message (optional)
;;(let ((inhibit-message t))
;;  (message "Welcome to GNU Emacs / N Λ N O edition")
;;  (message (format "Initialization time: %s" (emacs-init-time))))
;;
;;;; Splash (optional)
;;(unless (member "-no-splash" command-line-args)
;;  (require 'nano-splash))
;;
;;;; Help (optional)
;;(unless (member "-no-help" command-line-args)
;;  (require 'nano-help))
;;
;;(provide 'nano)

(add-to-list 'default-frame-alist '(font . "Roboto Mono"))
(set-face-attribute 'default t :font "Roboto Mono")
(set-face-attribute 'default nil :font "Roboto Mono")
(set-frame-font "Roboto Mono" nil t)

(setq evil-want-keybinding nil)

;; Setup packages
(require 'evil)
(require 'evil-leader)
(require 'which-key)
(require 'ivy)
(require 'company)
(require 'flycheck)

;; evil

;; evil-leader
(setq evil-leader/in-all-states 1)
(evil-leader/set-leader "SPC")
(evil-leader/set-key "ff" 'find-file)

(evil-leader/set-key "bb" 'buffer-menu)
(evil-leader/set-key "be" 'eval-buffer)

(evil-leader/set-key "ws" 'split-window-below)
(evil-leader/set-key "wv" 'split-window-right)
(evil-leader/set-key "wj" 'windmove-down)
(evil-leader/set-key "wk" 'windmove-up)
(evil-leader/set-key "wh" 'windmove-left)
(evil-leader/set-key "wl" 'windmove-right)
(evil-leader/set-key "wd" 'delete-window)
(evil-leader/set-key "p" 'projectile-command-map)

;; which-key
(which-key-setup-side-window-right)

;; sr-speedbar
(setq speedbar-use-images nil)

;; company
(add-hook 'after-init-hook 'global-company-mode)

;; enable modes
(exec-path-from-shell-initialize)
(global-evil-leader-mode)
(evil-mode 1)
(which-key-mode)
(ivy-mode 1)
(evil-collection-init)
(counsel-projectile-mode)
(global-flycheck-mode)

;; default emacs configuration
(tool-bar-mode -1)
(scroll-bar-mode -1)
(set-default 'truncate-lines t)
(setq ispell-program-name "/opt/homebrew/bin/ispell")

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
