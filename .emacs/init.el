;; Remove toolbars and menu bars to save space
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

(setq make-backup-files nil)

;; Set the default font size (optional)
(set-face-attribute 'default nil :height 120)  ;; 120 is 12pt, adjust as needed.

;; Hide the startup screen
(setq inhibit-startup-message t)

;; Enable line numbers globally
(global-display-line-numbers-mode t)

;; Disable the toolbar (the graphical icons)
(tool-bar-mode -1)

(setq use-package-always-ensure t)

;; Extending the places Emacs can fetch packages from
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)

;; Initialize the package system
(package-initialize)

;; Bootstrap 'use-package'
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; Tree-sitter and languages
(use-package tree-sitter
  :ensure t
  :config
  (require 'tree-sitter-langs) ; Load the languages
  (global-tree-sitter-mode)    ; Enable Tree-sitter globally
  (add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode)) ; Enable syntax highlighting

(use-package tree-sitter-langs
  :ensure t
  :config
  ;; Manually ensure the major modes are associated with the right tree-sitter languages
  (add-to-list 'tree-sitter-major-mode-language-alist '(rust-mode . rust))
  (add-to-list 'tree-sitter-major-mode-language-alist '(c-mode . c))
  (add-to-list 'tree-sitter-major-mode-language-alist '(c++-mode . cpp))
  (add-to-list 'tree-sitter-major-mode-language-alist '(go-mode . go)))

(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")

(load-theme 'noctilux t)

(if (not (display-graphic-p))
    (set-face-background 'default "color-16"))

(global-display-line-numbers-mode 0)

(setq auto-save-default nil)

;; Install Evil
(unless (package-installed-p 'evil)
  (package-refresh-contents)
  (package-install 'evil))

(require 'evil)
(evil-mode 1) ; Enable Evil mode
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(org-bullets magit key-chord evil tree-sitter-langs rust-mode lsp-mode go-mode)))

(unless (package-installed-p 'key-chord)
  (package-refresh-contents)
  (package-install 'key-chord))

(require 'key-chord)
(key-chord-mode 1)

;;Exit insert mode by pressing j and then k quickly
(key-chord-define evil-insert-state-map  "jk" 'evil-normal-state)


(define-key evil-normal-state-map (kbd "C-p") nil)
(define-key evil-normal-state-map (kbd "C-n") nil)
(define-key evil-normal-state-map (kbd "Esc-x") nil)
(define-key evil-normal-state-map (kbd "L") 'evil-end-of-line)
(define-key evil-normal-state-map (kbd "H") 'evil-beginning-of-line)

(defun move-8-lines-down ()
  "Move the cursor 8 lines downwards."
  (interactive)
  (evil-next-line 8))

(define-key evil-normal-state-map (kbd "J") 'move-8-lines-down)

(defun move-8-lines-up ()
  "Move the cursor 8 lines upwards."
  (interactive)
  (evil-previous-line 8))

(define-key evil-normal-state-map (kbd "K") 'move-8-lines-up)

;; Install Magit if it's not already installed
(unless (package-installed-p 'magit)
  (package-refresh-contents)
  (package-install 'magit))

;; Automatically show Magit status on startup if in a Git repository

(defun open-magit-status-on-startup ()
  (let ((default-directory (locate-dominating-file default-directory ".git")))
    (when default-directory
      (magit-status-setup-buffer default-directory)
      ;; Kill the *scratch* buffer
      (when (get-buffer "*scratch*")
        (kill-buffer "*scratch*"))
      ;; Delete other windows to ensure only Magit status is displayed
      (delete-other-windows))))

(add-hook 'emacs-startup-hook 'open-magit-status-on-startup)

;; split windows vertically by default
(setq split-height-threshold nil)
(setq split-width-threshold 160)

(require 'windmove)

(when (fboundp 'windmove-default-keybindings)
  (windmove-default-keybindings))

(with-eval-after-load 'evil
  (define-key evil-normal-state-map (kbd "C-l") 'windmove-right)
  (define-key evil-normal-state-map (kbd "C-h") 'windmove-left)
  (define-key evil-normal-state-map (kbd "C-j") 'windmove-down)
  (define-key evil-normal-state-map (kbd "C-k") 'windmove-up))

(with-eval-after-load 'org
  (define-key org-mode-map (kbd "C-l") nil)
  (define-key org-mode-map (kbd "C-h") nil)
  (define-key org-mode-map (kbd "C-j") nil)
  (define-key org-mode-map (kbd "C-k") nil))


(with-eval-after-load 'org
  (define-key org-mode-map (kbd "C-l") 'windmove-right)
  (define-key org-mode-map (kbd "C-h") 'windmove-left)
  (define-key org-mode-map (kbd "C-j") 'windmove-down)
  (define-key org-mode-map (kbd "C-k") 'windmove-up))

(with-eval-after-load 'evil-org
  (evil-define-key 'normal evil-org-mode-map (kbd "C-l") 'windmove-right)
  (evil-define-key 'normal evil-org-mode-map (kbd "C-h") 'windmove-left)
  (evil-define-key 'normal evil-org-mode-map (kbd "C-j") 'windmove-down)
  (evil-define-key 'normal evil-org-mode-map (kbd "C-k") 'windmove-up))

(setq org-agenda-files '("~/org/agenda"))

(require 'org)
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)
(setq org-log-done t)

(setq org-clock-persistence-insinuate t)
(org-clock-persistence-insinuate)
(setq org-clock-persist t)
(setq org-clock-in-resume t)

(defun remove-zero-time-clocks ()
  "Remove clock entries with zero duration."
  (org-clock-remove-empty-clock-drawer))

;; Add the hook
(add-hook 'org-clock-out-hook 'remove-zero-time-clocks)

(setq org-clock-out-when-done t)

(setq org-clock-report-include-clocking-task t)

(use-package org-bullets
  :ensure t
  :hook (org-mode . org-bullets-mode))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )


(define-key org-mode-map (kbd "C-c d") 'org-deadline)
(define-key org-mode-map (kbd "C-c s") 'org-schedule)
