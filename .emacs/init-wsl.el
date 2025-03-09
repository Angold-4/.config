;; Declarations to silence warnings
(defvar org-agenda-mode-map)
(defvar magit-mode-map)
(defvar shell-mode-map)
(defvar compilation-mode-map)

(setq make-backup-files nil)
(setq evil-want-keybinding nil)

(tool-bar-mode -1)
(menu-bar-mode -1)

(set-face-attribute 'default nil :height 120)
(setq inhibit-startup-message t)
(global-display-line-numbers-mode t)

(setq use-package-always-ensure t)

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives '("gnu" . "https://elpa.gnu.org/packages/") t)
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(use-package tree-sitter
  :ensure t
  :config
  (require 'tree-sitter-langs)
  (global-tree-sitter-mode)
  (add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode))

(use-package tree-sitter-langs
  :ensure t
  :config
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

(use-package evil
  :ensure t
  :init
  (setq select-enable-clipboard t)
  :config
  (evil-mode 1))

(use-package evil-collection
  :after evil
  :ensure t
  :config
  (evil-collection-init))

(use-package key-chord
  :ensure t
  :config
  (key-chord-mode 1)
  (key-chord-define evil-insert-state-map "jk" 'evil-normal-state))

;; Custom Evil keybindings
(with-eval-after-load 'evil
  (define-key evil-normal-state-map (kbd "C-p") nil)
  (define-key evil-normal-state-map (kbd "C-n") nil)
  (define-key evil-normal-state-map (kbd "Esc-x") nil)
  (define-key evil-normal-state-map (kbd "L") 'evil-end-of-line)
  (define-key evil-normal-state-map (kbd "H") 'evil-beginning-of-line)
  ;; Global windmove bindings as fallback
  (define-key evil-normal-state-map (kbd "C-h") 'windmove-left)
  (define-key evil-normal-state-map (kbd "C-l") 'windmove-right)
  (define-key evil-normal-state-map (kbd "C-j") 'windmove-down)
  (define-key evil-normal-state-map (kbd "C-k") 'windmove-up))

(defun move-8-lines-down ()
  "Move the cursor 8 lines downwards."
  (interactive)
  (evil-next-line 8))

(defun move-8-lines-up ()
  "Move the cursor 8 lines upwards."
  (interactive)
  (evil-previous-line 8))

(with-eval-after-load 'evil
  (define-key evil-normal-state-map (kbd "J") 'move-8-lines-down)
  (define-key evil-normal-state-map (kbd "K") 'move-8-lines-up))

(require 'windmove)
(when (fboundp 'windmove-default-keybindings)
  (windmove-default-keybindings))

;; Function to set windmove keys in Evil normal state
(defun my/set-windmove-keys ()
  "Set windmove keybindings for the current mode in Evil normal state."
  (evil-define-key 'normal (current-local-map) (kbd "C-h") 'windmove-left)
  (evil-define-key 'normal (current-local-map) (kbd "C-l") 'windmove-right)
  (evil-define-key 'normal (current-local-map) (kbd "C-j") 'windmove-down)
  (evil-define-key 'normal (current-local-map) (kbd "C-k") 'windmove-up))

;; Apply windmove keys to specific modes
(add-hook 'org-mode-hook 'my/set-windmove-keys)
(add-hook 'org-agenda-mode-hook 'my/set-windmove-keys)
(add-hook 'magit-mode-hook 'my/set-windmove-keys)
(add-hook 'compilation-mode-hook 'my/set-windmove-keys)
(add-hook 'shell-mode-hook 'my/set-windmove-keys)

(setq org-agenda-files '("~/Posley/orgwork"))
(setq org-agenda-custom-commands
      '(("d" "Daily Agenda" agenda "" ((org-agenda-span 'day)))))

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

(add-hook 'org-clock-out-hook 'remove-zero-time-clocks)
(setq org-clock-out-when-done t)
(setq org-clock-report-include-clocking-task t)

(use-package org-bullets
  :ensure t
  :hook (org-mode . org-bullets-mode))

(define-key org-mode-map (kbd "C-c d") 'org-deadline)
(define-key org-mode-map (kbd "C-c s") 'org-schedule)

(global-set-key (kbd "C-c r") 'rename-buffer)
(setq org-agenda-clockreport-parameter-plist '(:link t :maxlevel 3))

(with-eval-after-load 'evil
  (evil-define-key 'normal org-mode-map (kbd "TAB") #'org-cycle))

(setq ring-bell-function 'ignore)
(setq visible-bell nil)

(require 'solidity-mode)
(define-key solidity-mode-map (kbd "C-c C-g") 'solidity-estimate-gas-at-point)

(setq shell-file-name "/bin/bash")
(setq explicit-shell-file-name "/bin/bash")
(setq explicit-sh-args '("-l"))
(setq comint-process-echoes t)

(add-hook 'shell-mode-hook
  (lambda ()
    (define-key shell-mode-map (kbd "<up>") 'comint-previous-input)
    (define-key shell-mode-map (kbd "<down>") 'comint-next-input)
    (define-key shell-mode-map (kbd "C-r") 'comint-history-isearch-backward)
    (evil-define-key 'insert shell-mode-map (kbd "C-r") 'comint-history-isearch-backward)))

(defun my/copy-to-osx-clipboard (beg end)
  "Copy region to the macOS clipboard using pbcopy."
  (interactive "r")
  (shell-command-on-region beg end "pbcopy")
  (message "Copied to clipboard"))

(define-key evil-visual-state-map (kbd "C-y") 'my/copy-to-osx-clipboard)

(use-package projectile
  :ensure t
  :config
  (projectile-mode +1)
  ;; Existing ripgrep binding
  (global-set-key (kbd "C-c C-f") 'projectile-ripgrep)
  ;; New bindings for core features
  (define-key projectile-mode-map (kbd "C-p") 'projectile-find-file)
  ;; Evil-specific bindings (optional)
  (with-eval-after-load 'evil
    (evil-define-key 'normal projectile-mode-map (kbd "C-p") 'projectile-find-file))
  ;; Enable caching for faster searches
  (setq projectile-enable-caching t))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-agenda-files nil)
 '(package-selected-packages
   '(## rgb rg ripgrep projectile deadgrep typescript-mode pbcopy solidity-mode magit ligature org-bullets key-chord evil tree-sitter-langs rust-mode lsp-mode go-mode evil-collection))
 '(warning-suppress-log-types '((comp))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; (set-face-attribute 'default nil :font "Cascadia Code PL-12.5")

(defun open-corresponding-html-in-browser ()
  "Open the corresponding HTML file in the Windows default browser if the current buffer is an org file."
  (interactive)
  (let ((current-file (buffer-file-name)))
    (if (and current-file
             (string= (file-name-extension current-file) "org"))
        (let* ((html-file (concat (file-name-directory current-file)
                                  (file-name-base current-file)
                                  ".html"))
               (html-file-exists (file-exists-p html-file)))
          (if html-file-exists
              (let ((windows-path (shell-command-to-string
                                   (concat "wslpath -w "
                                           (shell-quote-argument html-file)))))
                ;; Trim the newline from the wslpath output
                (setq windows-path (string-trim windows-path))
                (shell-command (concat "explorer.exe " (shell-quote-argument windows-path))))
            (message "Corresponding HTML file does not exist.")))
      (message "Not in an org file buffer."))))

;; Bind the function to C-x p in org-mode only
(with-eval-after-load 'org
  (define-key org-mode-map (kbd "C-x p") 'open-corresponding-html-in-browser))

(setq package-install-upgrade-built-in t)
