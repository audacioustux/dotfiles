(require 'package)

(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))

;; Make sure `use-package' is available.
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; Configure `use-package' prior to loading it.
(eval-and-compile
  (setq use-package-always-ensure nil)
  (setq use-package-always-defer nil)
  (setq use-package-always-demand nil)
  (setq use-package-expand-minimally nil)
  (setq use-package-enable-imenu-support t)
  (setq use-package-compute-statistics t)
  ;; The following is VERY IMPORTANT.  Write hooks using their real name
  ;; instead of a shorter version: after-init ==> `after-init-hook'.
  ;;
  ;; This is to empower help commands with their contextual awareness,
  ;; such as `describe-symbol'.
  (setq use-package-hook-name-suffix nil))

(eval-when-compile
  (require 'use-package))

;; custom file 
(defconst custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file :noerror)
;; --

;; change temp file directory
(setq backup-directory-alist
      `((".*" . ,(concat user-emacs-directory "backups"))))
(setq auto-save-file-name-transforms
      `((".*" ,(concat user-emacs-directory "backups") t)))
;; --

;; remove default UI shits
(use-package emacs
  :init
  (menu-bar-mode -1)
  (tool-bar-mode -1)
  (scroll-bar-mode -1)
  :config
  (setq use-file-dialog nil)
  (setq use-dialog-box t) ; only for mouse events
  (setq inhibit-splash-screen t)
  (setq inhibit-startup-message t)
  :bind (("C-z" . nil)
         ("C-x C-z" . nil)
         ("C-h h" . nil)))
;; --

;; https://200ok.ch/posts/2020-09-29_comprehensive_guide_on_handling_long_lines_in_emacs.html
(use-package emacs
  :config
  (setq-default bidi-paragraph-direction 'left-to-right)
  (setq bidi-inhibit-bpa t))

(use-package so-long
  :config
  (global-so-long-mode 1))
;; --

;; sync external file change
(global-auto-revert-mode t)

;; key-binding for font resize
(defun zoom-in ()
  (interactive)
  (let ((x (+ (face-attribute 'default :height)
              10)))
    (set-face-attribute 'default nil :height x)))

(defun zoom-out ()
  (interactive)
  (let ((x (- (face-attribute 'default :height)
              10)))
    (set-face-attribute 'default nil :height x)))

(define-key global-map (kbd "C-1") 'zoom-in)
(define-key global-map (kbd "C-0") 'zoom-out)
;; --

;; enable narrow to region. try 'disabled, then (C-x n n) for more info
(put 'narrow-to-region 'disabled nil)
;; --

;; remember the cursor position of files when reopening them
(use-package saveplace
  :config
  (setq save-place-file (expand-file-name "saveplace" user-emacs-directory))
  (setq save-place-forget-unreadable-files t)
  (save-place-mode 1))
;; --

(use-package ivy
  :ensure
  :config
  (ivy-mode 1))

