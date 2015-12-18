;; load-path を追加する関数を定義
(defun add-to-load-path (&rest paths)
  (let (path)
    (dolist (path paths paths)
      (let ((default-directory
	      (expand-file-name (concat user-emacs-directory path))))
	(add-to-list 'load-path default-directory)
	(if (fboundp 'normal-top-level-add-subdirs-to-load-path)
	    (normal-top-level-add-subdirs-to-load-path))))))

;; 引数のディレクトリとそのサブディレクトリを load-path に追加
(add-to-load-path "elisp" "conf" "public_repos")
 
;; 引数のディレクトリとそのサブディレクトリを load-path に追加
(add-to-load-path "elisp" "conf" "public_repos")
 
;; パスの設定
(add-to-list 'exec-path "/opt/local/bin")
(add-to-list 'exec-path "/usr/local/bin")

;; 文字コードを指定
(set-language-environment "Japanese")
(prefer-coding-system 'utf-8)

;; ファイル名の扱い Mac OS X
(when (eq system-type 'darwin)
  (require 'ucs-normalize)
  (set-file-name-coding-system 'utf-8-hfs)
  (setq locale-coding-system 'utf-8-hfs))
 
;; キーバインドの割り当て
;; (define-key キーマップ キーバインド 関数のシンボル)
(define-key global-map (kbd "C-m") 'newline-and-indent)
(define-key global-map (kbd "C-c l") 'toggle-truncate-lines)
(define-key global-map (kbd "C-t") 'other-window)
(define-key global-map (kbd "C-,") 'undo)
(define-key global-map (kbd "C-h") 'backward-delete-char)
(when (require 'undo-tree nil t)
  (global-undo-tree-mode t)
  (define-key global-map (kbd "C-.") 'undo-tree-redo))
(when (require 'anything nil t)
;  (define-key global-map (kbd "C-x C-f") 'anything-for-files)
  (define-key global-map (kbd "M-y") 'anything-show-kill-ring))

;; インデントの設定
(setq-default tab-width 4)
(setq-default indent-tabs-mode nil)

;; フレームに関する設定
(column-number-mode t)
(line-number-mode t)
(size-indication-mode 0)
;(setq display-time-day-and-date t)
(setq display-time-24hr-format t)
(display-time-mode t)
(setq frame-title-format "%f")

;; リージョン内の行数と文字数をモードラインに表示する（範囲指定時のみ）
(defun count-lines-and-chars ()
  (if mark-active
      (format "%d lines, %d chars "
	      (count-lines (region-beginning) (region-end))
	      (- (region-end) (region-beginning)) )))
(add-to-list 'default-mode-line-format
	     '(:eval (count-lines-and-chars)))
(set-face-background 'region "darkgreen")

;; 表示テーマ
(when (require 'color-theme nil t)
  (color-theme-initialize)
  (color-theme-billw))
(set-fontset-font
 nil 'japanese-jisx0208
 (font-spec :family "Osaka"))

;; auto-install の設定
(when (require 'auto-install nil t)
  (setq auto-install-directory "~/.emacs.d/elisp/")
  (auto-install-update-emacswiki-package-name t)
  (auto-install-compatibility-setup))

;; anything の設定
;; (auto-install-batch "anything")
(when (require 'anything nil t)
  (setq
   anything-idle-delay 0.3
   anything-input-idle-delay 0.2
   anything-candidate-number-limit 100
   anything-quick-update t
   anything-enable-shortcuts 'alphabet)

  (when (require 'anything-config nil t)
    (setq anything-su-or-sudo "sudo"))

  '(require 'anything-match-plugin nil t)

  (when (and (executable-find "cmigemo")
             (require 'migemo nil t))
    (require 'anything-migemo nil t))

  (when (require 'anything-complete nil t)
    (anything-lisp-complete-symbol-set-timer 150))

  (require 'anything-show-completion nil t)

  (when (require 'auto-install nil t)

    (require 'anything-auto-install nil t))

  (when (require 'descbinds-anything nil t)
    (descbinds-anything-install)))

;; auto-complete の設定
(when (require 'auto-complete-config nil t)
  (add-to-list 'ac-dictionary-directories
               "~/.emacs.d/elisp/ac-dict")
  (define-key ac-mode-map (kbd "M-TAB") 'auto-complete)
  (ac-config-default))



;; python-mode をロードする
(when (autoload 'python-mode "python-mode" "Python editing mode." t)
  ;; python-mode のときのみ python-pep8 のキーバインドを有効にする
  (setq python-mode-hook
  (function (lambda ()
    (local-set-key "\C-c\ p" 'python-pep8))))
  (setq auto-mode-alist (cons '("\\.py$" . python-mode) auto-mode-alist))
  (setq interpreter-mode-alist (cons '("python" . python-mode)
                                     interpreter-mode-alist)))

;; flymake for python
(add-hook 'python-mode-hook 'flymake-find-file-hook)
(when (load "flymake" t)
  (defun flymake-pyflakes-init ()
    (let* ((temp-file (flymake-init-create-temp-buffer-copy
                       'flymake-create-temp-inplace))
           (local-file (file-relative-name
                        temp-file
                        (file-name-directory buffer-file-name))))
  (add-to-list 'flymake-allowed-file-name-masks
               '("\\.py\\'" flymake-pyflakes-init)))
(load-library "flymake-cursor")


;; python-modeの設定
(defcustom python-pep8-command "/usr/local/bin/pep8"
  "PEP8 command."
  :type '(file)
  :group 'python-pep8)

(when (load "python-pep8")
  (define-key global-map (kbd "C-c p") 'python-pep8))

;; 起動時のフレームサイズを設定
(setq initial-frame-alist
	  (append (list
			   '(width . 200)
			   '(height . 64)
			   )
			  initial-frame-alist))
(setq default-frame-alist initial-frame-alist)

;; ツールバーを非表示にする
(tool-bar-mode -1)

;; 起動時にフレームを分割
(add-hook 'after-init-hook  (lambda()
                              (setq inhibit-startup-message t)
                              (setq w (selected-window))
                              (setq w2 (split-window w nil t))
                              (setq w3 (split-window w nil))
                              (setq w (selected-window))
                              (interactive)
                              (eshell)
                              (eshell-interactive-print (concat "cd ~/emacs\n"))
                              (eshell-emit-prompt)
                              ))
