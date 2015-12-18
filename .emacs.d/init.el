;; load-path を追加する関数を定義

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

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

;; load environment value
(load-file (expand-file-name "~/.emacs.d/shellenv.el"))
(dolist (path (reverse (split-string (getenv "PATH") ":")))
  (add-to-list 'exec-path path))

;; パスの設定
;;(add-to-list 'exec-path "/opt/local/bin")
;;(add-to-list 'exec-path "/usr/local/bin")

;; shellからPATHを引き継ぐ
;; (exec-path-from-shell-initialize)

;; PATHの追加
;;(dolist (dir (list
;;              "/sbin"
;;              "/usr/sbin"
;;              "/bin"
;;              "/usr/bin"
;;              "opt/local/bin"
;;              "/sw/bin"
;;              "/usr/local/bin"
;;              (expand-file-name "~/bin")
;;              (expand-file-name "~/.emacs.d/bin")))
  ;; PATHとexec-pathに同じものを追加
;;  (when (and (file-exists-p dir) (not (member dir exec-path)))
;;        (setenv "PATH" (concat dir ":" (getenv "PATH")))
;;        (setq exec-path (append (list dir) exec-path))))

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
(define-key global-map [165] [92]) ;; 165が¥（円マーク） , 92が\（バックスラッシュ）を表す
(define-key global-map (kbd "C-m") 'newline-and-indent)
(define-key global-map (kbd "C-c l") 'toggle-truncate-lines)
(define-key global-map (kbd "C-t") 'other-window)
(define-key global-map (kbd "C-,") 'undo)
(define-key global-map (kbd "C-h") 'backward-delete-char)
(define-key global-map (kbd "C-x ?") 'help-command)
(when (require 'undo-tree nil t)
  (global-undo-tree-mode t)
  (define-key global-map (kbd "C-.") 'undo-tree-redo))
(when (require 'anything nil t)
  (define-key global-map (kbd "C-x C-f") 'anything-for-files)
  (define-key global-map (kbd "M-y") 'anything-show-kill-ring))
(define-key global-map (kbd "C-x x") 'find-file)

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
(require 'linum)
(global-linum-mode t)
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

(defun my-make-scratch (&optional arg)
  (interactive)
  (progn
    ;; *scratch* を作成して buffer-list に放り込む
    (set-buffer (get-buffer-create "*scratch*"))
    (funcall initial-major-mode)
    (erase-buffer)
    (when (and initial-scratch-message (not inhibit-startup-message))
      (insert initial-scratch-message))
    (or arg (progn (setq arg 0)
                   (switch-to-buffer "*scratch*")))
    (cond ((= arg 0) (message "*scratch* is cleared up."))
          ((= arg 1) (message "another *scratch* is created")))))

(add-hook 'kill-buffer-query-functions
          ;; *scratch* バッファで kill-buffer したら内容を消去するだけにする
          (lambda ()
            (if (string= "*scratch*" (buffer-name))
                (progn (my-make-scratch 0) nil)
              t)))

(add-hook 'after-save-hook
          ;; *scratch* バッファの内容を保存したら *scratch* バッファを新しく作る
          (lambda ()
            (unless (member (get-buffer "*scratch*") (buffer-list))
              (my-make-scratch 1))))

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

;; python-pep8
(defcustom python-pep8-command "/usr/local/bin/pep8"
  "PEP8 command."
  :type '(file)
  :group 'python-pep8)
(load "python-pep8")

;; python-mode をロードする
(when (autoload 'python-mode "python-mode" "Python editing mode." t)
  ;; python-mode のときのみ python-pep8 のキーバインドを有効にする
  (setq python-mode-hook
  (function (lambda ()
    (local-set-key "\C-c\ p" 'python-pep8))))
  (setq auto-mode-alist (cons '("\\.py$" . python-mode) auto-mode-alist))
  (setq interpreter-mode-alist (cons '("python" . python-mode)
                                     interpreter-mode-alist)))

(add-to-list 'load-path "~/.emacs.d") ;; check path

;; Ctrl-c c -> exce compile H270526導入　上記flymakeと同ページ
;;;; mode-compile
(autoload 'mode-compile "mode-compile"
  "Command to compile current buffer file based on the major mode" t)
(global-set-key "\C-cc" 'mode-compile)
(autoload 'mode-compile-kill "mode-compile"
  "Command to kill a compilation launched by `mode-compile'" t)
(global-set-key "\C-ck" 'mode-compile-kill)
 
;; 全てバッファを自動的にセーブする
(setq mode-compile-always-save-buffer-p t)
;; コマンドをいちいち確認しない
(setq mode-compile-never-edit-command-p t)
;; メッセージ出力を抑制
(setq mode-compile-expert-p t)
;; メッセージを読み終わるまで待つ時間
(setq mode-compile-reading-time 0)
 
;; コンパイルが完了したらウィンドウを閉じる
(defun compile-autoclose (buffer string)
  (cond ((string-match "finished" string)
         (message "Build maybe successful: closing window.")
         (run-with-timer 0.3 nil
                         'delete-window
                         (get-buffer-window buffer t)))
        (t (message "Compilation exited abnormally: %s" string))))
(setq compilation-finish-functions 'compile-autoclose)


;;flymakeの導入 H270526 url='shnya.jp/blog/?p=477'
;; flymakeパッケージを読み込み
(require 'flymake)
;; 全てのファイルでflymakeを有効化
(add-hook 'find-file-hook 'flymake-find-file-hook)
 
;; miniBufferにエラーを出力
(defun flymake-show-and-sit ()
  "Displays the error/warning for the current line in the minibuffer"
  (interactive)
  (progn
    (let* ((line-no (flymake-current-line-no) )
           (line-err-info-list (nth 0 (flymake-find-err-info flymake-err-info line-no)))
           (count (length line-err-info-list)))
      (while (> count 0)
        (when line-err-info-list
          (let* ((file (flymake-ler-file (nth (1- count) line-err-info-list)))
                 (full-file
                  (flymake-ler-full-file (nth (1- count) line-err-info-list)))
                 (text (flymake-ler-text (nth (1- count) line-err-info-list)))
                 (line (flymake-ler-line (nth (1- count) line-err-info-list))))
            (message "[%s] %s" line text)))
        (setq count (1- count)))))
  (sit-for 60.0))
(global-set-key "\C-ce" 'flymake-show-and-sit)
 
;; flymakeの色を変更
(set-face-background 'flymake-errline "red4")
(set-face-background 'flymake-warnline "dark slate blue")
 
;; Makefile が無くてもC/C++のチェック
(defun flymake-simple-generic-init (cmd &optional opts)
  (let* ((temp-file  (flymake-init-create-temp-buffer-copy
                      'flymake-create-temp-inplace))
         (local-file (file-relative-name
                      temp-file
                      (file-name-directory buffer-file-name))))
    (list cmd (append opts (list local-file)))))
 
(defun flymake-simple-make-or-generic-init (cmd &optional opts)
  (if (file-exists-p "Makefile")
      (flymake-simple-make-init)
    (flymake-simple-generic-init cmd opts)))
 
(defun flymake-c-init ()
  (flymake-simple-make-or-generic-init
   "gcc" '("-Wall" "-Wextra" "-pedantic" "-fsyntax-only")))
 
(defun flymake-cc-init ()
  (flymake-simple-make-or-generic-init
   "g++" '("-Wall" "-Wextra" "-pedantic" "-fsyntax-only")))
 
(push '("\\.c\\'" flymake-c-init) flymake-allowed-file-name-masks)
(push '("\\.\\(cc\\|cpp\\|C\\|CPP\\|hpp\\)\\'" flymake-cc-init)
      flymake-allowed-file-name-masks)

;;(when (load "flymake" t)
;;  (defun flymake-pylint-init ()
;;    (let* ((temp-file (flymake-init-create-temp-buffer-copy
;;                       'flymake-create-temp-inplace))
;;           (local-file (file-relative-name
;;                      temp-file
;;                      (file-name-directory buffer-file-name))))
;;    (list "~/.emacs.d/pyflymake.py" (list local-file))))
;;    ;;     check path
;;
;;(add-to-list 'flymake-allowed-file-name-masks
;;             '("\\.py\\'" flymake-pylint-init)))
;; 
;;(load-library "flymake-cursor")
;; 
;;(add-hook 'find-file-hook 'flymake-find-file-hook)
;; 
;;(defadvice flymake-post-syntax-check (before flymake-force-check-was-interrupted)
;;  (setq flymake-check-was-interrupted t))
;;(ad-activate 'flymake-post-syntax-check)

(defcustom python-pep8-options '("--ignore=E302")
  "Options to pass to pep8.py"
  :type '(repeat string)
  :group 'python-pep8)

;; 許されざるText is read-onlyを回避する
(defadvice eshell-get-old-input (after eshell-read-only-korosu activate)
  (setq ad-return-value (substring-no-properties ad-return-value)))

;; cua-mode の設定
(cua-mode t)
(setq cua-enable-cua-keys nil)

(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))
(when (< emacs-major-version 24)
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))
(package-initialize)

(setq inferior-lisp-program "/opt/local/bin/clisp")

;; sphinx
;; setting of rst.el
;; 2015.12.15

(require 'rst)
(setq auto-mode-alist
      (append '(("\\.rst$" . rst-mode)
                ("\\.rest$" . rst-mode)) auto-mode-alist))
(setq frame-background-mode 'dark)
(add-hook 'rst-mode-hook '(lambda () (setq indent-tabs-mode nil)))
