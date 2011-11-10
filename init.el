;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; GENERAL
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq inhibit-splash-screen t) ;; no splashscreen
(tool-bar-mode -1)             ;; turn off toolbar 
(menu-bar-mode -1)             ;; turn off menubar
(fset 'yes-or-no-p 'y-or-n-p)  ;; yes/no - y/n
(setq initial-scratch-message  ;; scratch message
      ";; scratch buffer \n")



;; LOAD PATH
(let* ((my-lisp-dir "~/.emacs.d/")
       (default-directory my-lisp-dir))
  (setq load-path (cons my-lisp-dir load-path))
  (normal-top-level-add-subdirs-to-load-path))

;; GLOBAL BINDINGS
(global-set-key (kbd "RET") 'newline-and-indent)
(global-set-key (kbd "M-1") 'delete-other-windows)
(global-set-key (kbd "M-2") 'split-window-horizontally)
(global-set-key (kbd "M-3") 'split-window-vertically)
(global-set-key (kbd "C-c c") 'comment-region)
(global-set-key (kbd "C-c u") 'uncomment-region)

;; Yank and indent
(dolist (command '(yank yank-pop))
  (eval `(defadvice ,command (after indent-region activate)
	   (indent-region (region-beginning) (region-end) nil))))

;; scrolling
(setq 
  scroll-margin 0
  scroll-conservatively 100000
  scroll-up-aggressively 0
  scroll-down-aggressively 0
  scroll-preserve-screen-position t)

;; LINUM
(global-linum-mode 1)
(setq linum-format "%4d ")

;; MODELINE
(line-number-mode t)     ;; show line numbers
(column-number-mode t)   ;; colum numbrs
(size-indication-mode t) ;; file size

;; MINIBUFFER
(setq
  enable-recursive-minibuffers nil
  max-mini-window-height .25      
  minibuffer-scroll-window nil
  resize-mini-windows nil)

(icomplete-mode t)            
(setq 
  icomplete-prospects-height 1
  icomplete-compute-delay 0)
(require 'icomplete+ nil 'noerror)

;; CACHE
;; backups
(setq make-backup-files t ;; make backups
  backup-by-copying t     ;; set directory
  backup-directory-alist '(("." . "~/.emacs.d/backups")) 
  version-control t
  kept-new-versions 2
  kept-old-versions 5
  delete-old-versions t)

;; autosave
(setq auto-save-list-file-prefix
  "~/.emacs.d/cache/auto-save-list/.saves-")

;; recent files
(require 'recentf)    ;; save recently used files
(setq
  recentf-save-file "~/.emacs.d/cache/recentfiles"
  recentf-max-saved-items 100     ;; max save 100
  recentf-max-menu-items 15)      ;; max 15 in menu
(recentf-mode t)                  ;; turn it on

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; PACKAGES 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; ido
(require 'ido)
(ido-mode 'both) ;; buffers and files
(setq
 ido-save-directory-list-file "~/.emacs.d/cache/ido.last"
 ido-ignore-buffers ;; ignore the following buffer names
 '("\\` " "^\*Mess" "\*scra" "^\*Back" ".*Completion" "^\*Ido" "^\*trace"
   "^\*compilation" "^\*GTAGS" "^session\.*" "^\*")
 ido-work-directory-list '("~/" "~/Desktop" "~/Documents" "~src")
 ido-case-fold  t                 ; be case-insensitive
 ido-enable-last-directory-history t ; remember last used dirs
 ido-max-work-directory-list 30   ; should be enough
 ido-max-work-file-list      50   ; remember many
 ido-use-filename-at-point nil    ; don't use filename at point (annoying)
 ido-enable-flex-matching nil     ; don't try to be too smart
 ido-max-prospects 8              ; don't spam my minibuffer
 ido-confirm-unique-completion nil) ; wait for RET, even with unique completion

;; uniquify
(require 'uniquify) ;; makes buffer names unique without <2> type stuff
(setq
 uniquify-buffer-name-style 'post-forward
 uniquify-separator ":"
 uniquify-after-kill-buffer-p t
 uniquify-ignore-buffers-re "^\\*")

;; aspell
(setq ispell-program-name "aspell"
  ispell-extra-args '("--sug-mode=ultra"))

;; set dictionaries and run flyspell on buffer
(global-set-key (kbd "C-c F")
(lambda()(interactive)
  (ispell-change-dictionary "francais")
  (flyspell-buffer)))
(global-set-key (kbd "C-c E")
(lambda()(interactive)
  (ispell-change-dictionary "american")
  (flyspell-buffer)))

;; yassnippet
(require 'yasnippet)
(yas/load-directory "~/.emacs.d/packages/yasnippet-0.6.1c/snippets")
(yas/global-mode 1)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; KEY BINDINGS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; keymap
(defvar vdb-keys-minor-mode-map (make-keymap) "vdb-keys-minor-mode.")

;; define all the mappings here
(define-key vdb-keys-minor-mode-map (kbd "C-c C-c") 'vdb-compile)

;; create the minor mode
(define-minor-mode vdb-keys-minor-mode
  "clean way of mapping my bidings with major modes"
  t " vdb-keys" 'vdb-keys-minor-mode-map)

(vdb-keys-minor-mode t) ;; make it global

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; COMPILATION 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'compile)

;; ;; lets make the compile behave how we want: small horizontal buffer
(defun vdb-compile ()
  (interactive)
  (progn
    (call-interactively 'compile)
    ;; XXX : for some reason the resizing fucks up when used with the vdb-keys-minor-mode
    (setq cur (selected-window))
    (setq w (get-buffer-window "*compilation*"))
    (select-window w)
    (setq h (window-height w))
    (shrink-window (- h 10))
    (select-window cur)
    )
  )

;; make the compile window split vertically
(defadvice compile (around split-vertically activate)
  (let ((split-width-threshold nil)
	(split-height-threshold 0))
    ad-do-it))

;; python
(add-hook 'python-mode-hook
	  (lambda ()
	    (set (make-local-variable 'compile-command)
		 (let ((file (file-name-nondirectory buffer-file-name)))
		   (format "%s %s"
			   (or "python")
			   file)))))

;; java/processing via ant
(add-hook 'java-mode-hook
	  (lambda ()
	    (set (make-local-variable 'compile-command)
		   (format "%s -find" ;; for now just look for the build file in the tree
			   (or "ant")
			   ))))
