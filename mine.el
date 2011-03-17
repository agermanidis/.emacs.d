;; loading modules

(require 'inf-haskell)
(require 'pycomplete)
(require 'ipython)
(require 'python-automatize)
(require 'smooth-scrolling)

;; settings

(tool-bar-mode nil)                            ; No toolbar
(menu-bar-mode nil)                            ; No menu bar
(set-scroll-bar-mode nil)                      ; No scroll bar
(column-number-mode t)                         ; Show column number in mode-line
(global-hl-line-mode t)                        ; Highlight cursor line
(blink-cursor-mode 0)                          ; No blinking cursor
(icomplete-mode t)                             ; Completion in mini-buffer
(desktop-save-mode t)                          ; Save session before quitting
(cua-mode t)                                   ; Cut/Paste with C-x/C-c/C-
(global-linum-mode 1)                          ; Toggle linum mode
(display-time)                                 ; Display time in modeline
(put 'erase-buffer 'disabled nil)              ; Enable erase-buffer
(color-theme-zenburn)                       ; Blackboard theme
(setq next-line-add-newlines t)


(setq haskell-program-name "/usr/bin/ghci")
(setq auto-mode-alist (cons '("\\.py$" . python-mode) auto-mode-alist))
(setq interpreter-mode-alist(cons '("python" . python-mode)
			     interpreter-mode-alist))
(setq ipython-command "/usr/bin/ipython")
(setq evernote-enml-formatter-command '("w3m" "-dump" "-I" "UTF8" "-O" "UTF8")) ; option

(setq make-backup-files nil)


(autoload 'python-mode "python-mode" "Python editing mode." t)
(autoload 'pymacs-load "pymacs" nil t)
(autoload 'pymacs-eval "pymacs" nil t)
(autoload 'pymacs-apply "pymacs")
(autoload 'pymacs-call "pymacs")

(defun save-macro (name)                  
  "save a macro. Take a name as argument
     and save the last defined macro under 
     this name at the end of your .emacs"
  (interactive "SName of the macro :")  ; ask for the name of the macro    
  (kmacro-name-last-macro name)         ; use this name for the macro    
  (find-file "~/.emacs")                ; open the .emacs file 
  (goto-char (point-max))               ; go to the end of the .emacs
  (newline)                             ; insert a newline
  (insert-kbd-macro name)               ; copy the macro 
  (newline)                             ; insert a newline
  (switch-to-buffer nil))

(defun rename-symbol (new-name)
  (interactive "sNew name: ")
  (replace-string (current-word) new-name))

(defun show-on-the-right (arg-window)
  (interactive)
  (split-window-horizontally)
  (ido-switch-buffer))

(defun search-buffer (some-string)
  (or (search-forward statement (end-of-buffer) t)
      (search-backward statement (end-of-buffer) t)))

(defun mark-line ()
  (interactive)
  (beginning-of-line)
  (mark-end-of-sentence ()))

(defun remember-remember ()
  (interactive)
  (point-to-register 5))

(defun remember-back ()
  (interactive)
  (jump-to-register 5))

(defun end-parens ()
  (interactive)
  (search-forward ")")
  (backward-char))

(defun show-on-browser ()
  (interactive)
  (shell-command
   (concat "google-chrome " (buffer-file-name))))


(defun book-view ()
  (interactive)
  (delete-other-windows)
  (beginning-of-buffer)
  (split-window-horizontally)
  (scroll-down))

(defun turn-page-forward ()
  (interactive)
  (scroll-down)
  (other-window 1)
  (scroll-down)
  (scroll-down)
  (other-window 1))

(defun turn-page-backward ()
  (interactive)
  (scroll-up)
  (other-window 1)
  (scroll-up)
  (scroll-up)
  (other-window 1))

;; setting keys

(global-set-key (kbd "C-RET") 'ipython-complete) 
(global-set-key "\C-c c" 'evernote-create-note)
(global-set-key "\C-c o" 'evernote-open-note)
(global-set-key "\C-c s" 'evernote-search-notes)
(global-set-key "\C-c S" 'evernote-do-saved-search)
(global-set-key "\C-c w" 'evernote-write-note)
(global-set-key "\M-c" 'py-complete)
(global-set-key "\C-x\C-k" 'kill-region)
(global-set-key "\C-w" 'backward-kill-word)
(global-set-key "\C-k" 'kill-whole-line)
(global-set-key "\M-u" 'add-import)
(global-set-key "\M-i" 'imenu)			
(global-set-key (kbd "C-(") 'begin-parens)
(global-set-key (kbd "C-)") 'end-parens)
(global-set-key "\M-r" 'rename-symbol)
(global-set-key "\C-l" 'mark-line)
(global-set-key (kbd "C-'") 'remember-remember)
(global-set-key (kbd "M-'") 'remember-back)
(global-set-key "\M-w" 'show-on-browser)


;; adding hooks

(add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
(add-hook 'haskell-mode-hook' 'turn-on-haskell-indentation)

(add-hook 'before-save-hook
          '(lambda ()
             (or (file-exists-p (file-name-directory buffer-file-name))
                 (make-directory (file-name-directory
				  buffer-file-name) t))))


;; org-mode settings

(setq org-log-done 'time)

(require 'js-comint)
(setq inferior-js-program-command "/usr/bin/java org.mozilla.javascript.tools.shell.Main")
(add-hook 'js2-mode-hook '(lambda () 
			    (local-set-key "\C-x\C-e" 'js-send-last-sexp)
			    (local-set-key "\C-\M-x" 'js-send-last-sexp-and-go)
			    (local-set-key "\C-cb" 'js-send-buffer)
			    (local-set-key "\C-c\C-b" 'js-send-buffer-and-go)
			    (local-set-key "\C-cl" 'js-load-file-and-go)
			    ))





(provide 'mine)






