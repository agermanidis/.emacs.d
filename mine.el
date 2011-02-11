;; loading modules

(require 'inf-haskell)
(require 'pycomplete)
(require 'ipython)

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
(color-theme-blackboard)                       ; Blackboard theme


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
  (set-window-buffer (next-window) arg-window))

(defun new-class (name inherits)
  (interactive "sName of class: \nsInherits: ")
  (insert (concat "class " name "(" inherits ")"))
  (insert ":\n    def __init__(self):")
  (py-newline-and-indent)
  (insert inherits)
  (insert ".__init__(self)"))

(defun search-buffer (some-string)
  (or (search-forward statement (end-of-buffer) t)
      (search-backward statement (end-of-buffer) t)))

(defun add-import (module)
  (interactive "sImport: ")
  (save-excursion 
    (let ((statement (concat "import " module)))
      (if (not (or (search-forward statement (end-of-buffer) t)
                   (search-backward statement (end-of-buffer) t)))
          (progn
            (beginning-of-buffer)
            (newline-and-indent)
            (previous-line)
            (beginning-of-line)
            (insert statement))
        (message "Already imported")))))

(defun py-main ()
  (interactive)
  (end-of-buffer)
  (insert "\n\n\n\n")
  (insert "if __name__ == '__main__':")
  (add-import "sys")
  (py-newline-and-indent))

(defun pyqt-main ()
  (interactive)
  (py-main)
  (add-import "PyQt4.QtGui")
  (insert "app = PyQt4.QtGui.QApplication(sys.argv)")
  (py-newline-and-indent)
  (insert "app.exec_()"))

(defun py-constructor ()
  (interactive)
  (search-backward "def __init__")
  (py-end-of-def-or-class)
  (previous-line)
  (end-of-line)
  (py-newline-and-indent))

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

(provide 'mine)



