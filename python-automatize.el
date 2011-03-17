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
  (insert "\n\n")
  (insert "logging.basicConfig(level = logging.DEBUG, format ='%(asctime)s %(levelname)s %(message)s')")
  (insert "\n\n")
  (insert "if __name__ == '__main__':")
  (add-import "sys")
  (add-import "logging")
  (py-newline-and-indent))

(defun py-main-with-args (desc)
  (interactive "sDescription")
  (py-main)
  (add-import "argparse")
  (insert "parser = argparse.ArgumentParser(description=")
  (insert (concat desc "')")))

;; (defun py-argparse (&rest arguments)
;;   (interactive (list (read-from-minibuffer "Description: ")
;;                      (loop while (y-or-n-p "Another argument? ")
;;                            collect (list (read-from-minibuffer "Next argument: ")
;;                                          (read-from-minibuffer "Help: ")
;;                                          (read-from-minibuffer "Default: ")))))
;;   (insert "parser = argparse.ArgumentParser(description='" (concat (pop arguments) "')"))
;;   (py-newline-and-indent)
;;   (while arguments
;;     (let ((argument (pop arguments)))
;;       (let ((name (pop argument))
;;             (help (pop argument))
;;             (default (pop argument)))
;;         (message name)
;;         (insert (concat "parser.add_argument("  ))
;;         (insert (concat "name = " name))
;;         (insert (concat "help = " help))
;;         (if (string= default "")
;;             (insert (concat "default=" default)))))))


(defun py-class (name inherits)
  (if (equal inherits "")
      (setq inherits "object"))
  
  (insert (concat "class " name "(" inherits ")"))
  (insert ":\n    def __init__(self):")
  (py-newline-and-indent)
  (if (not (equal inherits "object"))
      (insert inherits ".__init__(self)")))


(defun py-function (name args)
  (insert (concat "def " name "(" args "):"))
  (py-newline-and-indent))

(defun py-class-interactive (name inherits)
  (interactive "sName of class: \nsInherits: ")
  (py-class name inherits))

(defun py-function-interactive (name args)
  (interactive "sName of function:\nArguments: ")
  (py-function name args))

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

(provide 'python-automatize)








