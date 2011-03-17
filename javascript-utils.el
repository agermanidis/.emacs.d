
(defvar libraries '("jquery" "jquery-ui" "underscore"))

(defun add-library (filename)
  (interactive "fChoose filename: ")
  (search-backward "<script>")
  (beginning-of-line)
  (open-line 1)
  (insert
   (concat "<script src=\"" filename "\"></script>")))


(defun sock-init (address) {
  (interactive "sInsert address: ")
  (insert "var sock = WebSocket(")
  (insert address)
  (insert ");")
  (insert "sock.onopen = function(evt) { };")
  (insert "sock.onmessage = function(evt) { };")
  (insert "sock.onerror = function(evt) { };"))


(defun webapp-begin ()
  (map-car 'add-library
           (map-car '(lambda (x) (concat "javascripts/" x ".js"))
                    '("raphael" "jquery" ))))



