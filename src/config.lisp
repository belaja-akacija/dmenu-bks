;; Logic to read a config file into the program

(defparameter *config* (uiop:native-namestring #P "~/.config/bks/bksrc.lisp")) ;; must be a lisp file

(defparameter *allowed-names* '(documents images audio video text music-directory
                                            books-directory pictures-directory video-directory
                                            music-directory homework-directory terminal player
                                            video-player editor))

(defun config-checks (config-file)
  (ensure-directories-exist (uiop:pathname-directory-pathname config-file))
  (if (null (uiop:probe-file* config-file))
      (with-open-file (file config-file
                            :if-does-not-exist :create
                            :if-exists nil
                            :direction :output)
        (format nil "Created file: ~A" config-file)
        (format file "~A" (uiop:read-file-string (uiop:native-namestring #P "~/.local/share/bks/default/bksrc.lisp"))))
      T))

;; trivial, but clear
(defun load-config (file)
  (load file))

(defun create-vars-from-config (alist)
  "Create global variables from the keys in an assoc list"
  (let ((var-list (mapcar (lambda (var) (car var)) alist)))
    (unless (listp var-list)
      (error "Must be a list of symbols"))
    (dolist (var var-list)
      (unless (and (symbolp var) (member var *allowed-names*))
        (error "Variable name ~A not allowed" var))
      (let ((new-var (intern (concatenate 'string "*" (string var) "*")))) ;; interns the filtered name into a symbol again
        (eval `(defparameter ,new-var (cdr (assoc ',var ',alist))))))))
