(defun launch-dmenu (lngth file &optional label)
  (string-trim '(#\NewLine #\Space) (uiop:run-program `("dmenu" "-l" ,lngth "-p" ,label)
                     :input file
                     :output :string
                     :ignore-error-status nil)))

(defun launch-zathura (path)
  (uiop:run-program `("zathura" ,path)))

(defun launch-sxiv (path)
  (uiop:run-program `("sxiv" "-a" ,path)))

(defun overwrite-file! (file removed-lines &key (type :human))
  (with-open-file (in file
                      :direction :output
                      :if-exists :supersede
                      :if-does-not-exist :create) ; overwrite file
    (if (equal type :data)
    (format in "~s" removed-lines)
    (format in "~A" removed-lines))))

(defun show-dialog (dialog &key (justify "left"))
  (let* ((justification (format nil "--justify=~A" justify))
         (dialog-width (length dialog))
         (dialog-height (length (cl-ppcre:split "\\n" dialog)))
         (geometry (format nil "--geometry=~Ax~A+550+300" dialog-width (* 32 dialog-height))))
    (uiop:run-program `("yad" "--text-info" "--wrap" "--margins=20" ,geometry ,justification "--fore=#f2e5bc" "--back=#32302f")
                      :input
                      (uiop:process-info-output
                        (uiop:launch-program `("echo" ,dialog) :output :stream))
                      :output :string)))

(defun get-directory-files (lst)
         (mapcar #'pathname-name lst))

(defun index-of (lst ele i)
  (cond ((null lst) nil)
        ((equal (car lst) ele) i)
        ((index-of (cdr lst) ele (1+ i)))))

(defun get-file-lines (file)
  (let ((lngth 0)) (with-open-file (stream file)
     (loop for line = (read-line stream nil)
           while line
           do (setf lngth (1+ lngth))))
    (format nil "~s" lngth)))
