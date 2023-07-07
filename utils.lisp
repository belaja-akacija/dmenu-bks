(defun launch-dmenu (lngth file &optional label)
  (string-trim '(#\NewLine #\Space) (uiop:run-program `("dmenu" "-l" ,lngth "-p" ,label)
                     :input file
                     :output :string
                     :ignore-error-status nil)))

(defun launch-dmenu-prompt (prompt)
  (string-trim '(#\NewLine) (uiop:run-program `("dmenu" "-l" "6" "-p" ,prompt) :output :string)))

(defun launch-zathura (path)
  (uiop:run-program `("zathura" ,path)))

(defun launch-sxiv (path)
  (uiop:run-program `("sxiv" "-a" ,path)))

(defun launch-player (terminal player path)
  (uiop:run-program `(,terminal "-e" ,player ,path)))

(defun launch-nvlc (terminal path)
  (uiop:run-program `(,terminal "-e" "nvlc" ,path)))

(defun launch-generic (program path &optional terminal)
  (if (null terminal)
      (uiop:run-program `(,program ,path))
        (uiop:run-program `(,terminal "-e" ,program ,path))))

(defun launch-text (terminal path &optional editor)
  (let ((editor-list '("vim" "nvim"))
        (tmp #P "/tmp/editors-bks"))
    (overwrite-file! tmp (format nil "~{~A~%~}" editor-list :type :human))
    (if (string-equal editor "") ; make sure editor is nil, if editor was passed to the function
        (setf editor nil))
    (cond ((and
             (fad:file-exists-p #P "/usr/bin/vim")
             (fad:file-exists-p #P "/usr/bin/nvim")
             (null editor))
           (uiop:run-program `(,terminal "-e"
                                         ,(launch-dmenu "2" tmp (format nil "(~A) Launch in: " (pathname-name path)))
                                         ,path)))
          ((null editor)
           (uiop:run-program `(,terminal "-e"
                                         ,(launch-dmenu-prompt (format nil "(~A) Which editor do you want to use?"
                                                                       (pathname-name path)))
                                         ,path)
                             :ignore-error-status t))
          (t
           (uiop:run-program `(,terminal "-e" ,editor ,path))))))

(defun overwrite-file! (file removed-lines &key (type :human))
  (with-open-file (in file
                      :direction :output
                      :if-exists :supersede ; overwrite file
                      :if-does-not-exist :create)
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
