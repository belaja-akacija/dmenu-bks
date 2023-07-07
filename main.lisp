;;;; open a pdf in zathura in my bookmarks folder from dmenu

;;; TODO be able to type ".." in dmenu and go back a directory

(defparameter *documents* '("*.PDF" "*.pdf" "*.djvu"))
(defparameter *images* '("*.PNG" "*.png" "*.jpg" "*.jpeg"))
(defparameter *audio* '("*.wav" "*.mp3" "*.ogg"))
(defparameter *books-directory* #P "~/Documents/Books/" )
(defparameter *pictures-directory* #P "~/Pictures/" )
(defparameter *terminal* "st")
(defparameter *player* "nvlc")

(defun check-path (path)
  (directory path))

(defun show-dir (path)
  "Shows the current directory in dmenu"
  (let* ((path-list-raw (cl-fad:list-directory path))
        (path-length (format nil "~s" (length path-list-raw)))
        (path-list (format nil "~{~A~%~}" path-list-raw))
        (tmp #P "/tmp/bks.tmp"))
    (overwrite-file! tmp path-list)
    (if (check-path path)
        (launch-dmenu path-length tmp "Choose file: ")
        (launch-dmenu path-length tmp "Directory not found."))))

(defun follow-path (path cwd)
  "Follows directories and sends paths to (send-file)"
  (cond ((string-equal path "..")
         (follow-path (show-dir (cl-fad:pathname-parent-directory cwd)) (cl-fad:pathname-parent-directory cwd)))
        ((null (cl-fad:directory-pathname-p path))
         (send-file path))
        ((cl-fad:directory-pathname-p path)
         (follow-path (show-dir path) (cl-fad:pathname-parent-directory path)))))

(defun send-file (path)
  "Send the directory path to the program that opens that type of file"
  (let ((curr-path (directory-namestring path)))
    (cond
     ((find path *documents* :test #'pathname-match-p)
      (launch-zathura path)
      (follow-path (show-dir curr-path) (cl-fad:pathname-directory-pathname curr-path)))
     ((find path *images* :test #'pathname-match-p)
      (launch-sxiv path)
      (follow-path (show-dir curr-path) (cl-fad:pathname-directory-pathname curr-path))) ; go back to the current directory you were just in, in dmenu, after closing sxiv
     ((find path *audio* :test #'pathname-match-p)
      (if (cl-fad:file-exists-p (merge-pathnames #P "/usr/bin/" (pathname *player*)))
          (launch-player *terminal* *player* path)
          (launch-player *terminal* "ffplay" path))
      (follow-path (show-dir curr-path) (cl-fad:pathname-directory-pathname curr-path)))
     (t (format t "suitable program not found.")))))

(defun main ()
  (cond
    ((not (null (find (nth 1 sb-ext:*posix-argv*) '("nil" "b" "bks") :test #'string-equal )))
     (follow-path (show-dir *books-directory*) *books-directory*))
     ((not (null (find (nth 1 sb-ext:*posix-argv*) '("i" "img") :test #'string-equal)))
      (follow-path (show-dir *pictures-directory*) *pictures-directory*))))
