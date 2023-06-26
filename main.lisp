;;;; open a pdf in zathura in my bookmarks folder from dmenu

(defparameter *documents* '("*.PDF" "*.pdf" "*.djvu"))
(defparameter *images* '("*.png" "*.jpg" "*.jpeg"))

(defun show-dir (path)
  "Shows the current directory in dmenu"
  (let* ((path-list-raw (cl-fad:list-directory path))
        (path-length (format nil "~s" (length path-list-raw)))
        (path-list (format nil "~{~A~%~}" path-list-raw))
        (tmp #P "/tmp/bks.tmp"))
    (overwrite-file! tmp path-list)
    (launch-dmenu path-length tmp "Choose file: ")))

(defun follow-path (path)
  "Follows directories and sends paths to (send-file)"
  (if  (null (cl-fad:directory-pathname-p path))
      (send-file path)
      (follow-path (show-dir path))))

(defun send-file (path)
  "Send the directory path to the program that opens that type of file"
  (let ((curr-path (directory-namestring path)))
    (cond
     ((find path *documents* :test #'pathname-match-p)
      (launch-zathura path))
     ((find path *images* :test #'pathname-match-p)
      (launch-sxiv path)
      (follow-path (show-dir curr-path))) ; go back to the current directory you were just in, in dmenu, after closing sxiv
     (t (format t "suitable program not found.")))))

(defun main ()
  (cond
    ((not (null (find (nth 1 sb-ext:*posix-argv*) '("nil" "b" "bks") :test #'string-equal )))
     (follow-path (show-dir #P "~/Documents/Books/")))
     ((not (null (find (nth 1 sb-ext:*posix-argv*) '("i" "img") :test #'string-equal)))
      (follow-path (show-dir #P "~/Pictures/")))))
