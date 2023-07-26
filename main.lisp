;;;; open a pdf in zathura in my bookmarks folder from dmenu (and more!)

(defparameter *documents* '("*.PDF" "*.pdf" "*.djvu"))
(defparameter *images* '("*.PNG" "*.png" "*.jpg" "*.jpeg"))
(defparameter *audio* '("*.wav" "*.WAV" "*.MP3" "*.mp3" "*.ogg" "*.OGG"))
(defparameter *text* '("*.txt" "*.TXT" "*.md" "*.MD"))
(defparameter *books-directory* '(#P "~/Documents/Books/") )
(defparameter *pictures-directory* '(#P "~/Pictures/") )
(defparameter *music-directory* '(#P "/media/backup-drive/AUDIO/" #P "~/Documents/Music/"))
(defparameter *homework-directory* '(#P "~/Documents/Personal/CS-degree/homework/") )
(defparameter *terminal* "st")
(defparameter *player* "nvlc")
(defparameter *editor* "nvim")


;; ListOfPaths -> Path
(defun available-path (lop)
"produce the first available path, given a list of paths"
  (cond ((null (car lop))
         #P "nil/")
        ((if (directory (car lop))
             (car lop)
             (available-path (cdr lop))))))

(defun check-path (path)
  (directory path))

(defun show-dir (path)
  "Shows the current directory in dmenu"
  (let* ((path-list-raw (fad:list-directory path))
         (path-length (format nil "~s" (length path-list-raw)))
         (path-list (format nil "~{~A~%~}" path-list-raw))
         (tmp #P "/tmp/bks.tmp"))
    (overwrite-file! tmp path-list)
    (if (check-path path)
        (launch-dmenu "8" tmp (format nil "(~A) Choose file: " path-length)) ; show only 8 files at a time
        (launch-dmenu path-length tmp "Directory not found."))))

(defun follow-path (path cwd)
  "Follows directories and sends paths to (send-file)"
  (let ((parent (fad:pathname-parent-directory cwd)))
    (cond ((or (string-equal path "..") (string-equal path "...") (string-equal path "../")) ; just in case a file name weirdly has multiple dots in it
           (follow-path (show-dir parent) parent))
          ((null (fad:directory-pathname-p path))
           (send-file path))
          ((fad:directory-pathname-p path)
           (follow-path (show-dir path) path)))))

;;; TODO cleanup this function. Possibly extract out that really nested thing (pls)
(defun send-file (path)
  "Send the directory path to the program that opens that type of file"
  (let ((curr-path (directory-namestring path)))
    (cond
      ((find path *documents* :test #'pathname-match-p)
       (launch-zathura path)
       (follow-path (show-dir curr-path) (fad:pathname-directory-pathname curr-path)))
      ((find path *images* :test #'pathname-match-p)
       (launch-sxiv path)
       (follow-path (show-dir curr-path) (fad:pathname-directory-pathname curr-path))) ; go back to the current directory you were just in, in dmenu, after closing sxiv
      ((find path *audio* :test #'pathname-match-p)
       (if (fad:file-exists-p (merge-pathnames #P "/usr/bin/" (pathname *player*)))
           (launch-player *terminal* *player* path)
           (launch-player *terminal* "ffplay" path))
       (follow-path (show-dir curr-path) (fad:pathname-directory-pathname curr-path)))
      ((find path *text* :test #'pathname-match-p)
       (launch-text *terminal* path *editor*)
       (follow-path (show-dir curr-path) (fad:pathname-directory-pathname curr-path)))
      (t (launch-generic (launch-dmenu-prompt (format nil "(~A) Which program?" (pathname-name path)))
                         path
                         (if (not (string=
                                    "n"
                                    (string-downcase
                                      (launch-dmenu-prompt "With terminal?(Y/n)"))))
                             *terminal* ; if not "n", then send it with the terminal variable in the param list
                             nil)) ; still a bit of an abomination, but not as bad now
         (follow-path (show-dir curr-path) (fad:pathname-directory-pathname curr-path))))))

;; ListOfPaths -> Void
;; driver function that abstracts out the needed nesting for the functions to do their thing

(defun bks-driver (lop)
"Follow the first available path and show the directory in dmenu"
  (let ((path (available-path lop)))
    (follow-path (show-dir path) path)))


(defun main ()
  (cond
    ((find (nth 1 sb-ext:*posix-argv*) '("nil" "b" "bks") :test #'string-equal )
     (bks-driver *books-directory*))
    ((find (nth 1 sb-ext:*posix-argv*) '("i" "img") :test #'string-equal)
     (bks-driver *pictures-directory*))
    ((find (nth 1 sb-ext:*posix-argv*) '("m" "music") :test #'string-equal)
     (bks-driver *music-directory*))
    ((find (nth 1 sb-ext:*posix-argv*) '("h" "hw") :test #'string-equal)
     (bks-driver *homework-directory*))))
