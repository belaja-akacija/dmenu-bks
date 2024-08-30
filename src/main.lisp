;;;; Written by Eliza Oselskyi (belaja-akacija)
;;;; Version: 1.10
;;;; Description: open a pdf in zathura in my bookmarks folder from dmenu (and more!)

;;; TODO: add feature to be able to choose which alternate directory to go to.
;;; But the default should always be the first one in the list, or the first
;;; one that is available.

;; ListOfPaths -> Path
(defun available-path (lop)
"produce the first available path, given a list of paths"
  (cond ((null (car lop))
         #P "")
        ((if (directory (car lop))
             (car lop)
             (available-path (cdr lop))))))

;; ListOfApps -> String
(defun available-program (loa)
  "produce the first available program string to give to the terminal, given a list of programs."
    (cond ((null (car loa))
          '())
         ((if (fad:file-exists-p (merge-pathnames #P "/usr/bin/" (pathname (car loa))))
              (car loa)
              (available-program (cdr loa))))))

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
        (handle-dmenu-error (launch-dmenu "8" tmp (format nil "(~A) Choose file: " path-length))) ; show only 8 files at a time
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

;;; TODO: Not sure if this function should be private or not yet. For now it's private
;; Path -> Void
;(defun go-back-dir (path)
  ;"Go back to the given directory after closing an application, then open it in dmenu"
  ;(follow-path (show-dir path) (fad:pathname-directory-pathname path)))

;;; TODO cleanup this function. Possibly extract out that really nested thing (pls)
;;; Should write a generic driver function that can take in a program and path as an input
(defun send-file (path)
  "Send the directory path to the program that opens that type of file"
  (let ((curr-path (directory-namestring path)))
    (flet ((go-back-dir (path)
             "Go back to the given directory after closing an application, then open it in dmenu"
             (follow-path (show-dir path) (fad:pathname-directory-pathname path)))
           ;; TODO: should this be extracted from the function itself, or stay private?
           (launch-generic-program ()
             "Launch a program with a prompt in dmenu"
             (launch-generic (handle-dmenu-error (launch-dmenu-prompt (format nil "(~A) Which program?" (pathname-name path))))
                             path
                             (if (not (string=
                                        "n"
                                        (string-downcase
                                          (handle-dmenu-error (launch-dmenu-prompt "With terminal?(Y/n)")))))
                                 *terminal* ; if not "n", then send it with the terminal variable in the param list
                                 nil)))) ; still a bit of an abomination, but not as bad now
      (cond
        ((find path *documents* :test #'pathname-match-p)
         (launch-zathura path)
         (go-back-dir curr-path))
        ((find path *images* :test #'pathname-match-p)
         (launch-sxiv path)
         (go-back-dir curr-path)) ; go back to the current directory you were just in, in dmenu, after closing sxiv
        ((find path *audio* :test #'pathname-match-p)
         (launch-player *terminal* (available-program *player*) path)
         (go-back-dir curr-path))
        ((find path *video* :test #'pathname-match-p)
         (launch-player *terminal* (available-program *video-player*) path)
         (go-back-dir curr-path))
        ((find path *text* :test #'pathname-match-p)
         (launch-text *terminal* path *editor*)
         (go-back-dir curr-path))
        (t (launch-generic-program)
           (go-back-dir curr-path))))))

;; ListOfPaths -> Void
;; driver function that abstracts out the needed nesting for the functions to do their thing

(defun bks-driver (lop)
"Follow the first available path and show the directory in dmenu"
  (let ((path (available-path lop)))
    (if (equal path #P "")
        (handle-dmenu-error (launch-dmenu-prompt (format nil "Unavailable path: ~A" path)))
        (follow-path (show-dir path) path))))

(defun main ()
  (progn
    (config-checks *config*)
    (load-config *config*)
    (create-vars-from-config *settings*))
  (cond
    ((find (nth 1 sb-ext:*posix-argv*) '("nil" "b" "bks") :test #'string-equal )
     (bks-driver *books-directory*))
    ((find (nth 1 sb-ext:*posix-argv*) '("i" "img") :test #'string-equal)
     (bks-driver *pictures-directory*))
    ((find (nth 1 sb-ext:*posix-argv*) '("m" "music") :test #'string-equal)
     (bks-driver *music-directory*))
    ((find (nth 1 sb-ext:*posix-argv*) '("v" "vid") :test #'string-equal)
     (bks-driver *video-directory*))
    ((find (nth 1 sb-ext:*posix-argv*) '("h" "hw") :test #'string-equal)
     (bks-driver *homework-directory*))))
