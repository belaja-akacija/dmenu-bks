(defsystem "bks"
  :description "bks: open books from dmenu"
  :author "belaja-akacija"
  :depends-on ("cl-ppcre" "cl-fad" "vlime")
  :pathname "src/"
  :serial t
  :components ((:file "utils")
               (:file "config" :depends-on ("utils"))
               (:file "main" :depends-on ("utils" "config")))
  :build-operation "program-op"
  :build-pathname "bks"
  :entry-point "main")
