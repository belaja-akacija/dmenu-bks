(defsystem "bks"
  :description "bks: open books from dmenu"
  :author "belaja-akacija"
  :depends-on ("cl-ppcre" "cl-fad" "vlime")
  :components ((:file "utils")
               ;(:file "config" :depends-on ("utils"))
               (:file "main" :depends-on ("utils")))
  :build-operation "program-op"
  :build-pathname "bks"
  :entry-point "main")
