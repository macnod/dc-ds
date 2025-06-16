(asdf:defsystem :dc-ds
  :description "Functions to represent and render nested data structures"
  :author "Donnie Cameron <macnod@gmail.com>"
  :license "MIT License"
  :depends-on (:yason :cl-ppcre)
  :serial t
  :components ((:file "dc-ds-package")
               (:file "dc-ds")))
