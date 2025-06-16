(defpackage :dc-ds
  (:use :cl)
  (:local-nicknames 
    (:re :ppcre)
    (:json :yason))
  (:export
   ds
   clone
   collection
   from-json
   pick
   human
   unify
   paths
   put
   to-json
   d-type
   list-to-json
   from-list
   d-equal
   ))
