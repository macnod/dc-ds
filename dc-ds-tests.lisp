;; Run these tests from the shell, with
;;     make test

(in-package :cl-user)
(require :dc-ds)
(require :prove)
(defpackage :dc-ds-tests (:use :cl :prove :dc-ds))
(in-package :dc-ds-tests)

(plan 99)

(is (ds nil) nil "ds with nil")
(is (ds "") "" "ds with empty string")
(is (ds 1) 1 "ds with integer")
(is (ds 1/2) 1/2 "ds with fraction")
(is (ds "hello") "hello" "ds with string")
(is (ds '(:list 1 2 3)) '(1 2 3) "ds with list of integers")
(ok (equalp (ds '(:array 1 2 3)) (vector 1 2 3)) "ds with array of integers")
(let ((list-of-maps (list (make-hash-table :test 'equal)
                          (make-hash-table :test 'equal)))
      (ds (ds '(:list (:map :a 1 :b 2) (:map :a 3 :b 4)))))
  (setf (gethash :a (first list-of-maps)) 1)
  (setf (gethash :b (first list-of-maps)) 2)
  (setf (gethash :a (second list-of-maps)) 3)
  (setf (gethash :b (second list-of-maps)) 4)
  (let ((s1 (loop for h in list-of-maps
                  collect (loop for k being the hash-keys of h
                                  using (hash-value v)
                                collect (format nil "~a=~a" k v))
                    into kv-pairs
                  finally (return (format nil "~{~a~^, ~}" kv-pairs))))
        (s2 (loop for h in ds
                  collect (loop for k being the hash-keys of h
                                  using (hash-value v)
                                collect (format nil "~a=~a" k v))
                    into kv-pairs
                  finally (return (format nil "~{~a~^, ~}" kv-pairs)))))
    (is s1 s2 "ds with list of maps")))
(let ((ds (ds '(:map :a (:list 1 2 3) :b (:list 4 5 6))))
      (map-of-lists (make-hash-table :test 'equal)))
  (setf (gethash :a map-of-lists) '(1 2 3))
  (setf (gethash :b map-of-lists) '(4 5 6))
  (is (format nil "~a" (ds-list ds))
      (format nil "~a" (ds-list map-of-lists))
      "ds with map of lists"))
(is-error (ds '(:map :a 1 :b)) 'type-error "ds with map")
(is-error (ds (list 1)) 'type-error "ds with list of single integer")
(is-error (ds '(:vector 1 2 3)) 'type-error "ds with vector of integers")

;; ds-list
(is (ds-list (ds nil)) nil "ds-list (ds nil)")
(is (ds-list (ds "")) "" "ds-list (ds empty string)")
(is (ds-list (ds 1)) 1 "ds-list (ds integer)")
(is (ds-list (ds 1/2)) 1/2 "ds-list (ds fraction)")
(is (ds-list (ds "hello")) "hello" "ds-list (ds string)")
(is (ds-list (ds '(:list 1 2 3)))
    '(:list 1 2 3)
    "ds-list (ds list of integers)")
(is (ds-list (ds '(:list (:map :a 1 :b 2) (:map :a 3 :b 4))))
    '(:list (:map :a 1 :b 2) (:map :a 3 :b 4))
    "ds-list (ds list of maps)")
(is (ds-list (ds '(:map :a (:list 1 2 3) :b (:list 4 5 6))))
    '(:map :a (:list 1 2 3) :b (:list 4 5 6))
    "ds-list (ds map of lists)")

;; ds-get
(let ((ds (ds '(:map
                :a (:list 1 2 3)
                :b (:list 4 5 (:array 6 7 8))
                :c "nine"))))
  (is (ds-get ds :c) "nine" "ds-get with :c")
  (is (ds-get ds :a 0) 1 "ds-get with :a 0")
  (is (ds-get ds :a 1) 2 "ds-get with :a 1")
  (is (ds-get ds :a 2) 3 "ds-get with :a 2")
  (is (ds-get ds :a 3) nil "ds-get with :a 3")
  (is (ds-get ds :b 0) 4 "ds-get with :b 0")
  (is (ds-get ds :b 1) 5 "ds-get with :b 1")
  (is (ds-get ds :b 3) nil "ds-get with :b 3")
  (is (ds-get ds :b 2 0) 6 "ds-get with :b 2 0")
  (is (ds-get ds :b 2 1) 7 "ds-get with :b 2 1")
  (is (ds-get ds :b 2 2) 8 "ds-get with :b 2 2")
  (is (ds-get ds :b 2 3) nil "ds-get with :b 2 3"))

;; ds-paths
(let ((ds-1 (ds '(:list
                  (:map :name "Donnie" :age 55 :phone "919-429-1371")
                  (:map :name "Tracy" :age 41 :phone "650-622-1491")
                  (:map :name "Graydon" :age 8 :phone "n/a"))))
      (ds-2 (ds '(:map
                  :donnie (:map :name "Donnie" :age 55 :phone "919-429-1371")
                  :tracy (:map :name "Tracy" :age 41 :phone "650-622-1491")
                  :graydon (:map :name "Graydon" :age 8 :phone "n/a"))))
      (ds-3 (ds '(:list 1 2 (:map :a 1 :b 2 :c (:map :three 3 :four 4 :five
                                                (:list 5 6 7)))))))
  (is (ds-paths ds-1) '((0 :name) (0 :age) (0 :phone)
                        (1 :name) (1 :age) (1 :phone)
                        (2 :name) (2 :age) (2 :phone))
      "ds-paths with list of maps")
  (is (ds-paths ds-2) '((:donnie :name) (:donnie :age) (:donnie :phone)
                        (:tracy :name) (:tracy :age) (:tracy :phone)
                        (:graydon :name) (:graydon :age) (:graydon :phone))
      "ds-paths with map of maps")
  (is (ds-paths (ds '(:list 1 2 3))) '((0) (1) (2)) "ds-paths for '(1 2 3)")
  (is (ds-paths (ds 1)) (list nil) "ds-paths for (ds 1)")
  (is (ds-paths (ds "one")) (list nil) "ds-paths for (ds \"one\")")
  (is (ds-paths nil) (list nil) "ds-paths for (ds nil)")
  (is (ds-paths 1) (list nil) "ds-paths for 1")
  (is (ds-paths "one") (list nil) "ds-paths for \"one\"")
  (is (ds-paths nil) (list nil) "ds-paths for nil")
  (is (mapcar #'car (ds-paths (ds '(:map :one 1 :two 2 :three 3))))
      (list :one :two :three)
      "ds-paths with a simple map")
  (is (ds-paths ds-3)
      '((0) (1) (2 :A) (2 :B) (2 :C :THREE) (2 :C :FOUR)
        (2 :C :FIVE 0) (2 :C :FIVE 1) (2 :C :FIVE 2))
      "ds-paths with unbalanced tree"))

;; ds-type
(is (ds-type (ds '(:map :a 1))) 'hash-table "ds-type hash-table")
(is (ds-type (ds '(:list 1 2 3))) 'sequence "ds-type sequence (list)")
(is (ds-type (ds '(:array 4 5 6))) 'sequence "ds-type sequence (array)")
(is (ds-type (ds '(:map "hello" (:list 1 2 3) "world" (:list 4 5 6))))
    'hash-table "ds-type (hash of lists)")
(is (ds-type (ds '(:list (:map :one 1 :two 2)
                   (:map :one 3 :two 4))))
    'sequence "ds-type (list of hashes)")
(ok (not (member (ds-type (ds 1)) '(hash-table sequence)))
    "ds-type (ds 1) is not a hash-table or a sequence")
(ok (not (member (ds-type (ds "")) '(hash-table sequence)
                 :test 'equal))
    "ds-type (ds \"\") is not a hash-table or a sequence")
(ok (not (member (ds-type 1) '(hash-table sequence)))
    "ds-type 1 is not a hash-table or a sequence")
(is (ds-type 1) 'bit "ds-type 1 is a bit")
(is (ds-type "abc") 'string "ds-type \"abc\" is a string")

;; ds-set
(let ((ds (ds '(:map :a 1 :b 2 :c 3
                :d (:list 4 5 (:map :six 6 :seven 7 "eight" 8
                                    :nine (:list 9 10 11)))))))
  (ds-set ds :a 5)
  (is (ds-get ds :a) 5 "ds-set :a 5")
  (ds-set ds :b 6)
  (is (ds-get ds :b) 6 "ds-set :b 6")
  (ds-set ds :c 7)
  (is (ds-get ds :c) 7 "ds-set :c 7")
  (ds-set ds '(:d 0) 4.5)
  (is (ds-get ds :d 0) 4.5 "ds-set :d 0 4.5")
  (ds-set ds '(:d 2 :seven) 7.5)
  (is (ds-get ds :d 2 :seven) 7.5 "ds-set :d 2 :seven 7.5")
  (ds-set ds '(:d 2 "eight") 8.5)
  (is (ds-get ds :d 2 "eight") 8.5 "ds-set :d 2 \"eight\" 8.5")
  (ds-set ds '(:d 2 :nine 2) 11.5)
  (is (ds-get ds :d 2 :nine 2) 11.5 "ds-set :d 2 :nine 2 11.5")
  (is (ds-list ds)
      (ds-list
       (ds '(:map :a 5 :b 6 :c 7 :d
             (:list 4.5 5
              (:map :six 6
                    :seven 7.5
               "eight" 8.5
               :nine (:list 9 10 11.5))))))
      "ds-set all changes")
  (ds-set ds '(:d 2 :nine 2) (ds '(:map :eleven 11 :twelve 12)))
  (is (ds-get ds :d 2 :nine 2 :eleven) 11 "ds-set :d 2 :nine 2 :eleven 11")
  (is (ds-get ds :d 2 :nine 2 :twelve) 12 "ds-set :d 2 :nine 2 :twelve 12")
  (ds-set ds '(:e :f :g) 1)
  (is (ds-get ds :e :f :g) 1 "ds-set atom non-existing map path 1")
  (ds-set ds '(:h 1) 2)
  (is-error (ds-get ds '(:h 1)) 'simple-error
            "ds-get keys must be integers, strings, or keywords 1")
  (is (ds-get ds :h 1) 2 "ds-set atom non-existing map/list path 2")
  (is-error (ds-set ds '(:i 1 '(:j :k :l) :m 2) 3) 'simple-error
            "ds-set keys must be integers, strings, or keywords 2")
  (ds-set ds '(:l 1 :m 2 :n) 4)
  (is (ds-get ds :l 1 :m 2 :n) 4 "ds-set atom non-existing map/list path 3"))
(let ((ds (ds '(:list 1 1 3))))
  (ds-set ds 1 2)
  (is (ds-get ds 1) 2 "ds-set 1 2")
  (ds-set ds 4 5)
  (is (ds-get ds 4) 5 "ds-set 4 5"))
(let ((ds (ds '(:array 1 1 3))))
  (ds-set ds 1 2)
  (is (ds-get ds 1) 2 "ds-set 1 2")
  (is-error (ds-set ds 3 4) 'simple-error
            "Non-list sequence extension is not supported 1")
  (is-error (ds-set ds 5 6) 'simple-error
            "Non-list sequence extension is not supported 2"))

;; ds-paths
(let ((ds (ds '(:map :a 1 :b 2 :c 3
                :d (:list 4 5 (:map :six 6 :seven 7 "eight" 8
                                    :nine (:list 9 10 11)))))))
  (is (ds-paths ds)
      '((:a) (:b) (:c) (:d 0) (:d 1) (:d 2 :six) (:d 2 :seven)
        (:d 2 "eight") (:d 2 :nine 0) (:d 2 :nine 1) (:d 2 :nine 2))
      "ds-paths with a nested data structure")
  (is (apply #'ds-get (cons ds (elt (ds-paths ds) 0))) 1
      "path #1 leads to 1")
  (is (apply #'ds-get (cons ds (elt (ds-paths ds) 1))) 2
      "path #2 leads to 2")
  (is (apply #'ds-get (cons ds (elt (ds-paths ds) 2))) 3
      "path #3 leads to 3")
  (is (apply #'ds-get (cons ds (elt (ds-paths ds) 3))) 4
      "path #4 leads to 4")
  (is (apply #'ds-get (cons ds (elt (ds-paths ds) 4))) 5
      "path #5 leads to 5")
  (is (apply #'ds-get (cons ds (elt (ds-paths ds) 5))) 6
      "path #6 leads to 6")
  (is (apply #'ds-get (cons ds (elt (ds-paths ds) 6))) 7
      "path #7 leads to 7")
  (is (apply #'ds-get (cons ds (elt (ds-paths ds) 7))) 8
      "path #8 leads to 8")
  (is (apply #'ds-get (cons ds (elt (ds-paths ds) 8))) 9
      "path #9 leads to 9")
  (is (apply #'ds-get (cons ds (elt (ds-paths ds) 9))) 10
      "path #10 leads to 10")
  (is (apply #'ds-get (cons ds (elt (ds-paths ds) 10))) 11
      "path #11 leads to 11"))

;; ds-clone
(let* ((ds (ds '(:map :a 1 :b 2)))
       (ds-1 (ds-clone ds))
       (ds-2 '(:map :a (:map b (:map :c (:list 1 2 3))) :aa 4))
       (ds-3 (ds-clone ds-2)))
  (is (ds-list ds) (ds-list ds-1)
      "ds-clone clone is an exact copy of the original")
  (ds-set ds-1 :a 3)
  (isnt (ds-list ds) (ds-list ds-1)
        "ds-clone original doesn't change when clone changes")
  (is (ds-list ds-2) (ds-list ds-3)
      "ds-clone clone is an exact copy of the deeply-nested original"))

;; ds-merge
(let* ((ds-1 (ds '(:map :a 1 :b 2))))
  (is (ds-list (ds-merge ds-1 (ds '(:map :c 3 :d 4))))
      '(:map :a 1 :b 2 :c 3 :d 4)
      "ds-merge (:a 1 :b 2) (:c 3 :d 4)")
  (is (ds-list (ds-merge ds-1 (ds '(:map :a 3))))
      '(:map :a 3 :b 2)
      "ds-merge (:a 1 :b 2) (:a 3)")
  (is (ds-list (ds-merge ds-1 (ds '(:map :a 3 :b 4))))
      '(:map :a 3 :b 4)
      "ds-merge (:a 1 :b 2) (:a 3 :b 4)")
  (is (ds-list (ds-merge ds-1 (ds '(:map :a 3 :c 5))))
      '(:map :a 3 :b 2 :c 5)
      "ds-merge (:a 1 :b 2) (:a 3 :c 5)")
  (is (ds-list (ds-merge ds-1
                         (ds '(:map :b (:list 1 2 3)))
                         (ds '(:map :b (:list 1 4 3)))))
      '(:map :a 1 :b (:list 1 4 3))
      "ds-merge (:a 1 :b 2) (:b (:list 1 2 3)) (:b (:list 1 4 3))")
  (is (ds-list (ds-merge ds-1 (ds '(:map :b (:list 1 2 3)))))
      '(:map :a 1 :b (:list 1 2 3))
      "ds-merge (:a 1 :b 2) (:b (1 2 3))")
  (is (ds-list (ds-merge ds-1 (ds '(:map :a (:map :aa 11 :ab 12)))))
      '(:map :a (:map :aa 11 :ab 12) :b 2)
      "ds-merge (:a 1 :b 2) (:a (:map :aa 11 :ab 12))"))

;; ds-from-json
(let* ((json-1 "{\"a\":1,\"b\":2,\"c\":[1,2,3],\"d\":{\"e\":4,\"f\":5}}")
       (json-2 (format nil "{\"a\":1,\"b\":2,\"c\":[1,2,~a],\"d\":\"five\"}"
                       "{\"e\":4,\"f\":[\"one\",\"two\",\"three\"]}"))
       (ds-1 (ds-from-json json-1))
       (ds-2 (ds-from-json json-2)))
  (is (ds-list ds-1)
      '(:map "a" 1 "b" 2 "c" (:list 1 2 3) "d" (:map "e" 4 "f" 5))
      "ds-from-json map with ints, a string, a nested list, a nested map")
  (is (ds-to-json ds-1) json-1 "ds-to-json")
  (is (ds-list ds-2)
      '(:map
        "a" 1
        "b" 2
        "c" (:list 1 2 (:map
                        "e" 4
                        "f" (:list "one" "two" "three")))
        "d" "five")
      "ds-from-json with more deeply nested structures")
  (is (ds-get ds-2 "c" 2 "f" 1) "two"
      "ds-get against a deeply-nested ds-to-json structure"))

(finalize)
