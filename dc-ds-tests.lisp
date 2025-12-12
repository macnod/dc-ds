;; Tests for p-log package

(require :asdf)
(require :fiveam)
(require :uiop)

(push (uiop:getcwd) asdf:*central-registry*)
(ql:register-local-projects)
(asdf:load-system :dc-ds)

(defpackage :dc-ds-tests (:use :cl :fiveam :dc-ds))

(in-package :dc-ds-tests)

(def-suite dc-ds-suite :description "FiveAM tests for the dc-ds package")

(in-suite dc-ds-suite)

(test ds-basic
  (is (equal (ds nil) nil) "ds with nil")
  (is (equal (ds "") "") "ds with empty string")
  (is (equal (ds 1) 1) "ds with integer")
  (is (equal (ds 1/2) 1/2) "ds with fraction")
  (is (equal (ds "hello") "hello") "ds with string")
  (is (equal (ds '(:list 1 2 3)) '(1 2 3)) "ds with list of integers")
  (is (equalp (ds '(:array 1 2 3)) (vector 1 2 3)) "ds with array of integers"))

(test ds-list-of-maps
  (let ((list-of-maps (list (make-hash-table :test 'equal)
                            (make-hash-table :test 'equal)))
        (ds-result (ds '(:list (:map :a 1 :b 2) (:map :a 3 :b 4)))))
    (setf (gethash :a (first list-of-maps)) 1)
    (setf (gethash :b (first list-of-maps)) 2)
    (setf (gethash :a (second list-of-maps)) 3)
    (setf (gethash :b (second list-of-maps)) 4)
    (let ((s1 (format nil "~{~a~^, ~}"
                      (loop for h in list-of-maps
                            collect (loop for k being the hash-keys of h
                                            using (hash-value v)
                                          collect (format nil "~a=~a" k v)))))
          (s2 (format nil "~{~a~^, ~}"
                      (loop for h in ds-result
                            collect (loop for k being the hash-keys of h
                                            using (hash-value v)
                                          collect (format nil "~a=~a" k v))))))
      (is (equal s1 s2)))))

(test ds-map-of-lists
  (let ((ds-result (ds '(:map :a (:list 1 2 3) :b (:list 4 5 6))))
        (map-of-lists (make-hash-table :test 'equal)))
    (setf (gethash :a map-of-lists) '(1 2 3))
    (setf (gethash :b map-of-lists) '(4 5 6))
    (is (equal (format nil "~a" (human ds-result))
               (format nil "~a" (human map-of-lists))))))

(test ds-errors
  (signals type-error (ds '(:map :a 1 :b)) "ds with map")
  (signals type-error (ds (list 1)) "ds with list of single integer")
  (signals type-error (ds '(:vector 1 2 3)) "ds with vector of integers"))

(test human-basic
  "Basic human tests"
  (is (equal (human (ds nil)) nil) "human (ds nil)")
  (is (equal (human (ds "")) "") "human (ds empty string)")
  (is (equal (human (ds 1)) 1) "human (ds integer)")
  (is (equal (human (ds 1/2)) 1/2) "human (ds fraction)")
  (is (equal (human (ds "hello")) "hello") "human (ds string)")
  (is (equal (human (ds '(:list 1 2 3)))
             '(:list 1 2 3))
      "human (ds list of integers)")
  (is (equal (human (ds '(:list (:map :a 1 :b 2) (:map :a 3 :b 4))))
             '(:list (:map :a 1 :b 2) (:map :a 3 :b 4)))
      "human (ds list of maps)")
  (is (equal (human (ds '(:map :a (:list 1 2 3) :b (:list 4 5 6))))
             '(:map :a (:list 1 2 3) :b (:list 4 5 6)))
      "human (ds map of lists)"))

(test pick
  "pick tests"
  (let ((ds (ds '(:map
                  :a (:list 1 2 3)
                  :b (:list 4 5 (:array 6 7 8))
                  :c "nine"))))
    (is (equal (pick ds :c) "nine") "pick with :c")
    (is (equal (pick ds :a 0) 1) "pick with :a 0")
    (is (equal (pick ds :a 1) 2) "pick with :a 1")
    (is (equal (pick ds :a 2) 3) "pick with :a 2")
    (is (equal (pick ds :a 3) nil) "pick with :a 3")
    (is (equal (pick ds :b 0) 4) "pick with :b 0")
    (is (equal (pick ds :b 1) 5) "pick with :b 1")
    (is (equal (pick ds :b 3) nil) "pick with :b 3")
    (is (equal (pick ds :b 2 0) 6) "pick with :b 2 0")
    (is (equal (pick ds :b 2 1) 7) "pick with :b 2 1")
    (is (equal (pick ds :b 2 2) 8) "pick with :b 2 2")
    (is (equal (pick ds :b 2 3) nil) "pick with :b 2 3")))

(test paths
  "paths tests"
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
    (is (equal (paths ds-1)
               '((0 :name) (0 :age) (0 :phone)
                 (1 :name) (1 :age) (1 :phone)
                 (2 :name) (2 :age) (2 :phone)))
        "paths with list of maps")
    (is (equal (paths ds-2)
               '((:donnie :name) (:donnie :age) (:donnie :phone)
                 (:tracy :name) (:tracy :age) (:tracy :phone)
                 (:graydon :name) (:graydon :age) (:graydon :phone)))
        "paths with map of maps")
    (is (equal (paths (ds '(:list 1 2 3))) '((0) (1) (2))) "paths for '(1 2 3)")
    (is (equal (paths (ds 1)) (list nil)) "paths for (ds 1)")
    (is (equal (paths (ds "one")) (list nil)) "paths for (ds \"one\")")
    (is (equal (paths nil) (list nil)) "paths for (ds nil)")
    (is (equal (paths 1) (list nil)) "paths for 1")
    (is (equal (paths "one") (list nil)) "paths for \"one\"")
    (is (equal (paths nil) (list nil)) "paths for nil")
    (is (equal (mapcar #'car (paths (ds '(:map :one 1 :two 2 :three 3))))
               (list :one :two :three))
        "paths with a simple map")
    (is (equal (paths ds-3)
               '((0) (1) (2 :A) (2 :B) (2 :C :THREE) (2 :C :FOUR)
                 (2 :C :FIVE 0) (2 :C :FIVE 1) (2 :C :FIVE 2)))
        "paths with unbalanced tree")))

(test d-type
  "d-type tests"
  (is (equal (d-type (ds '(:map :a 1))) 'hash-table) "d-type hash-table")
  (is (equal (d-type (ds '(:list 1 2 3))) 'sequence) "d-type sequence (list)")
  (is (equal (d-type (ds '(:array 4 5 6))) 'sequence) "d-type sequence (array)")
  (is (equal (d-type (ds '(:map "hello" (:list 1 2 3) "world" (:list 4 5 6))))
             'hash-table) "d-type (hash of lists)")
  (is (equal (d-type (ds '(:list (:map :one 1 :two 2)
                            (:map :one 3 :two 4))))
             'sequence) "d-type (list of hashes)")
  (is (not (member (d-type (ds 1)) '(hash-table sequence))) "d-type (ds 1) is not a hash-table or a sequence")
  (is (not (member (d-type (ds "")) '(hash-table sequence) :test 'equal)) "d-type (ds \"\") is not a hash-table or a sequence")
  (is (not (member (d-type 1) '(hash-table sequence))) "d-type 1 is not a hash-table or a sequence")
  (is (equal (d-type 1) 'bit) "d-type 1 is a bit")
  (is (equal (d-type "abc") 'string) "d-type \"abc\" is a string"))

(test put-map
  "put tests on maps"
  (let ((ds (ds '(:map :a 1 :b 2 :c 3
                  :d (:list 4 5 (:map :six 6 :seven 7 "eight" 8
                                      :nine (:list 9 10 11)))))))
    (put ds :a 5)
    (is (equal (pick ds :a) 5) "put :a 5")
    (put ds :b 6)
    (is (equal (pick ds :b) 6) "put :b 6")
    (put ds :c 7)
    (is (equal (pick ds :c) 7) "put :c 7")
    (put ds '(:d 0) 4.5)
    (is (equal (pick ds :d 0) 4.5) "put :d 0 4.5")
    (put ds '(:d 2 :seven) 7.5)
    (is (equal (pick ds :d 2 :seven) 7.5) "put :d 2 :seven 7.5")
    (put ds '(:d 2 "eight") 8.5)
    (is (equal (pick ds :d 2 "eight") 8.5) "put :d 2 \"eight\" 8.5")
    (put ds '(:d 2 :nine 2) 11.5)
    (is (equal (pick ds :d 2 :nine 2) 11.5) "put :d 2 :nine 2 11.5")
    (is (equal (human ds)
               (human
                (ds '(:map :a 5 :b 6 :c 7 :d
                      (:list 4.5 5
                       (:map :six 6
                             :seven 7.5
                             "eight" 8.5
                             :nine (:list 9 10 11.5)))))))
        "put all changes")
    (put ds '(:d 2 :nine 2) (ds '(:map :eleven 11 :twelve 12)))
    (is (equal (pick ds :d 2 :nine 2 :eleven) 11) "put :d 2 :nine 2 :eleven 11")
    (is (equal (pick ds :d 2 :nine 2 :twelve) 12) "put :d 2 :nine 2 :twelve 12")
    (put ds '(:e :f :g) 1)
    (is (equal (pick ds :e :f :g) 1) "put atom non-existing map path 1")
    (put ds '(:h 1) 2)
    (signals simple-error (pick ds '(:h 1)) "pick keys must be integers, strings, or keywords 1")
    (is (equal (pick ds :h 1) 2) "put atom non-existing map/list path 2")
    (signals simple-error (put ds '(:i 1 '(:j :k :l) :m 2) 3) "put keys must be integers, strings, or keywords 2")
    (put ds '(:l 1 :m 2 :n) 4)
    (is (equal (pick ds :l 1 :m 2 :n) 4) "put atom non-existing map/list path 3")))

(test put-list
  "put tests on lists"
  (let ((ds (ds '(:list 1 1 3))))
    (put ds 1 2)
    (is (equal (pick ds 1) 2) "put 1 2")
    (put ds 4 5)
    (is (equal (pick ds 4) 5) "put 4 5")))

(test put-array
  "put tests on arrays"
  (let ((ds (ds '(:array 1 1 3))))
    (put ds 1 2)
    (is (equal (pick ds 1) 2) "put 1 2")
    (signals simple-error (put ds 3 4) "Non-list sequence extension is not supported 1")
    (signals simple-error (put ds 5 6) "Non-list sequence extension is not supported 2")))

(test paths-nested
  "paths with nested data structure"
  (let ((ds (ds '(:map :a 1 :b 2 :c 3
                  :d (:list 4 5 (:map :six 6 :seven 7 "eight" 8
                                      :nine (:list 9 10 11)))))))
    (is (equal (paths ds)
               '((:a) (:b) (:c) (:d 0) (:d 1) (:d 2 :six) (:d 2 :seven)
                 (:d 2 "eight") (:d 2 :nine 0) (:d 2 :nine 1) (:d 2 :nine 2)))
        "paths with a nested data structure")
    (is (equal (apply #'pick (cons ds (elt (paths ds) 0))) 1) "path #1 leads to 1")
    (is (equal (apply #'pick (cons ds (elt (paths ds) 1))) 2) "path #2 leads to 2")
    (is (equal (apply #'pick (cons ds (elt (paths ds) 2))) 3) "path #3 leads to 3")
    (is (equal (apply #'pick (cons ds (elt (paths ds) 3))) 4) "path #4 leads to 4")
    (is (equal (apply #'pick (cons ds (elt (paths ds) 4))) 5) "path #5 leads to 5")
    (is (equal (apply #'pick (cons ds (elt (paths ds) 5))) 6) "path #6 leads to 6")
    (is (equal (apply #'pick (cons ds (elt (paths ds) 6))) 7) "path #7 leads to 7")
    (is (equal (apply #'pick (cons ds (elt (paths ds) 7))) 8) "path #8 leads to 8")
    (is (equal (apply #'pick (cons ds (elt (paths ds) 8))) 9) "path #9 leads to 9")
    (is (equal (apply #'pick (cons ds (elt (paths ds) 9))) 10) "path #10 leads to 10")
    (is (equal (apply #'pick (cons ds (elt (paths ds) 10))) 11) "path #11 leads to 11")))

(test clone
  "clone tests"
  (let* ((ds (ds '(:map :a 1 :b 2)))
         (ds-1 (clone ds))
         (ds-2 '(:map :a (:map b (:map :c (:list 1 2 3))) :aa 4))
         (ds-3 (clone ds-2)))
    (is (equal (human ds) (human ds-1)) "clone clone is an exact copy of the original")
    (put ds-1 :a 3)
    (is (not (equal (human ds) (human ds-1))) "clone original doesn't change when clone changes")
    (is (equal (human ds-2) (human ds-3)) "clone clone is an exact copy of the deeply-nested original")))

(test unify
  "unify tests"
  (let ((ds-1 (ds '(:map :a 1 :b 2))))
    (is (equal (human (unify ds-1 (ds '(:map :c 3 :d 4))))
               '(:map :a 1 :b 2 :c 3 :d 4))
        "unify (:a 1 :b 2) (:c 3 :d 4)")
    (is (equal (human (unify ds-1 (ds '(:map :a 3))))
               '(:map :a 3 :b 2))
        "unify (:a 1 :b 2) (:a 3)")
    (is (equal (human (unify ds-1 (ds '(:map :a 3 :b 4))))
               '(:map :a 3 :b 4))
        "unify (:a 1 :b 2) (:a 3 :b 4)")
    (is (equal (human (unify ds-1 (ds '(:map :a 3 :c 5))))
               '(:map :a 3 :b 2 :c 5))
        "unify (:a 1 :b 2) (:a 3 :c 5)")
    (is (equal (human (unify ds-1
                             (ds '(:map :b (:list 1 2 3)))
                             (ds '(:map :b (:list 1 4 3)))))
               '(:map :a 1 :b (:list 1 4 3)))
        "unify (:a 1 :b 2) (:b (:list 1 2 3)) (:b (:list 1 4 3))")
    (is (equal (human (unify ds-1 (ds '(:map :b (:list 1 2 3)))))
               '(:map :a 1 :b (:list 1 2 3)))
        "unify (:a 1 :b 2) (:b (1 2 3))")
    (is (equal (human (unify ds-1 (ds '(:map :a (:map :aa 11 :ab 12)))))
               '(:map :a (:map :aa 11 :ab 12) :b 2))
        "unify (:a 1 :b 2) (:a (:map :aa 11 :ab 12))")))

(test json
  "from-json and to-json tests"
  (let* ((json-1 "{\"a\":1,\"b\":2,\"c\":[1,2,3],\"d\":{\"e\":4,\"f\":5}}")
         (json-2 (format nil "{\"a\":1,\"b\":2,\"c\":[1,2,~a],\"d\":\"five\"}"
                         "{\"e\":4,\"f\":[\"one\",\"two\",\"three\"]}"))
         (ds-1 (from-json json-1))
         (ds-2 (from-json json-2)))
    (is (equal (human ds-1)
               '(:map "a" 1 "b" 2 "c" (:list 1 2 3) "d" (:map "e" 4 "f" 5)))
        "from-json map with ints, a string, a nested list, a nested map")
    (is (equal (to-json ds-1) json-1) "roundtrip json-1")
    (is (equal (human ds-2)
               '(:map
                 "a" 1
                 "b" 2
                 "c" (:list 1 2 (:map
                                 "e" 4
                                 "f" (:list "one" "two" "three")))
                 "d" "five"))
        "from-json with more deeply nested structures")
    (is (equal (to-json ds-2) json-2) "rountrip json-2")
    (is (equal (pick ds-2 "c" 2 "f" 1) "two") "pick against a deeply-nested to-json structure")))

(test from-list
  "from-list tests"
  (is (not (from-list nil)) "from-list empty list")
  (is (equal (from-list 1) 1) "from-list 1")
  (is (equal (from-list "a") "a") "from-list \"a\"")
  (is (equal (from-list "abc") "abc") "from-list \"abc\"")
  (is (d-equal (from-list '(1 2 3)) (vector 1 2 3)) "from-list '(1 2 3)")
  (is (d-equal (from-list '(:string "abc" :list (1 2 3) :vector #(4 5 6)))
               (ds '(:map
                     :string "abc"
                     :list (:array 1 2 3) :vector (:array 4 5 6))))
      "from-list (:string \"abc\" :list (1 2 3) :vector #(4 5 6))")
  (is (d-equal (from-list '(:a (:one 1 :two 2 :three (list 4 5 "hello"))))
               (ds '(:map :a (:map :one 1
                                   :two 2
                                   :three (:array list 4 5 "hello")))))
      "from-list (:a (:one 1 :two 2 :three (list 4 5 \"hello\")))"))

;;; Run tests
(unless (run-all-tests)
  (sb-ext:quit :unix-status 1))
