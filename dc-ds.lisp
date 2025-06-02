(in-package :dc-ds)

(defparameter *debug* nil)

(defun collection-p (x)
  "Return T if X is of type collection. See the documentation for
 collection."
  (cond ((atom x) t)
        ((and (listp x)
              (not (zerop (length x)))
              (member (car x) '(:map :list :array)))
         (if (eq (car x) :map) (evenp (length (cdr x))) t))
        (t nil)))

(deftype collection ()
  "A specifier for the type collection. An item of type collection
is either an atom or a list. If it is a list, then the first element
must be the type of the collection, and the type must be one of:
  :list
  :array
  :map

The rest of the list must represent the collection item's
contents. If the collection item is a map, then the list must have
an even number of elements (key/value pairs). Here are some examles of
collections:

  - (:list 1 2 3)
  - (:array 1 2 3)
  - (:map \"a\" 1 \"b\" 2 \"c\" 3)
  - (:list (:map \"a\" 1 \"b\" 2)
           (:map \"c\" 3 \"d\" 4)
           (:map \"e\" 5 \"f\" 6))

A collection item is just an easy way for humans to write a
representation of a nested data structure. The ds command can
turn that representation into an actual data structure that
uses standard Common Lisp types. The human command accepts
a nested data structure and writes it out as a collection
item."
  `(satisfies collection-p))


(declaim (ftype (function (collection) t) ds))
(defun ds (collection)
  "Create a nested data structure.  Each node in LIST-OR-ATOM can be an
atom or a collection. A collection is a map (hash table), an array, or
a list.  Valid collection types are :map, :array, and :list. These
types respresent the corresponding types in Common Lisp. An atom is
represented by any Common Lisp atom. A collection consists of a Common
Lisp list that starts with collection type. In lists and arrays, the
elements that follow the collection type are the elements of the list
or array. In maps, the elements that follow the collection type are
taken in pairs, to represent the key and value pairs of the map.

For example, here's how you would call DS to create the
given data structures:

  A list of integers:

    (ds '(:list 1 2 3))

  A list of strings:

    (ds '(:list \"one\" \"two\" \"three\"))

  An array of integers:

    (ds '(:array 1 2 3))

  A map of strings to integers:

    (ds '(:map \"one\" 1 \"two\" 2 \"three\" 3))

If you want to create a datastructure that consists of an atom, you
can do this:

    (ds 1)

However, if you pass a list instead of an atom, and you don't provide
a type for the list, you get an error. Don't try this:

    (ds '(1 2 3))  ==>  ERROR: Unknown collection type 1

The data structures you pass can be nested to any depth. Here's an
example:

    (ds '(:list 1 2 3 (:list \"one\" \"two\" \"three\")))

Note how you have to specify the type for each collection.

Here's an example of a list of maps:

    (ds '(:array (:map :name \"Donnie\" :age 50 :height \"6'4\" :weight 225)
                 (:map :name \"Tracy\" :age 45 :height \"5'0'\" :weight 120)))

When you create a data structure like the one above, you can use other
ds functions to access and manipulate the data."
  (if (atom collection)
      collection
      (let* ((list (copy-list collection))
             (type (pop list)))
        (case type
          (:map (loop with h = (make-hash-table :test #'equal)
                      while list
                      for key = (pop list)
                      for val = (ds (pop list))
                      do (setf (gethash key h) val)
                      finally (return h)))
          (:array (apply #'vector (mapcar 'ds list)))
          (:list (mapcar #'ds list))))))


(defun valid-keys (keys)
  "Check that the keys are strings, integers, or keywords."
  (loop for key in keys
        for key-type = (type-of key)
        when (not (or (stringp key)
                      (integerp key)
                      (eq key-type 'keyword)))
          do (error "Invalid key type. Must be a string, integer, or keyword.")))


(defun pick (ds &rest keys)
  "Get a node (a leaf or a subtree) of DS, a dc-utilities data
structure.  The parameters that follow ds, collected in KEYS, describe
the path to the node.  For example, given the following data structure
in bogus-ds:

    (ds '(:array (:map :name \"Donnie\" :age 50 :height \"6'4\" :weight 225)
                 (:map :name \"Tracy\" :age 45 :height \"5'0'\" :weight 120)))

You can get Tracy's weight like this:

    (pick bogus-ds 1 :weight)

or like this:

    (pick (elt (remove-if-not (lambda (x) (string= (pick x :name) \"Tracy\"))
                                bogus-ds)
                 0)
            :weight)"
  (valid-keys keys)
  (if keys
      (case (d-type ds)
        (hash-table
         (multiple-value-bind (value exists)
             (gethash (car keys) ds)
           (if exists
               (if (= (length keys) 1)
                   (values value t)
                   (values (apply #'pick (cons value (cdr keys))) t))
               (values nil nil))))
        (sequence
         (if (< (car keys) (length ds))
             (if (= (length keys) 1)
                 (values (elt ds (car keys)) t)
                 (values (apply #'pick (cons (elt ds (car keys))
                                             (cdr keys)))
                         t))
             (values nil nil)))
        (t (values nil nil)))
      (values ds t)))

(defun paths (ds &optional parent-keys)
  "Given a nested data structure DS, this function returns the path
to every leaf.  If you provide a key or list of keys in PARENT-KEYS,
those keys are prepended to the path to every leaf."
  (when (and parent-keys (atom parent-keys))
    (setf parent-keys (list parent-keys)))
  (case (d-type ds)
    (hash-table
     (loop for k being the hash-keys in ds
           for new-parent-keys = (append parent-keys (list k))
           for child-ds = (gethash k ds)
           for child-keys = (paths child-ds new-parent-keys)
           append child-keys))
    (sequence
     (loop for i from 0 below (length ds)
           for new-parent-keys = (append parent-keys (list i))
           for child-ds = (elt ds i)
           append (paths child-ds new-parent-keys)))
    (t (list parent-keys))))

(defun d-type (ds)
  "Given a dc-eclectic data structure DS, this function returns the type
of the data structure.  Valid return values include 'string,
'sequence, 'hash-table, and some Common Lisp types. This function is
used internally, by the clone, pick, paths, human, put,
and to-json functions."
  (let* ((a (type-of ds))
         (b (string-downcase (format nil "~a" a))))
    (cond ((ppcre:scan
            "simple-array character|vector character"
            b)
           'string)
          ((or (string= b "cons")
               (ppcre:scan "vector|array" b))
           'sequence)
          ((atom a) a)
          (t (car a)))))

(defun set-list-element (list index value)
  "Set the element of LIST at INDEX to VALUE. If INDEX is greater than
the last index of LIST, the list is extended with nils before setting
the element."
  (if (< index (length list))
      (setf (elt list index) value)
      (loop while (< (length list) index)
            do (push nil (cdr (last list)))
            finally (push value (cdr (last list)))))
  list)

(defun set-collection-element (sequence-or-hashtable index value)
  "Set the element of SEQUENCE-OR-HASHTABLE at INDEX to VALUE. If
SEQUENCE-OR-HASHTABLE is a list and the index is larger than the last
index of the sequence, then this function extends the list with nil
values before setting the element. If SEQUENCE-OR-HASHTABLE is a
non-list sequence and the index is larger than the last index of the
sequence, then this function signals an error. SEQUENCE-OR-HASHTABLE
can also be a hash table, in which case INDEX becomes the value's key."
  (cond ((listp sequence-or-hashtable)
         (set-list-element sequence-or-hashtable index value))
        ((arrayp sequence-or-hashtable)
         (if (< index (length sequence-or-hashtable))
             (setf (elt sequence-or-hashtable index) value)
             (error "Index out of bounds.")))
        ((hash-table-p sequence-or-hashtable)
         (setf (gethash index sequence-or-hashtable) value))
        (t (error "Invalid data structure."))))

(defun create-sub-ds (keys)
  (if (integerp (second keys))
      (make-list (1+ (second keys)))
      (make-hash-table :test #'equal)))

(defun put (ds location value)
  "In the given dc-utilities data structure DS, this function sets the
value of the node at LOCATION-KEY-PATH, which is a key or an index, or
a list of keys or indexes, to VALUE."
  (let* ((keys (if (atom location)
                   (list location)
                   location))
         (key (car keys)))
    (when *debug* (format t "ds=~a; keys=~a~%" (human ds) keys))
    (valid-keys keys)
    (if (= (length keys) 1)
        (progn
          (when *debug* (format t "one key left"))
          (set-collection-element ds key value))
        (multiple-value-bind (target-ds exists)
            (pick ds key)
          (if exists
              (progn
                (when *debug*
                  (format t "key ~a exists~%" key)
                  (format t "target-ds=~a~%" (human target-ds)))
                (if (or (stringp target-ds) (numberp target-ds) (null target-ds))
                    (let ((sub-ds (if (integerp (second keys))
                                      (make-list (1+ (second keys)))
                                      (make-hash-table :test #'equal))))
                      (set-collection-element ds key sub-ds)
                      (put sub-ds (cdr keys) value))
                    (put target-ds (cdr keys) value)))
              (progn
                (when *debug* (format t "key ~a doesn't exist~%" key))
                (case (d-type ds)
                  (hash-table
                   (setf (gethash key ds) (create-sub-ds keys))
                   (put ds keys value))
                  (sequence
                   (setf (elt ds key) (create-sub-ds keys))
                   (put ds keys value))
                  (otherwise
                   (setf ds (create-sub-ds keys))
                   (put ds keys value)))))))))

;; Needs tests!
(defun unify (ds-base &rest ds-rest)
  "Merges dc-utilities data structures, starting with DS-BASE and then
progressing through the rest of the data structures, collected in
put, in order.  Values in later data structures override values in
earlier data structures when the paths of the values coincide."
  (loop with ds-main = (clone ds-base)
        for ds in ds-rest do
          (loop for key-path in (paths ds)
                do (put ds-main key-path (apply #'pick (cons ds key-path))))
        finally (return ds-main)))

(defun clone (ds)
  "Clone the dc-utilities data structure DS."
  (case (d-type ds)
    (hash-table
     (loop with ds-new = (make-hash-table :test 'equal)
           for key being the hash-keys in ds
           do (setf (gethash key ds-new) (clone (gethash key ds)))
           finally (return ds-new)))
    (string
     (copy-seq ds))
    (sequence
     (if (equal (type-of ds) 'cons)
         (loop
           with ds-new = nil
           for i from 0 below (length ds)
           do (push (clone (elt ds i)) ds-new)
           finally (return (reverse ds-new)))
         (loop
           with l = (length ds)
           with ds-new = (make-array l)
           for i from 0 below l
           do (setf (elt ds-new i) (clone (elt ds i)))
           finally (return ds-new))))
    (t ds)))

(defun human (ds)
  "Render the dc-utilities data structure DS in a human-readable way. For
hash-table portions of the data structure, sort the keys so that the
human-readable version of the hash table can be compared to another.  When
comparing two keys for sorting, the keys are sorted naturally if they are both
of the same type, number, string, or keyword. Otherwise, number keys sort first,
then keyword keys, then string keys. If none of the two keys are a number,
keyword, or string, then the original order is retained."
  (case (d-type ds)
    (hash-table
     (loop with list = (list :map)
           for (k v) in (loop for key being the hash-keys in ds
                          using (hash-value value)
                          collect (list key value) into items
                          finally 
                          (return
                            (sort
                              items
                              ;; If 2 keys are of the same type, sort them.
                              ;; Otherwise, 
                              (lambda (a b) 
                                (cond 
                                  ((and (numberp (car a)) (numberp (car b)))
                                    (< (car a) (car b)))
                                  ((and (stringp (car a)) (stringp (car b)))
                                    (string< (car a)  (car b)))
                                  ((and (keywordp (car a)) (keywordp (car b)))
                                    (string< (car a) (car b)))
                                  ((numberp (car a)) t)
                                  ((keywordp (car a)) t)
                                  ((numberp (car b)) nil)
                                  ((keywordp (car b)) nil)
                                  (t t))))))
           do (push k list)
              (push (human v) list)
           finally (return (nreverse list))))
    (string
     (map 'string 'identity (copy-seq ds)))
    (sequence
     (if (equal (type-of ds) 'cons)
         (loop
           with list = (list :list)
           for a in ds
           do (push (human a) list)
           finally (return (nreverse list)))
         (loop
           with list = (list :array)
           for a across ds
           do (push (human a) list)
           finally (return (nreverse list)))))
    (otherwise ds)))

(defun from-json (json)
  "Creates a dc-utilities data structure from JSON.  This is useful if
you want to easily traverse the JSON data structure."
  (let* ((data (yason:parse json)))
    (ds (if (hash-table-p data)
            (ds data)
            (ds (cons :array data))))))

(defun to-json (ds)
  "Converts the dc-utilities data structure DS into JSON."
  (let ((yason:*symbol-key-encoder* #'yason:encode-symbol-as-lowercase)
        (yason:*symbol-encoder* #'yason:encode-symbol-as-lowercase))
    (with-output-to-string (json)
      (yason:encode ds json))))

(defun from-list (list)
  (cond ((stringp list) list)
        ((vectorp list) (map 'vector #'from-list list))
        ((null list) nil)
        ((atom list) list)
        ((plistp list) (loop with h = (make-hash-table :test 'equal)
                             for key in list by #'cddr
                             for value in (cdr list) by #'cddr
                             for processed-value = (from-list value)
                             do (setf (gethash key h) processed-value)
                             finally (return h)))
        ((listp list) (map 'vector #'from-list list))))

(defun list-to-json (list)
  (to-json (from-list list)))

(defun plistp (list)
  (and (evenp (length list))
       (loop for key in list by #'cddr always (keywordp key))))

(defun d-equal (v1 v2)
  (equal (human (from-list v1))
         (human (from-list v2))))
